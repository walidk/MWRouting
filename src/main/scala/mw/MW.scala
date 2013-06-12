package mw
import breeze.linalg._

class Expert

/** Abstract class that represents Nature, i.e. everything that is external to 
 *  the experts algorithm.
 *  
 *  An extending `class N extends Nature` can interact with `MWAlgorithm`s
 *  through an instance of a `MWCoordinator[N]`. The coordinator will run each
 *  algorithm to get the current strategies, and update the state of nature 
 *  using those strategies. It is recommended that extending classes be 
 *  immutable, and the state of nature be entirely captured in the type State.
 *  
 *  See [[mw.RoutingGame]] for an example of an extending class
 */
abstract class Nature {
  type State
  
  /** @param strategies: the initial strategies of the MWAlgorithms 
   *  interacting with this instance of nature, returns the initial state of 
   *  nature
   *  @return the initial state of this instance  
   */
  def initialState(strategies: Array[DenseVector[Double]]): State
  
  /** @param state: the current state of nature 
   *  @param strategies: the current strategies of the MWAlgorithms
   *  @return the updated state of nature 
   */
  def update(state: State, strategies: Array[DenseVector[Double]]): State
  
  /** @param state: the current state of nature 
   *  @param expert: an `Expert` (action) of on the MWAlgorithms interacting 
   *  with nature
   *  @return the loss of the expert given the state
   */
  def loss(state: State)(expert: Expert): Double
}

/** A class that coordinates a set of MWAlgorithms with an instance of nature.
 *  Provides streams of states, usually of type 
 *  @param nature: an instance of nature, of type N
 *  @param algorithms: an array of MWAlgorithms interacting with nature
 *  @param randomizedStart: Boolean that specifies whether the initial 
 *  strategies should be randomly drawn from the simplex. When set to false, the
 *  algorithms will be initialized with the uniform strategy (i.e. uniform 
 *  distribution over the experts).
 */
class MWCoordinator[N<:Nature](
    val nature: N, 
    algorithms: Array[MWAlgorithm], 
    randomizedStart: Boolean) {
  
  // type alias
  type NatureState = nature.State
  
  private def expertLosses(state: NatureState)(experts: Array[Expert]): DenseVector[Double] = 
    DenseVector(experts map nature.loss(state))
  
  private def algorithmLosses(state: NatureState): Array[DenseVector[Double]] = 
    algorithms map (_.experts) map expertLosses(state) 
    
  /** Captures the global state, which includes
   *  @param time
   *  @param natureState
   *  @param losses: the current losses. losses(i)(j) is the loss of expert j 
   *  for algorithm i
   *  @param stratgies: the current strategies. strategies(i)(j) is the 
   *  probability that algorithm i picks expert j.
   */
  case class GlobalState(
      time: Int, 
      natureState: NatureState, 
      losses: Array[DenseVector[Double]],
      strategies: Array[DenseVector[Double]]) 
  
  /* Main update function. Takes the current global state and returns the 
   * updated state.
   * First updates nature's state
   * Then computes the updated strategies given the current nature state and 
   * losses, by calling method nextStrategy on each algorithm.
   * Then computes the next losses.
   */
  private def next(state: GlobalState): GlobalState = state match {
    case GlobalState(time, natureState, losses, strategies) => {
      val nextNatureState = nature.update(natureState, strategies)
      val nextStrategies = 
        for(id <- algorithms.indices; alg = algorithms(id); strategy = strategies(id); loss=losses(id))
          yield alg.nextStrategy(strategy, loss, alg.epsilon(time))
      val nextLosses = algorithmLosses(nextNatureState)
      GlobalState(time+1, nextNatureState, nextLosses, nextStrategies.toArray)
    }
  }
  
  /* Stream of GlobalStates, starting at the initial state defined by
   * - the initialState method of nature
   * - the initialStrategy method of each algorithm
   * The stream is defined recursively as 
   * `stream = initialState #:: stream.map(next)`
   */
  private val stateStream: Stream[GlobalState] = {
    val strategies = algorithms.map(_.initialStrategy(randomizedStart))
    val natureState = nature.initialState(strategies)
    val losses = algorithmLosses(natureState)
    val state = GlobalState(0, natureState, losses, strategies) 
    state #:: stateStream map next
  }
  
  private def mappedStream[T](f: GlobalState => T): Stream[T] = 
    stateStream map f
  
  /* A helper method that computes, given a stream of elements of type 
   * Array[DenseVector[Double]], the sequence of averages. If we call 
   * x_i the i-th element of stream
   * y_n the n-th element of averages(stream)
   * then y_n = \sum_{i = 1}^n x_i / n
   */
  private def averages(stream: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
    def plus (a: Array[DenseVector[Double]], b: Array[DenseVector[Double]]) =
      (a zip b) map (x=>x._1 + x._2)
    def div (a: Array[DenseVector[Double]], b: Double) =
      a map (_/b)
    def acc(
        s: Stream[Array[DenseVector[Double]]], 
        sumSoFar: Array[DenseVector[Double]], 
        countSoFar: Int): Stream[Array[DenseVector[Double]]] = s match {
      case h#::t => {
        val s = plus(h, sumSoFar)
        div(plus(h, s),countSoFar+1) #:: acc(t, s, countSoFar+1)
      }
    }
    acc(stream.tail, stream.head, 0)
  }
  
  /**
   * 
   */
  val strategiesStream = mappedStream(_.strategies)
  val lossStream = mappedStream(_.losses)
  val averageStrategiesStream = averages(strategiesStream)
  val averageLossStream = averages(lossStream)
  val natureStateStream = mappedStream(_.natureState)
}


/** Abstract class, used to specify which update rule to use in the 
 *  Multiplicative Weights algorithm. To add a new update rule, one should 
 *  extend this class using a case class, and add a match case in the 
 *  MWAlgorithm
 */
abstract class UpdateRule

case class ExponentialUpdate() extends UpdateRule
case class PolyUpdate(exponent: Double) extends UpdateRule
case class FollowTheMeanUpdate() extends UpdateRule

class MWAlgorithm(
    var epsilon: Int => Double, 
    val experts: Array[Expert],
    val updateRule: UpdateRule) {
  
  def initialStrategy(randomizeStart: Boolean) = 
    if(randomizeStart) randomStrategy else uniformStrategy
  
  private def exponentialWeights(strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) = 
    losses.map(loss => math.exp(-epsilon*loss))
  private def polynomialWeight(exponent: Double)(strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) = 
    losses.map(loss => epsilon/math.pow(loss, exponent))
  private def followTheMeanWeight(strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) = {
    val avgLoss = sum(strategy :* losses)
    losses.map(loss => 1+epsilon*(avgLoss - loss))
  }
  
  val nextWeight = updateRule match {
    case ExponentialUpdate() => exponentialWeights _
    case PolyUpdate(exponent) => polynomialWeight (exponent) _
    case FollowTheMeanUpdate() => followTheMeanWeight _
  }
  
  def uniformStrategy = {
    val s = DenseVector(experts.map(_=>1.))
    s/s.norm(1)
  }
  
  def randomStrategy = {
    val rand = new scala.util.Random()
    val s = DenseVector(experts.map(_=>rand.nextDouble))
    s/s.norm(1)
  }
  
  def nextStrategy(
      strategy: DenseVector[Double], 
      losses: DenseVector[Double], 
      epsilon: Double
      ): DenseVector[Double] = {
    val weights = nextWeight(strategy, losses, epsilon)
    val next = strategy :* weights
    next :/ next.norm(1)
  }
}