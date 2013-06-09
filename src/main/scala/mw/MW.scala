package mw
import breeze.linalg._

class Expert

abstract class Nature {
  type State
  def initialState(strategies: Array[DenseVector[Double]]): State
  def update(state: State, strategies: Array[DenseVector[Double]]): State
  def loss(state: State)(expert: Expert): Double
}

class MWCoordinator[N<:Nature](
    val nature: N, 
    algorithms: Array[MWAlgorithm], 
    randomizeStart: Boolean) {
  type NatureState = nature.State
  
  private def expertLosses(state: NatureState)(experts: Array[Expert]): DenseVector[Double] = 
    DenseVector(experts map nature.loss(state))
  
  private def algorithmLosses(state: NatureState): Array[DenseVector[Double]] = 
    algorithms map (_.experts) map expertLosses(state) 
    
  
  case class GlobalState(
      time: Int, 
      natureState: NatureState, 
      losses: Array[DenseVector[Double]],
      strategies: Array[DenseVector[Double]]) 
  
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
  
  private val stateStream: Stream[GlobalState] = {
    val strategies = algorithms.map(_.initialStrategy(randomizeStart))
    val natureState = nature.initialState(strategies)
    val losses = algorithmLosses(natureState)
    val state = GlobalState(0, natureState, losses, strategies) 
    state #:: stateStream map next
  }
  
  private def mappedStream[T](f: GlobalState => T): Stream[T] = 
    stateStream map f
  
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
  
  // The main fields in this class. These are infinite streams, so to use them
  // one should restrict them to a finite range. For example, use 
  // lossStream.take(10) 
  val strategiesStream = mappedStream(_.strategies)
  val lossStream = mappedStream(_.losses)
  val averageStrategiesStream = averages(strategiesStream)
  val averageLossStream = averages(lossStream)
  val natureStateStream = mappedStream(_.natureState)
}


class UpdateRule

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
    
  // Computes the weights. Abstract here, must be implemented by sub classes
  val nextWeight = updateRule match {
    case ExponentialUpdate() => exponentialWeights _
    case PolyUpdate(exponent: Double) => polynomialWeight (exponent) _
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