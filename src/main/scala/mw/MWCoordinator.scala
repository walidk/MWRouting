package mw
import breeze.linalg.DenseVector

/** A class that coordinates a set of MWAlgorithms with an instance of game.
 *  Provides streams of states, usually of type 
 *  @param game: an instance of game, of type G
 *  @param algorithms: an array of MWAlgorithms interacting with game
 *  @param randomizedStart: Boolean that specifies whether the initial 
 *  strategies should be randomly drawn from the simplex. When set to false, the
 *  algorithms will be initialized with the uniform strategy (i.e. uniform 
 *  distribution over the experts).
 */
class MWCoordinator[G<:Game](
    val game: G,
    algorithms: Array[MWAlgorithm],
    randomizedStart: Boolean) {
  
  // type alias
  type GameState = game.State
  
  private def expertLosses(state: GameState)(experts: Array[Expert]): DenseVector[Double] = 
    DenseVector(experts map game.loss(state))
  
  private def algorithmLosses(state: GameState): Array[DenseVector[Double]] =
    for(alg <- algorithms; experts = alg.experts) 
      yield expertLosses(state)(experts)
    
    
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
      gameHistory: List[GameState], 
      losses: Array[DenseVector[Double]],
      strategies: Array[DenseVector[Double]],
      learningRates: Array[Double]) 
  
  /* Main update function. Takes the current global state and returns the 
   * updated state.
   * First updates nature's state
   * Then computes the updated strategies given the current nature state and 
   * losses, by calling method nextStrategy on each algorithm.
   * Then computes the next losses.
   */
  private def next(state: GlobalState): GlobalState = state match {
    case GlobalState(time, natureHistory, losses, strategies, _) => {
      val nextGameState = game.update(natureHistory.head, strategies)
      val learningRates =
        for(alg <- algorithms)
          yield alg.learningRate(natureHistory)(time)
      val nextStrategies = 
        for(id <- algorithms.indices;
          alg = algorithms(id);
          strategy = strategies(id);
          loss = losses(id);
          learningRate = alg.learningRate(natureHistory)(time);
          nextStrategy = alg.nextStrategy(strategy, loss, learningRate))
          yield nextStrategy
      val nextLosses = algorithmLosses(nextGameState)
      GlobalState(time+1, nextGameState::natureHistory, nextLosses, nextStrategies.toArray, learningRates)
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
    val natureState = game.initialState(strategies)
    val natureHistory = List(natureState)
    val losses = algorithmLosses(natureState)
    val learningRates = 
      for(id <- algorithms.indices;
        alg = algorithms(id);
        strategy = strategies(id))
      yield
        alg.learningRate(natureHistory)(0)
    val state = GlobalState(0, natureHistory, losses, strategies, learningRates.toArray)
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
  private def weightedAverages(
      stream: Stream[Array[DenseVector[Double]]], 
      weightStream: Stream[Array[Double]]): Stream[Array[DenseVector[Double]]] = {
    def plus (xs: Array[DenseVector[Double]], ys: Array[DenseVector[Double]]) =
      (xs zip ys) map ({case(x, y) => x+y})
    def mul (xs: Array[DenseVector[Double]], ys: Array[Double]) =
      xs zip ys map ({case(x, y) => x*y})
    def div (xs: Array[DenseVector[Double]], ys: Array[Double]) =
      xs zip ys map ({case(x, y) => x/y})
    def acc(
        s: Stream[(Array[DenseVector[Double]], Array[Double])],
        sCuml: Array[DenseVector[Double]], 
        wCuml: Array[Double]): Stream[Array[DenseVector[Double]]] = s match {
      case (hs, hw)#::t => {
        val s = plus(mul(hs,hw), sCuml)
        val wCumlNew = wCuml zip hw map ({case(x, y) => x+y})
        div(plus(hs, s), wCumlNew) #:: acc(t, s, wCumlNew)
      }
    }
    stream zip weightStream match {
      case (s, w)#::t => acc(t, mul(s, w), w)
    }
  }
  
  private val onesStream: Stream[Array[Double]] = algorithms.map(_=>1.)#::onesStream
  private def averages(stream: Stream[Array[DenseVector[Double]]]) = weightedAverages(stream, onesStream)
  
  /**
   * 
   */
  val strategiesStream = mappedStream(_.strategies)
  val lossStream = mappedStream(_.losses)
  val gameStateStream = mappedStream(_.gameHistory.head)
  val learningRateStream = mappedStream(_.learningRates)
  val regretsStream = lossStream zip strategiesStream map ({case(ls, ss) => {
    val regret = for((l, s) <- ls zip ss; lBar = l.dot(s))
      yield for(pathLoss<- l) yield lBar - pathLoss
    regret
  }})
  val averageRegretsStream = weightedAverages(regretsStream, learningRateStream)
  val averageStrategiesStream = averages(strategiesStream)
  val averageLossStream = averages(lossStream)
  
}
