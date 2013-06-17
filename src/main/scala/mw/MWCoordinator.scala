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
      strategies: Array[DenseVector[Double]]) 
  
  /* Main update function. Takes the current global state and returns the 
   * updated state.
   * First updates nature's state
   * Then computes the updated strategies given the current nature state and 
   * losses, by calling method nextStrategy on each algorithm.
   * Then computes the next losses.
   */
  private def next(state: GlobalState): GlobalState = state match {
    case GlobalState(time, natureHistory, losses, strategies) => {
      val nextGameState = game.update(natureHistory.head, strategies)
      val nextStrategies = 
        for(id <- algorithms.indices;
          alg = algorithms(id);
          strategy = strategies(id);
          loss = losses(id);
          epsilon = alg.epsilon(natureHistory)(time);
          nextStrategy = alg.nextStrategy(strategy, loss, epsilon))
          yield nextStrategy
      val nextLosses = algorithmLosses(nextGameState)
      GlobalState(time+1, nextGameState::natureHistory, nextLosses, nextStrategies.toArray)
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
    val losses = algorithmLosses(natureState)
    val state = GlobalState(0, List(natureState), losses, strategies) 
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
    def plus (xs: Array[DenseVector[Double]], ys: Array[DenseVector[Double]]) =
      (xs zip ys) map ({case(x, y) => x+y})
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
  val gameStateStream = mappedStream(_.gameHistory.head)
}
