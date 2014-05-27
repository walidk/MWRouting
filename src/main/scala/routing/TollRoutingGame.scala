package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class TollRoutingGame(network: LatencyNetwork, tollDelay: Int, tollInterval: Int = 1) extends Game {
  case class NetworkState(
      time: Int,
      pathFlows: Array[DenseVector[Double]],
      pathLatencies: Array[DenseVector[Double]],
      pathTolls: Array[DenseVector[Double]],
      futurePathTolls: List[Array[DenseVector[Double]]]
      ) extends GameState
  
  type State = NetworkState 
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    val zeroTolls = strategies.map(s=>DenseVector.zeros[Double](s.size))
    assert(tollDelay >= 0, "tollDelay must be a non-negative integer")
    assert(tollInterval >= 1, "tollInterval must be positive integer")
    val pathTollsSeq = for(i <- 0 to tollDelay) yield zeroTolls
    update(NetworkState(0, null, null, null, pathTollsSeq.toList), strategies)
  }
  
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val time = state.time+1
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathTolls = network.pathTollsFromPathFlows(pathFlows)
    val futurePathTolls = 
      if(time%tollInterval == 0) 
        state.futurePathTolls.tail:::List(pathTolls)
      else
        state.futurePathTolls
    NetworkState(time, pathFlows, pathLatencies, state.futurePathTolls.head, futurePathTolls)
  }
  
  protected def getToll(state: State)(groupId: Int)(pathId: Int) = 
    state.pathTolls(groupId)(pathId)
  
  protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)+getToll(state)(groupId)(pathId)
  }
}

case class TollRoutingExpert(groupId: Int, pathId: Int) extends Expert

class TollRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  tollDelay: Int,
  tollInterval: Int,
  randomizedStart: Boolean) extends RoutingGameSimBase(graph) {

  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val game = new TollRoutingGame(network, tollDelay, tollInterval)
  val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
  
  val legend = commodities.map(_.paths.map(pathToString))
  
  val coordinator = new MWCoordinator[TollRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = coordinator.gameStateStream.map(state => state.pathFlows)
  val tolls = coordinator.gameStateStream.map(state => state.pathTolls)
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))
  val latencies = coordinator.gameStateStream.map(_.pathLatencies)
  val losses = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val solver = new SocialOptimizer(network)
  val optStrategy = solver.optimalStrategy
  val optCost = solver.optimalCost
  
  def runFor(T: Int) {
//    System.out.println(network.toJSON())
    println(optCost)
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
      .exportToPdf("out/tolling/pathFlows")
//    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "lat(t)", legend)
    Visualizer("Path tolls").plotLineGroups(tolls.take(T), "t", "toll(t)", legend)
      .exportToPdf("out/tolling/pathTolls")
    Visualizer("Path Losses (latency + toll)").plotLineGroups(losses.take(T), "t", "loss(t)", legend)
      .exportToPdf("out/tolling/latencyAndToll")
    Visualizer("Social cost").plotLine(socialCosts.take(T), "t", "social cost")
      .plotDashedLine(Stream.continually(optCost).take(T), "t", "social cost")
      .exportToPdf("out/tolling/socialCost")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
      .exportToPdf("out/tolling/strategies")
  }
}