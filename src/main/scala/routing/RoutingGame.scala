package routing

import breeze.linalg._
import util.Visualizer
import mw._
import scala.collection.mutable.HashMap

class RoutingGame(network: LatencyNetwork) extends Game {
  case class NetworkState(
      pathFlows: Array[DenseVector[Double]], 
      pathLatencies: Array[DenseVector[Double]]) extends GameState
  
  type State = NetworkState
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  def update(state: State, strategies: Array[DenseVector[Double]]): State = {
	val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    NetworkState(pathFlows, pathLatencies)
  }
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)
  }
  
  protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
}


case class RoutingExpert(groupId: Int, pathId: Int) extends Expert


class RoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  randomizedStart: Boolean) extends RoutingGameSimBase(graph) {
   
  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val game = new RoutingGame(network)
  
  val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
  val legend = commodities.map(_.paths.map(pathToString))
  val coordinator = new MWCoordinator[RoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
  val flows = coordinator.gameStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream

  
  def runFor(T: Int) {
//    network.dumpJSON("graph.json")
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend).exportToPdf("flows")
    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "l(t)", legend).exportToPdf("latencies")
    Visualizer( "Average Latencies").plotLineGroups(avgLatencies.take(T), "t", "Avg latency", legend).exportToPdf("average_latencies")
    Visualizer("Strategies").plotStrategies(strategies.take(T)) 
  }
}