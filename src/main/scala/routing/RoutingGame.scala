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
  
  private def exportToCSV(fileName: String, dataStream: Stream[Array[DenseVector[Double]]], header: Array[String]) {
    import java.io.{File, FileWriter}
    val writer = new FileWriter(fileName)
    writer.write("t,")
    writer.write(header.reduce(_+","+_))
    for((datum, t) <- dataStream.zipWithIndex){
      writer.write("\n")
      writer.write(t+",")
      writer.write(datum.flatMap(_.toArray).map(_.toString).reduce(_+","+_))
    }
    writer.close()
  }
  
  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val game = new RoutingGame(network)
  
  val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
  val legend = commodities.map(_.paths.map(pathToString))
  val coordinator = new MWCoordinator[RoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
  val flows = coordinator.gameStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgStrategyLatencies = coordinator.averageStrategiesStream.map(s => network.pathLatenciesFromPathFlows(network.pathFlowsFromStrategies(s)))
  
  def runFor(T: Int) {
//    network.dumpJSON("graph.json")
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend).exportToPdf("out/flows")
    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "l(t)", legend).exportToPdf("out/latencies")
    Visualizer("Average Latencies").plotLineGroups(avgStrategyLatencies.take(T), "t", "Avg strategy latency", legend).exportToPdf("out/avg_strategy_latencies")
    val v = Visualizer("Strategies").plotStrategies(strategies.take(T))
//    Visualizer("Learning rates").plotLineGroups(learningRates.take(T), "t", "epsilon", legend)
//    .exportToPdf("out/learning_rates")
//    v.exportToPdf("out/strategies")
    
    // export data to a csv file
    exportToCSV("out/flows.csv", flows.take(T), Array("f11", "f12", "f13", "f21", "f22", "f23"))
    exportToCSV("out/latencies.csv", latencies.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
  }
}