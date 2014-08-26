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
  val regrets = coordinator.regretsStream
  val avgRegrets = coordinator.averageRegretsStream
  val avgStrategyLatencies = coordinator.averageStrategiesStream.map(s => network.pathLatenciesFromPathFlows(network.pathFlowsFromStrategies(s)))
  
  def runFor(T: Int) {
//    network.dumpJSON("graph.json")
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend).exportToPdf("out/flows")
    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "l(t)", legend).exportToPdf("out/latencies")
    Visualizer("Average Latencies").plotLineGroups(avgStrategyLatencies.take(T), "t", "Avg strategy latency", legend).exportToPdf("out/avg_strategy_latencies")
    Visualizer("Discounted Regrets").plotLineGroups(avgRegrets.take(T), "t", "Discounted Regrets", legend).exportToPdf("out/discountedRegrets")
    Visualizer("Instantaneous Regrets").plotLineGroups(regrets.take(T), "t", "Instantaneous Regrets", legend).exportToPdf("out/regrets")
    val v = Visualizer("Strategies").plotStrategies(strategies.take(T))
//    Visualizer("Learning rates").plotLineGroups(learningRates.take(T), "t", "epsilon", legend)
//    .exportToPdf("out/learning_rates")
//    v.exportToPdf("out/strategies")
    
    // export data to a csv file
    exportToCSV("out/flows.csv", flows.take(T), Array("f11", "f12", "f13", "f21", "f22", "f23"))
    exportToCSV("out/latencies.csv", latencies.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
    exportToCSV("out/latavg.csv", avgStrategyLatencies.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
    exportToCSV("out/regrets.csv", regrets.take(T), Array("r11", "r12", "r13", "r21", "r22", "r23"))
    exportToCSV("out/discountedRegrets.csv", avgRegrets.take(T), Array("r11", "r12", "r13", "r21", "r22", "r23"))
  }
  
  // Expected losses
  def expectedRunFor(T: Int, eqFlows: Stream[Array[DenseVector[Double]]]) {
    def sumStreams(ass: Stream[Array[DenseVector[Double]]], bss: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass zip bss map ({case(as, bs) => as zip bs map {case(a, b) => a+b}})
    }
    def dist(ass: Stream[Array[DenseVector[Double]]], bss: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass zip bss map ({
        case(as, bs) => {
          val nor: Double = (as zip bs map {case(a, b) => (a-b).norm(2)}).sum
          Array(DenseVector(nor))
        }})}
    def distance(ass: Stream[Array[DenseVector[Double]]], bss: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass zip bss map ({case(as, bs) => {as zip bs map {case(a, b) => DenseVector((a-b).norm(2))}}})
      }
    
    def scaleStreams(lambda: Double, ass: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass map (as => as map (a => a*lambda))
    }
    
    val nbSim = 150
    val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
    val legend = commodities.map(_.paths.map(pathToString))
    
    var coordinator = new MWCoordinator[RoutingGame](game, algorithms, randomizedStart)
    var flows = coordinator.gameStateStream.map(_.pathFlows)
    var variance = distance(flows, eqFlows)
    var latencies = coordinator.lossStream
    var regrets = coordinator.regretsStream
    
    for(i <- 2 to nbSim) {
      coordinator = new MWCoordinator[RoutingGame](game, algorithms, randomizedStart)
      flows = sumStreams(flows, coordinator.gameStateStream.map(_.pathFlows))
      latencies = sumStreams(latencies, coordinator.lossStream)
      regrets = sumStreams(regrets, coordinator.regretsStream)
      variance = sumStreams(variance, distance(flows, eqFlows))
    }
    
    flows = scaleStreams(1./nbSim, flows)
    latencies = scaleStreams(1./nbSim, latencies)
    regrets = scaleStreams(1./nbSim, regrets)
    variance = scaleStreams(1./nbSim, variance)
    
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend).exportToPdf("out/flows")
    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "l(t)", legend).exportToPdf("out/latencies")
    Visualizer("Instantaneous Regrets").plotLineGroups(regrets.take(T), "t", "Instantaneous Regrets", legend).exportToPdf("out/regrets")
    Visualizer("Variances").plotLineGroups(variance.take(T), "t", "Variances", legend).exportToPdf("out/vars")
    
    exportToCSV("out/expflows.csv", flows.take(T), Array("f11", "f12", "f13", "f21", "f22", "f23"))
    exportToCSV("out/explatencies.csv", latencies.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
    exportToCSV("out/expregrets.csv", regrets.take(T), Array("r11", "r12", "r13", "r21", "r22", "r23"))
  }
  
}