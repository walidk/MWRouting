package routing

import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import util.Visualizer
import mw._

class PreloadedRoutingGame(network: LatencyNetwork, cFlows: Array[DenseVector[Double]]) extends RoutingGame(network) {
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
	val flows = network.pathFlowsFromStrategies(strategies)
	val pathFlows = for((flow, cFlow) <- flows.zip(cFlows)) yield flow+cFlow
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    NetworkState(pathFlows, pathLatencies)
  }
}

class LLFRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  ncCommodities: Array[Commodity],
  cCommodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val network = new LatencyNetwork(graph, latencyFunctions, ncCommodities)
  private val solver = SocialOptimizer(graph, latencyFunctions, ncCommodities, cCommodities)
  private val optStrategy = solver.optimalStrategy
  private val optCost = solver.optimalCost
  private val llfFlows = solver.llfFlows
  private val game = new PreloadedRoutingGame(network, llfFlows)
  
  val algorithms = new Array[MWAlgorithm](ncCommodities.length)
  
  for(commodityId <- ncCommodities.indices) {
    val commodity = ncCommodities(commodityId)
    val paths = commodity.paths
    val experts: Array[Expert] = 
      for(pathId <- paths.indices.toArray) 
        yield RoutingExpert(commodityId, pathId)
    algorithms(commodityId) = new MWAlgorithm(commodity.epsilon, experts, commodity.updateRule)
  }
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val ncLegend = ncCommodities.map(_.paths.map(pathToString))
  val cLegend = ncCommodities.map(_.paths.map(pathToString(_) + " (compliant)"))
  val legend = ncLegend ++ cLegend
  
  val coordinator = new MWCoordinator[PreloadedRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = coordinator.gameStateStream.map(state => state.pathFlows)
  val cFlows = Stream.continually(llfFlows)
  val ncFlows = flows.map(f=>for((flow, cFlow) <- f.zip(llfFlows)) yield flow-cFlow)
  val bothFlows = ncFlows.zip(cFlows).map({case(a, b) => a++b})
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))
  
  def runFor(T: Int) {
//    println(socialCosts(T))
//    println(llfFlows(0))
    System.out.println(network.toJSON())
    
    Visualizer("Path Flows").plotLineGroups(bothFlows.take(T), "t", "f(t)", legend)
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
//    Visualizer( "Average Latencies").plotLineGroups(avgLatencies.take(T), "t", "Avg latency", legend)
    Visualizer("Social Costs").plotLine(socialCosts.take(T), "t", "social cost")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}