package routing

import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import util.Visualizer
import mw._

class LLFRoutingGame(network: LatencyNetwork, llfStrategy: Array[DenseVector[Double]]) extends RoutingGame(network) {
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    super.update(state, strategies++llfStrategy)
  }
}

class LLFRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  ncCommodities: Array[Commodity],
  cCommodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val network = new LatencyNetwork(graph, latencyFunctions, ncCommodities++cCommodities)
  private val solver = new SocialOptimizer(network)
  private val optStrategy = solver.optimalStrategy
  private val optCost = solver.optimalCost
  private val llfStrategy: Array[DenseVector[Double]] = solver.computeLLFStrategy()
  private val game = new LLFRoutingGame(network, llfStrategy)
  
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
  
  val coordinator = new MWCoordinator[LLFRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
  val flows = coordinator.gameStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))
  
  def runFor(T: Int) {
//    println(socialCosts(T))
//    println(llfFlows(0))
    System.out.println(network.toJSON())
    
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
    Visualizer("Social Costs").plotLine(socialCosts.take(T), "t", "social cost")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}