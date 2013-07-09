package routing

import mw._
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import util.Visualizer

class ConstantTollRoutingGame(network: LatencyNetwork, pathTolls: Array[DenseVector[Double]]) extends RoutingGame(network) {
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathLosses = for((tolls, latencies) <- pathTolls.zip(pathLatencies)) yield tolls+latencies
    NetworkState(pathFlows, pathLosses)
  }
}

class OptimalConstantTollRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val optimizer = new SocialOptimizer(network)
  private val optimalStrategy = optimizer.optimalStrategy
  private val optimalTolls = network.pathTollsFromPathFlows(network.pathFlowsFromStrategies(optimalStrategy))
  private val game = new ConstantTollRoutingGame(network, optimalTolls)
  val algorithms = new Array[MWAlgorithm](commodities.length)
  
  for(commodityId <- algorithms.indices) {
    val commodity = commodities(commodityId)
    val epsilon = commodity.epsilon
    val updateRule = commodity.updateRule
    val experts: Array[Expert] = 
      for(pathId <- commodity.paths.indices.toArray) 
        yield RoutingExpert(commodityId, pathId)
    algorithms(commodityId) = new MWAlgorithm(epsilon, experts, updateRule)
  }
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val legend = commodities.map(_.paths.map(pathToString))
  
  val coordinator = new MWCoordinator[ConstantTollRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = coordinator.gameStateStream.map(state => state.pathFlows)
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))
  val losses = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val optCost = optimizer.optimalCost
  
  def runFor(T: Int) {
//    System.out.println(network.toJSON())
    println(optCost)
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
//    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "lat(t)", legend)
    Visualizer("Path Losses (latency + toll)").plotLineGroups(losses.take(T), "t", "loss(t)", legend)
    Visualizer("Social cost").plotLine(socialCosts.take(T), "t", "social cost", "Under tolls")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost", "Social optimum")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}