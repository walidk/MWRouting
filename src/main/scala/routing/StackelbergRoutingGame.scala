package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class StackelbergRoutingGame(network: LatencyNetwork) extends Game {
  case class NetworkState(
      pathFlows: Array[DenseVector[Double]],
      pathLatencies: Array[DenseVector[Double]],
      pathStackelbergCosts: Array[DenseVector[Double]]
      ) extends GameState
  
  type State = NetworkState 
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathStackelbergCosts = network.pathStackelbergCostsFromPathFlows(pathFlows)
    NetworkState(pathFlows, pathLatencies, pathStackelbergCosts)
  }
  
  protected def getStackelbergCost(state: State)(groupId: Int)(pathId: Int) = 
    state.pathStackelbergCosts(groupId)(pathId)
  
  protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)
    case StackelbergRoutingExpert(groupId, pathId) => getStackelbergCost(state)(groupId)(pathId)
  }
}

case class StackelbergRoutingExpert(groupId: Int, pathId: Int) extends Expert

class StackelbergRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  ncCommodities: Array[Commodity],
  cCommodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val network = new LatencyNetwork(graph, latencyFunctions, ncCommodities++cCommodities)
  private val game = new StackelbergRoutingGame(network)
  val cAlgorithms = new Array[MWAlgorithm](cCommodities.length)
  val ncAlgorithms = new Array[MWAlgorithm](ncCommodities.length)
  
  for(commodityId <- cAlgorithms.indices) {
    val commodity = cCommodities(commodityId)
    val epsilon = commodity.epsilon
    val updateRule = commodity.updateRule
    val cExperts: Array[Expert] = 
      for(pathId <- cCommodities(commodityId).paths.indices.toArray) 
        yield StackelbergRoutingExpert(commodityId, pathId)
    cAlgorithms(commodityId) = new MWAlgorithm(epsilon, cExperts, updateRule)
  }
  
  for(commodityId <- ncAlgorithms.indices) {
    val commodity = ncCommodities(commodityId)
    val epsilon = commodity.epsilon
    val updateRule = commodity.updateRule
    val ncExperts: Array[Expert] = 
      for(pathId <- ncCommodities(commodityId).paths.indices.toArray) 
        yield RoutingExpert(commodityId, pathId)
    ncAlgorithms(commodityId) = new MWAlgorithm(epsilon, ncExperts, updateRule)
  }
  
  val coordinator = new MWCoordinator[StackelbergRoutingGame](game, ncAlgorithms++cAlgorithms, randomizedStart)
  
  // LLF
  private val solver = new SocialOptimizer(network)
  val optStrategy = solver.optimalStrategy
  val optCost = solver.optimalCost
  val LLFStrategy = solver.LLFStrategy
  private val LLFGame = new LLFRoutingGame(network, LLFStrategy)
  val LLFcoordinator = new MWCoordinator[LLFRoutingGame](LLFGame, ncAlgorithms, randomizedStart)
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val ncLegend = ncCommodities.map(_.paths.map(pathToString))
  val cLegend = ncCommodities.map(_.paths.map(pathToString(_) + " (compliant)"))
  val legend = ncLegend ++ cLegend
  
  // Streams
  val strategies = coordinator.strategiesStream
  val flows = coordinator.gameStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))
  val LLFstrategies = LLFcoordinator.strategiesStream
  val LLFflows = LLFcoordinator.gameStateStream.map(_.pathFlows)
  val LLFlatencies = LLFcoordinator.lossStream
  val LLFsocialCosts = LLFflows.map(network.socialCostFromPathFlows(_))
  
  
  def runFor(T: Int) {
//    println(optCost)
//    println(socialCosts(T))
//    println(LLFsocialCosts(T))
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
    Visualizer("LLF Path Flows").plotLineGroups(LLFflows.take(T), "t", "f(t)", legend)
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
    Visualizer("LLF Path Losses").plotLineGroups(LLFlatencies.take(T), "t", "l(t)", legend)
//    Visualizer("Average Latencies").plotLineGroups(avgLatencies.take(T), "t", "Avg latency", legend)
    Visualizer("Social Costs")
      .plotLine(socialCosts.take(T), "t", "social cost", "MW optimizer")
      .plotLine(LLFsocialCosts.take(T), "t", "social cost", "LLF")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost", "Social optimum")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
    Visualizer("Strategies (LLF)").plotStrategies(LLFstrategies.take(T))
  }
}