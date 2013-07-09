package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class NoRegretSocialRoutingGame(network: LatencyNetwork) extends Game {
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
    case NoRegretSocialRoutingExpert(groupId, pathId) => getStackelbergCost(state)(groupId)(pathId)
  }
}

case class NoRegretSocialRoutingExpert(groupId: Int, pathId: Int) extends Expert

class NoRegretSocialRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  ncCommodities: Array[Commodity],
  cCommodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val network = new LatencyNetwork(graph, latencyFunctions, ncCommodities++cCommodities)
  private val game = new NoRegretSocialRoutingGame(network)
  val cAlgorithms = new Array[MWAlgorithm](cCommodities.length)
  val ncAlgorithms = new Array[MWAlgorithm](ncCommodities.length)
  
  for(commodityId <- cAlgorithms.indices) {
    val commodity = cCommodities(commodityId)
    val epsilon = commodity.epsilon
    val updateRule = commodity.updateRule
    val cExperts: Array[Expert] = 
      for(pathId <- cCommodities(commodityId).paths.indices.toArray) 
        yield NoRegretSocialRoutingExpert(commodityId, pathId)
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
  
  val coordinator = new MWCoordinator[NoRegretSocialRoutingGame](game, ncAlgorithms++cAlgorithms, randomizedStart)
  
  // LLF and scale
  private val solver = new SocialOptimizer(network)
  val optStrategy = solver.optimalStrategy
  val optCost = solver.optimalCost
  val LLFStrategy = solver.LLFStrategy
  val scaleStrategy = solver.scaleStrategy
  private val LLFGame = new ApproximateStackelbergRoutingGame(network, LLFStrategy)
  private val scaleGame = new ApproximateStackelbergRoutingGame(network, scaleStrategy)
  val LLFcoordinator = new MWCoordinator[ApproximateStackelbergRoutingGame](LLFGame, ncAlgorithms, randomizedStart)
  val scaleCoordinator = new MWCoordinator[ApproximateStackelbergRoutingGame](scaleGame, ncAlgorithms, randomizedStart)
  
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val ncLegend = ncCommodities.map(_.paths.map(pathToString))
  val cLegend = ncCommodities.map(_.paths.map(pathToString(_) + " (compliant)"))
  val legend = ncLegend ++ cLegend
  val legendLLF = legend.map(_.map(_+" (LLF)"))
  val legendScale = legend.map(_.map(_+" (Scale)"))
  
  // Streams
  val strategies = coordinator.strategiesStream
  val flows = coordinator.gameStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val socialCosts = flows.map(network.socialCostFromPathFlows(_))

  val LLFflows = LLFcoordinator.gameStateStream.map(_.pathFlows)
  val LLFlatencies = LLFcoordinator.lossStream
  val LLFsocialCosts = LLFflows.map(network.socialCostFromPathFlows(_))

  val scaleFlows = scaleCoordinator.gameStateStream.map(_.pathFlows)
  val scaleLatencies = scaleCoordinator.lossStream
  val scaleSocialCosts = scaleFlows.map(network.socialCostFromPathFlows(_))
  
  
  def runFor(T: Int) {
    println(scaleStrategy(0))
    println(LLFStrategy(0))
//    println(optCost)
//    println(socialCosts(T))
//    println(LLFsocialCosts(T))
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
//      .plotLineGroups(LLFflows.take(T), "t", "f(t)", legendLLF, dashed = true)
    Visualizer("LLF Path Flows").plotLineGroups(LLFflows.take(T), "t", "f(t)", legendLLF)
    Visualizer("Scale Path Flows").plotLineGroups(scaleFlows.take(T), "t", "f(t)", legendScale)
    
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
    Visualizer("LLF Path Losses").plotLineGroups(LLFlatencies.take(T), "t", "l(t)", legendLLF)
    Visualizer("Scale Path Losses").plotLineGroups(scaleLatencies.take(T), "t", "l(t)", legendScale)
    
    Visualizer("Social Costs")
      .plotLine(socialCosts.take(T), "t", "social cost", "No-regret social optimizer")
      .plotLine(LLFsocialCosts.take(T), "t", "social cost", "LLF", dashed = true)
      .plotLine(scaleSocialCosts.take(T), "t", "social cost", "Scale", dashed = true)
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost", "Social optimum", dashed = true)
    
//      Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}