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
  randomizedStart: Boolean) extends RoutingGameSimBase(graph) {

  private val network = new LatencyNetwork(graph, latencyFunctions, ncCommodities++cCommodities)
  private val game = new NoRegretSocialRoutingGame(network)
  val cAlgorithms = MWAlgorithmsFromCommodities[NoRegretSocialRoutingExpert](cCommodities)
  val ncAlgorithms = MWAlgorithmsFromCommodities[RoutingExpert](ncCommodities)
  val nashAlgorithms = MWAlgorithmsFromCommodities[RoutingExpert](ncCommodities++cCommodities)
  val coordinator = new MWCoordinator[NoRegretSocialRoutingGame](game, ncAlgorithms++cAlgorithms, randomizedStart)
  
  // LLF, scale, and Nash
  private val solver = new SocialOptimizer(network)
  val optStrategy = solver.optimalStrategy
  val optCost = solver.optimalCost
  val LLFStrategy = solver.LLFStrategy
  val scaleStrategy = solver.scaleStrategy
  private val LLFGame = new ApproximateStackelbergRoutingGame(network, LLFStrategy)
  private val scaleGame = new ApproximateStackelbergRoutingGame(network, scaleStrategy)
  private val NashGame = new RoutingGame(network)
  val LLFcoordinator = new MWCoordinator[ApproximateStackelbergRoutingGame](LLFGame, ncAlgorithms, randomizedStart)
  val scaleCoordinator = new MWCoordinator[ApproximateStackelbergRoutingGame](scaleGame, ncAlgorithms, randomizedStart)
  val NashCoordinator = new MWCoordinator[RoutingGame](NashGame, nashAlgorithms, randomizedStart)
  
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
  val NashSocialCosts = NashCoordinator.gameStateStream.map(state=>network.socialCostFromPathFlows(state.pathFlows))
  
  def runFor(T: Int) {
    println("Final compliant flows: "+flows(T)(1))
    println("Scale strategy: "+scaleStrategy(0))
    println("LLF strategy: "+LLFStrategy(0))
    
    println("Social optimal flows: "+solver.optimalTotalPathFlows(0))
    println("Social opt latencies: "+network.pathLatenciesFromPathFlows(network.pathFlowsFromStrategies(optStrategy))(0))
    
    println("Path optimizer costs: "+coordinator.gameStateStream(T).pathStackelbergCosts.toArray.toList)
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
//      .plotLineGroups(LLFflows.take(T), "t", "f(t)", legendLLF, dashed = true)
    Visualizer("LLF Path Flows").plotLineGroups(LLFflows.take(T), "t", "f(t)", legendLLF)
    Visualizer("Scale Path Flows").plotLineGroups(scaleFlows.take(T), "t", "f(t)", legendScale)
    
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
    Visualizer("LLF Path Losses").plotLineGroups(LLFlatencies.take(T), "t", "l(t)", legendLLF)
    Visualizer("Scale Path Losses").plotLineGroups(scaleLatencies.take(T), "t", "l(t)", legendScale)
    
    Visualizer("Social Costs")
      .plotLine(socialCosts.take(T), "t", "social cost", "No-regret social optimizer")
      .plotLine(LLFsocialCosts.take(T), "t", "social cost", "LLF")
      .plotLine(scaleSocialCosts.take(T), "t", "social cost", "Scale")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost", "Social optimum", dashed = true)
      .plotLine(NashSocialCosts.take(T), "t", "social cost", "Nash eq.", dashed = true)
    
//      Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}