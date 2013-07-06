package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class StackelbergRoutingGame(network: LatencyNetwork, stackelbergNetwork: LatencyNetwork) extends Game {
  case class NetworkState(
      pathNCFlows: Array[DenseVector[Double]],
      pathCFlows: Array[DenseVector[Double]],
      pathFlows: Array[DenseVector[Double]],
      pathLatencies: Array[DenseVector[Double]],
      pathDLatencies: Array[DenseVector[Double]]
      ) extends GameState
  
  type State = NetworkState 
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val nbPaths = strategies.length/2
    val pathNCFlows = network.pathFlowsFromStrategies(strategies.take(nbPaths))
    val pathCFlows = stackelbergNetwork.pathFlowsFromStrategies(strategies.takeRight(nbPaths))
    val pathFlows = (pathNCFlows zip pathCFlows) map ({case(x, y) => x+y})
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathDLatencies = stackelbergNetwork.pathLatenciesFromPathFlows(pathFlows)
    NetworkState(pathNCFlows, pathCFlows, pathFlows, pathLatencies, pathDLatencies)
  }
  
  protected def getDLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathDLatencies(groupId)(pathId)
  
    protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)
    case StackelbergRoutingExpert(groupId, pathId) => getDLatency(state)(groupId)(pathId)
  }
}

case class StackelbergRoutingExpert(groupId: Int, pathId: Int) extends Expert

class StackelbergRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  latencyDerivatives: HashMap[Int, LatencyFunction],
  ncCommodities: Array[Commodity],
  cCommodities: Array[Commodity],
  randomizedStart: Boolean) {

  private val identity = StaticLatencyFunction(x => x)
  private val stackelbergLatencies = 
    for((key, dlat) <- latencyDerivatives; lat = latencyFunctions(key))
      yield key->(lat*identity+dlat)
  private val network = new LatencyNetwork(graph, latencyFunctions, cCommodities)
  private val sNetwork = new LatencyNetwork(graph, stackelbergLatencies, ncCommodities)
  
  private val game = new StackelbergRoutingGame(network, sNetwork)
  
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
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val ncLegend = ncCommodities.map(_.paths.map(pathToString))
  val cLegend = ncCommodities.map(_.paths.map(pathToString(_) + " (compliant)"))
  val legend = ncLegend ++ cLegend
  
  val coordinator = new MWCoordinator[StackelbergRoutingGame](game, ncAlgorithms++cAlgorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = coordinator.gameStateStream.map(state => state.pathNCFlows++state.pathCFlows)
  val totalFlows = coordinator.gameStateStream.map(state => state.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream
  val socialCosts = totalFlows.map(network.socialCostFromPathFlows(_))
  val solver = SocialOptimizer(graph, latencyFunctions, ncCommodities, cCommodities)
  val optStrategy = solver.optimalStrategy
  val optCost = solver.optimalCost
  
  def runFor(T: Int) {
    System.out.println(network.toJSON())
    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend)
    Visualizer("Path Losses").plotLineGroups(latencies.take(T), "t", "l(t)", legend)
    Visualizer("Average Latencies").plotLineGroups(avgLatencies.take(T), "t", "Avg latency", legend)
    Visualizer("Social Costs").plotLine(socialCosts.take(T), "t", "social cost")
      .plotLine(Stream.continually(optCost).take(T), "t", "social cost")
    Visualizer("Strategies").plotStrategies(strategies.take(T))
  }
}