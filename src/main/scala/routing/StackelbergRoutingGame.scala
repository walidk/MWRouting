package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class StackelbergRoutingGame(network: LatencyNetwork, derivativeNetwork: LatencyNetwork) extends Game {
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
    val pathCFlows = network.pathFlowsFromStrategies(strategies.takeRight(nbPaths))
    val pathFlows = (pathNCFlows zip pathCFlows) map ({case(x, y) => x+y})
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathDLatencies = derivativeNetwork.pathLatenciesFromPathFlows(pathFlows)
    NetworkState(pathNCFlows, pathCFlows, pathFlows, pathLatencies, pathDLatencies)
  }
  
  protected def getDLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathDLatencies(groupId)(pathId)
  
    protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)
    case StackelbergRoutingExpert(groupId, pathId) => 
      getLatency(state)(groupId)(pathId) +
      getDLatency(state)(groupId)(pathId)*state.pathFlows(groupId)(pathId)
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

  private val network = new LatencyNetwork(graph, latencyFunctions, cCommodities)
  private val dNetwork = new LatencyNetwork(graph, latencyDerivatives, ncCommodities)
  
  private val game = new StackelbergRoutingGame(network, dNetwork)
  
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
  
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream

  
  def runFor(T: Int) {
    System.out.println(network.toJSON())
    Visualizer.plotLineGroups(flows.take(T), "t", "f(t)", "Path Flows", legend)
    Visualizer.plotLineGroups(latencies.take(T), "t", "l(t)", "Path Losses", legend)
    Visualizer.plotLineGroups(avgLatencies.take(T), "t", "\\sum_{i = 1}^t l(i)", "Average Latencies", legend)
    Visualizer.plotStrategies(strategies.take(T))
  }
}