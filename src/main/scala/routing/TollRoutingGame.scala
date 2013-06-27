package routing
import util.Visualizer
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import mw._


class TollRoutingGame(network: LatencyNetwork, tollNetwork: LatencyNetwork, tollDelay: Int, tollInterval: Int = 1) extends Game {
  case class NetworkState(
      time: Int,
      pathFlows: Array[DenseVector[Double]],
      pathLatencies: Array[DenseVector[Double]],
      pathTolls: Array[DenseVector[Double]],
      futurePathTolls: List[Array[DenseVector[Double]]]
      ) extends GameState
  
  type State = NetworkState 
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    val zeroTolls = strategies.map(s=>DenseVector.zeros[Double](s.size))
    assert(tollDelay >= 0, "tollDelay must be a non-negative integer")
    assert(tollInterval >= 1, "tollInterval must be positive integer")
    val pathTollsSeq = for(i <- 0 to tollDelay) yield zeroTolls
    update(NetworkState(0, null, null, null, pathTollsSeq.toList), strategies)
  }
  
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val time = state.time+1
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathTolls = tollNetwork.pathLatenciesFromPathFlows(pathFlows)
    val futurePathTolls = 
      if(time%tollInterval == 0) 
        state.futurePathTolls.tail:::List(pathTolls)
      else
        state.futurePathTolls
    NetworkState(time, pathFlows, pathLatencies, state.futurePathTolls.head, futurePathTolls)
  }
  
  protected def getToll(state: State)(groupId: Int)(pathId: Int) = 
    state.pathTolls(groupId)(pathId)
  
  protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)+getToll(state)(groupId)(pathId)
  }
}

case class TollRoutingExpert(groupId: Int, pathId: Int) extends Expert

class TollRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  latencyDerivatives: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  tollDelay: Int,
  tollInterval: Int,
  randomizedStart: Boolean) {

  private val identity = StaticLatencyFunction(x=>x)
  private val tollFunctions =
    for((key, dlat)<-latencyDerivatives)
      yield key->dlat*identity
  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val tollNetwork = new LatencyNetwork(graph, tollFunctions, commodities)
  
  private val game = new TollRoutingGame(network, tollNetwork, tollDelay, tollInterval)
  
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
  
  val coordinator = new MWCoordinator[TollRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = coordinator.gameStateStream.map(state => state.pathFlows)
  val tolls = coordinator.gameStateStream.map(state => state.pathTolls)
  
  private def socialCostOfState(state: coordinator.GameState): Double = {
    val flows = state.pathFlows
    val latencies = state.pathLatencies
    var cost = 0.
    for((flow, latency) <- flows.zip(latencies))
      cost+=(flow:*latency).sum
    cost
  }
  
  val socialCosts = coordinator.gameStateStream.map(socialCostOfState)
  val latencies = coordinator.gameStateStream.map(_.pathLatencies)
  val losses = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream

  
  def runFor(T: Int) {
    System.out.println(network.toJSON())
    Visualizer.plotLineGroups(flows.take(T), "t", "f(t)", "Path Flows", legend)
//    Visualizer.plotLineGroups(latencies.take(T), "t", "lat(t)", "Path Latencies)", legend)
    Visualizer.plotLineGroups(tolls.take(T), "t", "toll(t)", "Path Tolls", legend)
    Visualizer.plotLineGroups(losses.take(T), "t", "loss(t)", "Path Losses (latency + toll)", legend)
    Visualizer.plotLine(socialCosts.take(T), "t", "social cost", "Social Costs")
    Visualizer.plotStrategies(strategies.take(T))
  }
}