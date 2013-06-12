package mw


import scala.collection.mutable.HashMap
import util.Visualizer
import breeze.linalg.DenseVector


class StackelbergRoutingGame(ncTotalFlows: Array[Double], cTotalFlows: Array[Double], network: Network, derivativeNetwork: Network) extends Nature {
  case class NetworkState(
      pathFlows: Array[DenseVector[Double]], 
      pathLatencies: Array[DenseVector[Double]],
      pathDLatencies: Array[DenseVector[Double]]
      )
  
  type State = NetworkState
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val nbPaths = strategies.length/2
    val pathFlows = 
      (for(id <- 0 until nbPaths; 
        ncStrategy = strategies(id);
        cStrategy = strategies(nbPaths+id))
          yield (ncStrategy*ncTotalFlows(id)+cStrategy*cTotalFlows(id))) toArray
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathDLatencies = derivativeNetwork.pathLatenciesFromPathFlows(pathFlows)
    NetworkState(pathFlows, pathLatencies, pathDLatencies)
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

object StackelbergDirectedGraph{
  def fromAdjacencyMap(adj: Map[Int, List[(Int, Double=>Double, Double => Double)]]): (DirectedGraph, DirectedGraph) = {
    val latencyAdj = 
      for((fromId, neighb) <- adj)
        yield fromId -> 
          (for((toId, latency, dLatency) <- neighb) yield (toId, latency))
        
    val dLatencyAdj = 
      for((fromId, neighb) <- adj)
        yield fromId -> 
          (for((toId, latency, dLatency) <- neighb) yield (toId, dLatency))
          
    (DirectedGraph.fromAdjacencyMap(latencyAdj), DirectedGraph.fromAdjacencyMap(dLatencyAdj))
  }
}

case class StackelbergRoutingExpert(groupId: Int, pathId: Int) extends Expert

class StackelbergRoutingGameSim(
  adj: Map[Int, List[(Int, Double => Double, Double => Double)]],
  sourceSinkPairs: Array[(Int, Int)],
  nonCompliantTotalFlows: Array[Double],
  compliantTotalFlows: Array[Double],
  updateRule: UpdateRule,
  randomizedStart: Boolean) {

  private val (graph, dGraph) = StackelbergDirectedGraph.fromAdjacencyMap(adj)
  private val network = new Network(graph, sourceSinkPairs)
  private val dNetwork = new Network(dGraph, sourceSinkPairs)
  
  // this default value will only be used for initialization. Van change it later
  val defaultEpsilon = (t:Int) => 10. / (10 + t)
  
  private val game = new StackelbergRoutingGame(nonCompliantTotalFlows, compliantTotalFlows, network, dNetwork)
  
  private val nbPairs = sourceSinkPairs.length
  val algorithms = new Array[MWAlgorithm](nbPairs*2)
  
  for(groupId <- sourceSinkPairs.indices) {
    val nonCompliantExperts: Array[Expert] = 
      for(pathId <- network.pathsArray(groupId).indices.toArray) 
        yield RoutingExpert(groupId, pathId)
    val compliantExperts: Array[Expert] = 
      for(pathId <- network.pathsArray(groupId).indices.toArray) 
        yield StackelbergRoutingExpert(groupId, pathId)
    algorithms(groupId) = new MWAlgorithm(defaultEpsilon, nonCompliantExperts, updateRule)
    algorithms(nbPairs + groupId) = new MWAlgorithm(defaultEpsilon, compliantExperts, updateRule)
  }
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val ncLegend = network.pathsArray.map(paths => paths.map(pathToString))
  val cLegend = network.pathsArray.map(paths => paths.map(pathToString(_) + " (compliant)"))
  val legend = ncLegend ++ cLegend
  
  val coordinator = new MWCoordinator[StackelbergRoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
//  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val flows = 
    strategies.map(_ zip (nonCompliantTotalFlows++compliantTotalFlows) map (x => x._1*x._2))
  
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream

  
  def runFor(T: Int) {
    System.out.println(network.toJSON())
    Visualizer.plotLineGroups(flows.take(T), "t", "f(t)", "Path Flows", legend)
    Visualizer.plotLineGroups(latencies.take(T), "t", "l(t)", "Path Losses", legend)
    Visualizer.plotLineGroups(avgLatencies.take(T), "t", "\\sum_{i = 1}^t l(i)", "Average Latencies", legend)
    Visualizer.plotStrategies(strategies.take(T)) 
    Visualizer.plotFunctions(graph.edges.map(_._2.latency).toArray, (0, (compliantTotalFlows++nonCompliantTotalFlows) sum), "f", "l(f)", "latencies", 100)
  }
}