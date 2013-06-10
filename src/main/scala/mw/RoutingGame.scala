package mw

import breeze.linalg._
import scala.collection.mutable.HashMap
import scala.util.parsing.json._
import util.Visualizer

class Network(graph: DirectedGraph, val sourceSinkPairs: Array[(Int, Int)]) {
  val edges = graph.edges
  val nbNodes = graph.nodes.size
  val nbEdges = edges.size
  val pathsArray: Array[Array[List[Int]]] =
    sourceSinkPairs.map(s => graph.findLooplessPaths(s._1, s._2).toArray.map(_.edges.map(_.id)))

  val edgePathIncidence = new Array[DenseMatrix[Double]](sourceSinkPairs.size)
  
  for (k <- sourceSinkPairs.indices) {
    val paths = pathsArray(k)
    val nbPaths = paths.size
    edgePathIncidence(k) = DenseMatrix.zeros[Double](nbEdges, nbPaths)
    for (i <- 0 to nbEdges - 1; j <- 0 to nbPaths - 1)
      if (paths(j).contains(i))
        edgePathIncidence(k)(i, j) = 1
  }

  def edgeLatencies(edgeFlows: DenseVector[Double]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (edgeId <- 0 to nbEdges - 1)
      edgeLatencies(edgeId) = edges(edgeId).latency(edgeFlows(edgeId))
    edgeLatencies
  }

  def pathLatency(edgeLatencies: DenseVector[Double])(groupId: Int)(pathId: Int): Double = {
    (edgePathIncidence(groupId).t * edgeLatencies).apply(pathId)
  }

  def computeEdgeFlows(pathFlows: Array[DenseVector[Double]]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (k <- sourceSinkPairs.indices) {
      edgeLatencies :+= edgePathIncidence(k) * pathFlows(k)
    }
    edgeLatencies
  }

  def toJSON() : String = {
    def groupOf(id: Int): Int = { 
      val list = for(
          i <- sourceSinkPairs.indices; 
          (source, sink) = sourceSinkPairs(i); 
          if source == id || sink == id
          ) yield i
      list.headOption match {
        case None => 0
        case Some(group) => group + 1
      }
    }
    
    def JSONify(obj: Any): Any = obj match {
      case map: Map[String, Any] => JSONObject(map.map(e => e._1->JSONify(e._2)))
      case list: List[Any] => JSONArray(list.map(JSONify))
      case _ => obj
    }
    
    val jsonObject = JSONify(Map(
      "nodes"->
        (0 to nbNodes - 1).toList.map(id => Map(
            "name" -> id, 
            "group" -> groupOf(id))
        ),
      "links"->
        (0 to nbEdges - 1).toList.map(edgeId => Map(
            "source"->edges(edgeId).from.id,
            "target"->edges(edgeId).to.id,
            "value"->1)
        )
      ))
      
    jsonObject.toString
  }
}





class RoutingGame(totalFlows: Array[Double], network: Network) extends Nature {
  case class NetworkState(
      pathFlows: Array[DenseVector[Double]], 
      edgeFlows: DenseVector[Double],
      edgeLatencies: DenseVector[Double])
  
  type State = NetworkState
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val pathFlows = for((strategy, totalFlow) <- strategies zip totalFlows) yield strategy*totalFlow
    val edgeFlows = network.computeEdgeFlows(pathFlows)
    val edgeLatencies = network.edgeLatencies(edgeFlows)
    NetworkState(pathFlows, edgeFlows, edgeLatencies)
  }
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getLatency(state)(groupId)(pathId)
  }
  
  private def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    network.pathLatency(state.edgeLatencies)(groupId)(pathId)
}




case class RoutingExpert(groupId: Int, pathId: Int) extends Expert


class RoutingGameSim(
  adj: Map[Int, List[(Int, Double => Double)]],
  sourceSinkPairs: Array[(Int, Int)],
  totalFlows: Array[Double],
  updateRule: UpdateRule,
  randomizedStart: Boolean) {

  private val graph = DirectedGraph.fromAdjacencyMap(adj)
  private val network = new Network(graph, sourceSinkPairs)
  // this default value will only be used for initialization. Van change it later
  val defaultEpsilon = (t:Int) => 10. / (10 + t)
  
  private val game = new RoutingGame(totalFlows, network)
  
  val algorithms = new Array[MWAlgorithm](sourceSinkPairs.length)
  for(groupId <- sourceSinkPairs.indices) {
    val experts: Array[Expert] = 
      for(pathId <- network.pathsArray(groupId).indices.toArray) 
        yield RoutingExpert(groupId, pathId)
    algorithms(groupId) = new MWAlgorithm(defaultEpsilon, experts, updateRule)
  }
  
  private def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
    case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
  }
  
  val legend = network.pathsArray.map(paths => paths.map(pathToString))
  val coordinator = new MWCoordinator[RoutingGame](game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
  val flows = coordinator.natureStateStream.map(_.pathFlows)
  val latencies = coordinator.lossStream
  val avgLatencies = coordinator.averageLossStream

  
  def runFor(T: Int) {
    System.out.println(network.toJSON())
    Visualizer.plotLineGroups(flows.take(T), "t", "f(t)", "Path Flows", legend)
    Visualizer.plotLineGroups(latencies.take(T), "t", "l(t)", "Path Latencies", legend)
    Visualizer.plotLineGroups(avgLatencies.take(T), "t", "\\sum_{i = 1}^t l(i)", "Average Latencies", legend)
    Visualizer.plotStrategies(strategies.take(T)) 
  }
}