package routing

import scala.collection.mutable.HashMap
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.util.parsing.json._



class LatencyNetwork(
    val graph: DirectedGraph, 
    val latencyFunctions: HashMap[Int, LatencyFunction],
    val commodities: Array[Commodity]) {
  
  val edges = graph.edges
  val nbNodes = graph.nodes.size
  val nbEdges = edges.size

  // Cost functions, computed from the latency functions
  private val identity = StaticLatencyFunction(x=>x)
  private val tollFunctions = for((key, lat) <- latencyFunctions) yield key->lat.derivative*identity
  private val socialCostFunctions = for((key, lat) <- latencyFunctions) yield key->lat*identity
  private val stackelbergCostFunctions = for((key, lat) <- latencyFunctions) yield key->(lat + (lat.derivative*identity))
  
  // edge-path incidence matrix
  private val edgePathIncidence = new Array[DenseMatrix[Double]](commodities.size)
  
  for (k <- commodities.indices) {
    val paths = commodities(k).paths
    val nbPaths = paths.size
    edgePathIncidence(k) = DenseMatrix.zeros[Double](nbEdges, nbPaths)
    for (i <- 0 to nbEdges - 1; j <- 0 to nbPaths - 1)
      if (paths(j).contains(i))
        edgePathIncidence(k)(i, j) = 1
  }

  // Helper functions
  protected def edgeFlowsFromPathFlows(pathFlows: Array[DenseVector[Double]]): DenseVector[Double] = {
    val edgeFlows = DenseVector.zeros[Double](nbEdges)
    for (k <- commodities.indices) {
      edgeFlows :+= edgePathIncidence(k) * pathFlows(k)
    }
    edgeFlows
  }
  
  protected def edgeLossesFromEdgeFlows(lossFunctions: HashMap[Int, LatencyFunction])(edgeFlows: DenseVector[Double]): DenseVector[Double] = {
    val edgeLosses = DenseVector.zeros[Double](nbEdges)
    for (edgeId <- 0 to nbEdges - 1)
      edgeLosses(edgeId) = lossFunctions(edgeId)(edgeFlows(edgeId))
    edgeLosses
  }
  
  private def pathLossesFromPathFlows(lossFunctions: HashMap[Int, LatencyFunction])(pathFlows: Array[DenseVector[Double]]): Array[DenseVector[Double]] = {
    val edgeFlows = edgeFlowsFromPathFlows(pathFlows)
    val edgeLosses = edgeLossesFromEdgeFlows(lossFunctions)(edgeFlows)
    pathFlows.indices.map(groupId => edgePathIncidence(groupId).t * edgeLosses).toArray
  }
  
  // public API
  def socialCostFromPathFlows(pathFlows: Array[DenseVector[Double]]): Double = {
    val edgeFlows = edgeFlowsFromPathFlows(pathFlows)
    val edgeCosts = edgeLossesFromEdgeFlows(socialCostFunctions)(edgeFlows)
    edgeCosts.sum
  }
    
  
  def pathLatenciesFromPathFlows(pathFlows: Array[DenseVector[Double]]): Array[DenseVector[Double]] =
    pathLossesFromPathFlows(latencyFunctions)(pathFlows)
  
  def pathTollsFromPathFlows(pathFlows: Array[DenseVector[Double]]): Array[DenseVector[Double]] =
    pathLossesFromPathFlows(tollFunctions)(pathFlows)
  
  def pathStackelbergCostsFromPathFlows(pathFlows: Array[DenseVector[Double]]): Array[DenseVector[Double]] =
    pathLossesFromPathFlows(stackelbergCostFunctions)(pathFlows)
    
  def pathFlowsFromStrategies(strategies: Array[DenseVector[Double]]) ={
    val demands = commodities map (_.demand())
    for((strategy, totalFlow) <- strategies zip demands) 
      yield strategy*totalFlow
  } 
  
  // JSON
  def dumpJSON(fileName: String) {
    import java.io._
    val writer = new BufferedWriter(new FileWriter(fileName))
    writer.write(toJSON())
    writer.close()
  }
  
  def toJSON() : String = {
    def groupOf(id: Int): Int = { 
      val list = for(
          i <- commodities.indices; 
          source = commodities(i).source;
          sink = commodities(i).sink;
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