package routing

import scala.collection.mutable.HashMap
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.util.parsing.json._



class LatencyNetwork(
    graph: DirectedGraph, 
    latencies: HashMap[Int, LatencyFunction],
    commodities: Array[Commodity]) {
  
  val edges = graph.edges
  val nbNodes = graph.nodes.size
  val nbEdges = edges.size

  private val edgePathIncidence = new Array[DenseMatrix[Double]](commodities.size)
  
  for (k <- commodities.indices) {
    val paths = commodities(k).paths
    val nbPaths = paths.size
    edgePathIncidence(k) = DenseMatrix.zeros[Double](nbEdges, nbPaths)
    for (i <- 0 to nbEdges - 1; j <- 0 to nbPaths - 1)
      if (paths(j).contains(i))
        edgePathIncidence(k)(i, j) = 1
  }

  protected def edgeFlowsFromPathFlows(pathFlows: Array[DenseVector[Double]]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (k <- commodities.indices) {
      edgeLatencies :+= edgePathIncidence(k) * pathFlows(k)
    }
    edgeLatencies
  }
  
  protected def edgeLatenciesFromEdgeFlows(edgeFlows: DenseVector[Double]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (edgeId <- 0 to nbEdges - 1)
      edgeLatencies(edgeId) = latencies(edgeId)(edgeFlows(edgeId))
    edgeLatencies
  }

  def pathLatenciesFromPathFlows(pathFlows: Array[DenseVector[Double]]): Array[DenseVector[Double]] = {
    val edgeFlows = edgeFlowsFromPathFlows(pathFlows)
    val edgeLatencies = edgeLatenciesFromEdgeFlows(edgeFlows)
    pathFlows.indices.map(groupId => edgePathIncidence(groupId).t * edgeLatencies).toArray
  }
  
  def pathFlowsFromStrategies(strategies: Array[DenseVector[Double]]) ={
    val demands = commodities map (_.demand())
    for((strategy, totalFlow) <- strategies zip demands) 
      yield strategy*totalFlow
  } 
  
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