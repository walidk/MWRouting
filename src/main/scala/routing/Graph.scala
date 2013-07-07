package routing

import scala.collection.mutable.HashMap
import breeze.linalg._

case class Edge(id: Int, from:Node, to:Node) {
  override def toString(): String = {
    from.id + "->" + to.id
  }
}

case class Node(id: Int) {
  val inEdges = HashMap[Int, Edge]()
  val outEdges = HashMap[Int, Edge]()
  
  def preNodes = inEdges.values.map(_.from)
  def sucNodes = outEdges.values.map(_.to)
  
}

class DirectedGraph {
  val nodes = HashMap[Int, Node]()
  val edges = HashMap[Int, Edge]()
  var edgeId = 0
  
  def addNode(node: Node) {
    nodes += (node.id->node)
  }
  
  def addEdge(from: Node, to: Node): Edge = {
    val edge = Edge(edgeId, from, to)
    edges += edgeId->edge
    from.outEdges += (edgeId->edge)
    to.inEdges += (edgeId->edge)
    edgeId+=1
    edge
  }
  
  override def toString(): String = {
    "Directed Graph (\nnodes=" + 
    nodes.values.map(_.toString()) + 
    "\nedges=" + edges.values.map(_.toString) +
    ")"
  }
  
  def findLooplessPaths(sourceId: Int, sinkId: Int): Array[List[Int]] = {
    val paths = findLooplessPathsAsEdgeList(sourceId: Int, sinkId: Int)
    paths.map(_.map(_.id))
  }
  
  def findLooplessPathsAsEdgeList(sourceId: Int, sinkId: Int): Array[List[Edge]] = {
    type IntSet = scala.collection.immutable.HashSet[Int]
    val sink = nodes(sinkId)
    def findPathsRec(current: Node, exploredIds: IntSet): Iterable[List[Edge]] = {
      if(current == sink)
        List(Nil)
      else{
        val newExplored = exploredIds+(current.id)
        val edgesToExplore = current.outEdges.values.filterNot(edge => newExplored.contains(edge.to.id))
        edgesToExplore.flatMap(edge => findPathsRec(edge.to, newExplored).map(edge::_))
      }
    }
    findPathsRec(nodes(sourceId), new IntSet()).toArray
  }
}


/** Companion object.
 *  Helper class, provides some factory methods to construct graphs.
 */
object DirectedGraph{
  def graphFromAdjMap(adj: Map[Int, List[Int]]): DirectedGraph = {
    val graph = new DirectedGraph()
    
    for(id <- adj.keys)
      graph.addNode(new Node(id))
    val nodes = graph.nodes
    
    for((fromId, neighb) <- adj; toId <- neighb){
      val from = nodes(fromId)
      val to = nodes(toId)
      graph.addEdge(from, to)
    }
    graph
  }
  
  
  def graphAndLatenciesFromAdjMap(adj: Map[Int, List[(Int, LatencyFunction)]]): (DirectedGraph, HashMap[Int, LatencyFunction]) = { 
    val graph = new DirectedGraph()
    val latencyFunctions = new HashMap[Int, LatencyFunction]
    
    for(id <- adj.keys)
      graph.addNode(new Node(id))
    val nodes = graph.nodes
    
    for((fromId, neighb) <- adj; (toId, lat) <- neighb){
      val from = nodes(fromId)
      val to = nodes(toId)
      val edge = graph.addEdge(from, to)
      latencyFunctions(edge.id) = lat
    }
    (graph, latencyFunctions)
  }
}