package mw

import scala.collection.mutable.HashMap
import breeze.linalg._



case class Edge(id: Int, from:Node, to:Node, latency: Double => Double) {
  override def toString(): String = {
    from.id + "->" + to.id
  }
}

case class Path(edges: List[Edge])

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
  
  def addEdge(from: Node, to: Node, latency: Double=>Double) {
    val edge = Edge(edgeId, from, to, latency)
    edges += edgeId->edge
    from.outEdges += (edgeId->edge)
    to.inEdges += (edgeId->edge)
    edgeId+=1
  }
  
  override def toString(): String = {
    "Directed Graph (\nnodes=" + 
    nodes.values.map(_.toString()) + 
    "\nedges=" + edges.values.map(_.toString) +
    ")"
  }
  
  def findLooplessPaths(sourceId: Int, sinkId: Int): List[Path] = {
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
    findPathsRec(nodes(sourceId), new IntSet()).toList.map(Path(_))
  }
}

object DirectedGraph{
  def fromAdjacencyMap(adj: Map[Int, List[(Int, Double=>Double)]]): DirectedGraph = {
    val graph = new DirectedGraph()
    
    for(id <- adj.keys)
      graph.addNode(new Node(id))
    val nodes = graph.nodes
    
    for((fromId, neighb) <- adj; (toId, latency) <- neighb){
      val from = nodes(fromId)
      val to = nodes(toId)
      graph.addEdge(from, to, latency)
    }
    graph
  }
}