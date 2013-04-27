package mw

import scala.collection.mutable.HashMap
import breeze.linalg._

case class Edge(from:Node, to:Node, latency: Double => Double) {
  override def toString(): String = {
    from.id + "->" + to.id
  }
}

case class Node(id: Int) {
  val inEdges = HashMap[Int, Edge]()
  val outEdges = HashMap[Int, Edge]()
  
  def pred = inEdges.values.map(_.from)
  def succ = outEdges.values.map(_.to)
  
}



class DirectedGraph {
  val nodes = HashMap[Int, Node]()
  val edges = HashMap[(Int, Int), Edge]()
  
  def addNode(node: Node) {
    nodes += (node.id->node)
  }
  
  def addEdge(from: Node, to: Node, latency: Double=>Double) {
    val edge = Edge(from, to, latency)
    edges += (from.id, to.id)->edge
    from.outEdges += (to.id->edge)
    to.inEdges += (from.id->edge)
  }
  
  override def toString(): String = {
    "Directed Graph (\nnodes=" + 
    nodes.values.map(_.toString()) + 
    "\nedges=" + edges.values.map(_.toString) +
    ")"
  }
}



object DirectedGraph{
  def apply(adj: List[List[(Int, Double=>Double)]]): DirectedGraph = {
    val graph = new DirectedGraph()
    val n = adj.size
    
    (0 to n-1).map(id => graph.addNode(new Node(id)))
    val nodes = graph.nodes
    
    for((neighb, fromId) <- adj.zipWithIndex; (toId, latency) <- neighb){
      val from = nodes(fromId)
      val to = nodes(toId)
      graph.addEdge(from, to, latency)
    }
    
    graph
  }
  
}