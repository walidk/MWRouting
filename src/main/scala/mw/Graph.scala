package mw

import scala.collection.mutable.HashMap
import breeze.linalg._

case class Edge(from:Node, to:Node) {
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
  
  def addEdge(from: Node, to: Node) {
    val edge = Edge(from, to)
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
  def apply(adjMatrix: DenseMatrix[Int]): DirectedGraph = {
    val graph = new DirectedGraph()
    val n = adjMatrix.rows
    
    (0 to n-1).map(id => graph.addNode(new Node(id)))
    
    val nodes = graph.nodes
    
    for(fromId <- 0 to n-1; toId <- 0 to n-1; if(adjMatrix(fromId, toId) == 1)){
      val from = nodes(fromId)
      val to = nodes(toId)
      graph.addEdge(from, to)      
    }
    
    graph
  }
  
}