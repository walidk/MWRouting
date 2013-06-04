package mw

import breeze.linalg._
import scala.collection.mutable.HashMap
import util.Visualizer

class Network(graph: DirectedGraph, val groupSourceSinks: Array[(Int, Int)]) {
  val edges = graph.edges
  val nbNodes = graph.nodes.size
  val nbEdges = edges.size
  val nbGroups = groupSourceSinks.size
  val groupPaths: Array[Array[List[Int]]] =
    groupSourceSinks.map(s => graph.findLooplessPaths(s._1, s._2).toArray.map(_.edges.map(_.id)))

  val edgePathInc = new Array[DenseMatrix[Double]](nbGroups)
  for (k <- 0 to nbGroups - 1) {
    val paths = groupPaths(k)
    val nbPaths = paths.size
    edgePathInc(k) = DenseMatrix.zeros[Double](nbEdges, nbPaths)
    for (i <- 0 to nbEdges - 1; j <- 0 to nbPaths - 1)
      if (paths(j).contains(i))
        edgePathInc(k)(i, j) = 1
  }

  def edgeLatencies(edgeFlows: DenseVector[Double]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (edgeId <- 0 to nbEdges - 1)
      edgeLatencies(edgeId) = edges(edgeId).latency(edgeFlows(edgeId))
    edgeLatencies
  }

  def pathLatency(edgeLatencies: DenseVector[Double])(groupId: Int)(pathId: Int): Double = {
    (edgePathInc(groupId).t * edgeLatencies).apply(pathId)
  }

  def computeEdgeFlows(pathFlows: Array[DenseVector[Double]]): DenseVector[Double] = {
    val edgeLatencies = DenseVector.zeros[Double](nbEdges)
    for (k <- 0 to nbGroups - 1) {
      edgeLatencies :+= edgePathInc(k) * pathFlows(k)
    }
    edgeLatencies
  }

}

class RoutingGame(totalFlows: Array[Double], network: Network) extends Nature {
  val nbEdges = network.nbEdges
  val nbGroups = totalFlows.size
  var pathFlows: Array[DenseVector[Double]] = new Array[DenseVector[Double]](nbGroups)
  for (k <- 0 to nbGroups - 1) {
    val nbPaths = network.groupPaths(k).size
    pathFlows(k) = DenseVector.fill[Double](nbPaths) { totalFlows(k) / nbPaths }
  }

  var edgeFlows = DenseVector.zeros[Double](nbEdges)
  var edgeLatencies = DenseVector.zeros[Double](nbEdges)

  def getLatency(groupId: Int)(pathId: Int) = network.pathLatency(edgeLatencies)(groupId)(pathId)

  def getLatencies(groupId: Int) = new DenseVector[Double]((0 to pathFlows(groupId).size - 1).map(getLatency(groupId)(_)).toArray)

  def update(groupId: Int, strategy: DenseVector[Double]) = {
    pathFlows(groupId) = strategy * totalFlows(groupId)
    if (groupId == nbGroups - 1) {
      edgeFlows = network.computeEdgeFlows(pathFlows)
      edgeLatencies = network.edgeLatencies(edgeFlows)
    }
  }

  def toJSON() : String = {
    var content = new String()
    content = "{ \"nodes\":[ \n"
    for (id <- 0 to network.nbNodes - 1) {
      var cat = 0
      for (k <- 0 to nbGroups -1) {
        cat = if ( (network.groupSourceSinks(k)._1 == id) || (network.groupSourceSinks(k)._2 == id) ) k+1 else cat
      }
      content += "{\"name\":"+ id +", \"group\":"+ cat +"},\n"
    }
    content = content.substring(0, content.length()-2) + "], \n \"links\":[ \n"
    for (edgeId <- 0 to nbEdges - 1) {
      content += "{\"source\":" + network.edges(edgeId).from.id + ", \"target\":" + network.edges(edgeId).to.id + ", \"value\":" + edgeLatencies(edgeId) + "},\n"
    }
    content = content.substring(0, content.length()-2) + "] }"
    content
  }
  
}

class RoutingExpert(game: RoutingGame, val groupId: Int, val pathId: Int) extends Expert(game) {
  def nextLoss(): Double = {
    game.getLatency(groupId)(pathId)
  }
}

class RoutingGameSim(
  adj: Map[Int, List[(Int, Double => Double)]],
  sourceSinkPairs: Array[(Int, Int)],
  totalFlows: Array[Double], 
  randomizedStart: Boolean) {

  var singleStrategyPlot = false
  val graph = DirectedGraph.fromAdjacencyMap(adj)
  val network = new Network(graph, sourceSinkPairs)
  val K = network.nbGroups
  val eps: Array[Int => Double] = (0 to K-1).map(k=> (t:Int) => 10. / (10 + t)).toArray
  
  def launch(T: Int) {
    val game = new RoutingGame(totalFlows, network)
    val experts = new Array[List[RoutingExpert]](K)
    for (k <- 0 to K - 1)
      experts(k) = (0 to network.groupPaths(k).size - 1).map(new RoutingExpert(game, k, _)).toList

    val algs = new Array[MWAlgorithm[RoutingGame]](K)
    for (k <- 0 to K - 1)
      algs(k) = new MultilinearMWAlgorithm[RoutingGame](k, eps(k), experts(k), game, randomizedStart)

      
    def pathToString(edgeList: List[Int]): String = edgeList match {
      case Nil => ""
      case h::Nil => {val edge = graph.edges(h); edge.from.id + "->" + edge.to.id}
      case h::t => {val edge = graph.edges(h); edge.from.id + "->" + pathToString(t)}
    }  
    
    // simulation
    val xs = new Array[DenseMatrix[Double]](K)
    val ls = new Array[DenseMatrix[Double]](K)
    val avgLs = new Array[DenseMatrix[Double]](K)
    val names = new Array[Array[String]](K)
    for (k <- 0 to K - 1) {
      val nbExperts = experts(k).length
      xs(k) = DenseMatrix.zeros[Double](nbExperts, T)
      ls(k) = DenseMatrix.zeros[Double](nbExperts, T)
      avgLs(k) = DenseMatrix.zeros[Double](nbExperts, T)
      names(k) = network.groupPaths(k).map(pathToString)
    }

    for (t <- 0 to T - 1) {
      for (alg <- algs)
        alg.next()
      for (k <- 0 to K - 1) {
        xs(k)(::, t) := game.pathFlows(k)
        ls(k)(::, t) := game.getLatencies(k)
        val alpha = t.doubleValue/(t+1)
        avgLs(k)(::, t) := 
          {if(t == 0)
            ls(k)(::, t)
          else
            avgLs(k)(::, t-1)*alpha + ls(k)(::, t)*(1-alpha)
          }
      }
    }
    
    System.out.println(game.toJSON())
    new Visualizer("t", "mu(t)", "Flow").plotData(xs, names)
    new Visualizer("t", "mu(t)", "Latency").plotData(ls, names)
    new Visualizer("t", "mu(t)", "Average Latency").plotData(avgLs, names)
    
    if(singleStrategyPlot){
      val strategyVis = new Visualizer("", "", "Strategies")
      (0 to K-1).toList.foldLeft(strategyVis)((vis, k)=>vis.plotStrategies(xs(k)/totalFlows(k), true))
    }else{
      for (k <- 0 to K-1)
        new Visualizer("", "", "Strategies").plotStrategies(xs(k)/totalFlows(k), true)
    }
      
  }
}