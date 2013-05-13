package mw
import breeze.linalg._
import util.Visualizer

class ParallelRoutingSim(
    totalFlow: Double, 
    val latencies: Array[Double => Double],
    randomize: Boolean) {
  val size = latencies.size
  var eps: Int => Double = t => .1
  val adj = Map(0->latencies.toList.map(lat => (1, lat)))+(1->Nil)
  
  val graph = DirectedGraph.fromAdjacencyMap(adj)
  val network = new Network(graph, Array((0, 1)))
  
  def launch(T: Int) {
    val game = new RoutingGame(Array(totalFlow), network)
    val experts = (0 to size-1).map(new RoutingExpert(game, 0, _)).toList

    val alg = new MWAlgorithm[RoutingGame](0, eps, experts, game, randomize)

    // simulation
    val xs = DenseMatrix.zeros[Double](size, T)
    val ls = DenseMatrix.zeros[Double](size, T)
    val xNames = (0 to size - 1).map("path "+_).toArray
    
    for (t <- 0 to T - 1) {
      alg.next()
      xs(::, t) := game.pathFlows(0)
      ls(::, t) := game.getLatencies(0)
    }
    new Visualizer("t", "mu(t)", "Flow").plotData(xs, xNames)
    new Visualizer("t", "mu(t)", "Latency").plotData(ls, xNames)
    new Visualizer("", "", "Strategies").plotStrategies(xs/totalFlow, true)
      
    val latxs = new DenseVector((0 to size-1).map(game.pathFlows(0)(_)).toArray)
    val latys = new DenseVector((0 to size-1).map(game.getLatency(0)(_)).toArray)
    new Visualizer("f", "l(t)", "Latency functions")
      .plotFunctions(latencies, (0, totalFlow), xNames)
      .plotPoints(latxs, latys)
  }
}