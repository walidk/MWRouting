package mw
import breeze.linalg._
import util.Visualizer

class ParallelNetwork(val size: Int, latencies: Array[Double => Double]) {
  def getLatency(flows: DenseVector[Double])(path: Int): Double = 
    latencies(path)(flows(path))
}

class ParallelRoutingGame(totalFlow: Double, network: ParallelNetwork) extends Nature {
  var flows: DenseVector[Double] = DenseVector.zeros[Double](network.size)
  val size = network.size
  def getLatency(path: Int) = network.getLatency(flows)(path)
  
  def getLatencies() = new DenseVector[Double]((0 to size-1).map(getLatency).toArray)
  
  def getDelta(flows: DenseVector[Double]) = {
    val latencies = (0 to size-1).map(getLatency)
    latencies.max - latencies.min 
  }
  
  def update(id: Int, strategy: DenseVector[Double]) = {
    flows = strategy * totalFlow
  }
  
}

class ParallelRoutingExpert(game: ParallelRoutingGame, path: Int) extends Expert[ParallelRoutingGame](game) {
  def nextLoss(): Double = {
    game.getLatency(path)
  }
}

class ParallelRoutingSim(size: Int, totalFlow: Double, val latencies: Array[Double => Double]) {
  var eps: Int => Double = t => .1
  val network = new ParallelNetwork(size, latencies)
    
  def launch(T: Int) {
    val game = new ParallelRoutingGame(totalFlow, network)
    val pathExperts = (0 to size-1).map(new ParallelRoutingExpert(game, _)).toList
    val alg = new MWAlgorithm[ParallelRoutingGame](0, eps, pathExperts, game)

    val xs = DenseMatrix.zeros[Double](size, T)
    val ls = DenseMatrix.zeros[Double](size, T)

    for (t <- 0 to T - 1) {
      alg.next()
      val x = game.flows
      val l = game.getLatencies
      //      deltas(0, t) = game.getDelta(x)
      xs(::, t) := x
      ls(::, t) := l
    }
    
    new Visualizer("t", "mu(t)", "Flow").plotData(xs)
    new Visualizer("t", "mu(t)", "Latency").plotData(ls)
    new Visualizer("", "", "Strategies").plotStrategies(xs)
    
    val latxs = new DenseVector((0 to size-1).map(game.flows(_)).toArray)
    val latys = new DenseVector((0 to size-1).map(game.getLatency(_)).toArray)
    new Visualizer("f", "l(t)", "Latency functions")
      .plotFunctions(latencies, (0, totalFlow))
      .plotPoints(latxs, latys)

      
  }
}