package mw

import scala.collection.immutable.Map
import breeze.linalg._
import util.Visualizer

object main {
  def main(args: Array[String]): Unit = {
    Sims.launchGraphTest(200)
//    Sims.launchParallelGraphTest(200)
//    Sims.launchZeroSumGame(2000)
//    Sims.launchZeroSumGameAdversarial(200)
  }
}

object Sims {
  def launchParallelGraphTest(T: Int) {
    val latencies: Array[Double => Double] = 
      Array(
          x => 3 * x, 
          x => 2 + 2 * x, 
          x => x * x, 
          x => 2 * (x + 1) * (x + 1) - 1)
    val totalFlow = 2.
    val size = 4
    val randomize = true
    val sim = new ParallelRoutingSim(size, totalFlow, latencies, randomize)
    sim.launch(T)
  }

  def launchGraphTest(T: Int) {
    val adj: Map[Int, List[(Int, Double => Double)]] = Map(
      0 -> List((1, x => x * x+.5), (4, x => x / 2)),
      1 -> List(),
      2 -> List((3, x => x), (4, x => .5)),
      3 -> List(),
      4 -> List((5, x => 3 * x * x), (6, x => x*x*x)),
      5 -> List((1, x => x / 3), (3, x => x/4)), 
      6 -> List((1, x => x*x / 2), (3, x => x)))
    val sourceSinks = Array((0, 1), (2, 3))
    val totalFlows = Array(1., 1.)
    val randomize = true
    
    val sim = new RoutingGameSim(adj, sourceSinks, totalFlows, randomize)
    sim.eps(0) = t=>.2
    sim.eps(1) = t=>.2
//    sim.eps(0) = t=>100./(10+t)
//    sim.eps(1) = t=>100./(10+t)
    sim.launch(T)
  }

  
  // Zero Sum Games
  val A = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
  // here the column player responds optimally
  // we plot the average strategy
  def launchZeroSumGame(T: Int) {
    val randomize = true
    val sim = new ZeroSumGameSim(A, true, randomize)
    sim.eps(0) = t=>1./(1+t)
    sim.eps(1) = t=>1./(1+t)
    sim.launch(T)
  }

  def launchZeroSumGameAdversarial(T: Int) {
    val randomize = true
    val sim = new ZeroSumGameAdversarialSim(A, true, randomize)
    sim.eps = t=>10./(10+t)
    sim.launch(T)
  }
}
