package mw

import breeze.linalg._
import util.Visualization._

object main {
  def main(args: Array[String]): Unit = {
//    ZSGConvergenceTest.launchAdversarial()
//    ZSGConvergenceTest.launchNoRegret()
    GraphTest.launch()
  }
}


object GraphTest {
  def launch() {
    val adj: List[List[(Int, Double=>Double)]] = 
      List(
          List((1, x=>x), (3, x=> 1.)), 
          List((2, x=> x)), 
          List((3, x=>0.)), 
          List()
          )
    val graph = DirectedGraph(adj)
    
    println(graph)
    
  }
}

object ZSGConvergenceTest {
  // here the column player responds optimally
  // we plot the average strategy
  def launchAdversarial() {
    val T = 1000
    
    // payoff matrix
    val A = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
    val game = new ZSGAdversarial(A)
    val actions = (0 to 3).map(new ZSGAdversarialRow(game, _)).toList
    val alg = new MWAlgorithm[ZSGAdversarial](0, t=>.1, actions, game)

    val xs = DenseMatrix.zeros[Double](4, T)
    val deltas = DenseMatrix.zeros[Double](1, T)
    
    for (t <- 0 to T-1) {
      alg.next()
      val y = game.getAvgColStrategy()
      val x = game.getAvgRowStrategy()
//      val x = alg.strategy
      val delta = math.max(
        game.computeOutcome(x, game.computeBestColResponse(x)) - game.computeOutcome(x, y),
        game.computeOutcome(x, y) - game.computeOutcome(game.computeBestRowResponse(y), y))
      
      deltas(0, t) = delta
      xs(::, t) := x
      
    }
    printData(xs, "t", "mu(t)", "Average row strategy")
    printData(deltas, "t", "mu(t)", "delta")
  }
  
  
  // here both players use Multiplicative weights
  // we plot the average strategy
  def launchNoRegret() {
    val T = 3000
    val dec_eps: Int=>Double = t=>1./(2+t)
    val eps: Int=>Double = t=>.1
    
    // payoff matrix
    val A = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
    val game = new ZSGNoRegret(A)
    val rowActions = (0 to 3).map(new ZSGNoRegretRow(game, _)).toList
    val colActions = (0 to 2).map(new ZSGNoRegretCol(game, _)).toList
    val rowAlg = new MWAlgorithm[ZSGNoRegret](0, eps, rowActions, game)
    val colAlg = new MWAlgorithm[ZSGNoRegret](1, eps, colActions, game)

    val xs = DenseMatrix.zeros[Double](4, T)
    val ys = DenseMatrix.zeros[Double](3, T)
    val deltas = DenseMatrix.zeros[Double](1, T)
    
    for (t <- 0 to T-1) {
      rowAlg.next()
      colAlg.next()
//      val x = rowAlg.strategy
//      val y = colAlg.strategy
      val x = game.getAvgRowStrategy
      val y = game.getAvgColStrategy
      val delta = math.max(
        game.computeOutcome(x, game.computeBestColResponse(x)) - game.computeOutcome(x, y),
        game.computeOutcome(x, y) - game.computeOutcome(game.computeBestRowResponse(y), y))
      deltas(0, t) = delta
      xs(::, t) := x
      ys(::, t) := y
      
    }
    printData(xs, "t", "mu(t)", "avg row strategy")
    printData(ys, "t", "mu(t)", "avg col strategy")
    printData(deltas, "t", "mu(t)", "delta")
    println(deltas(0, T-1))
  }
}