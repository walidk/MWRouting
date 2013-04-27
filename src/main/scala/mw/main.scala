package mw

import breeze.linalg._
import util.Visualization._

object main {
  
  // here the column player responds optimally
  // we plot the average strategy
  def testZSGAdversarialConvergence() {
    val epsilon = .1
    val T = 1000
    
    // payoff matrix
    val A = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
    val game = new ZSGAdversarial(A)
    val actions = (0 to 3).map(new ZSGAdversarialRow(game, _)).toList
    val alg = new MWAlgorithm[ZSGAdversarial](0, epsilon, actions, game)

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
  def testZSGNoRegretConvergence() {
    val T = 2000
    
    // payoff matrix
    val A = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
    val game = new ZSGNoRegret(A)
    val rowActions = (0 to 3).map(new ZSGNoRegretRow(game, _)).toList
    val colActions = (0 to 2).map(new ZSGNoRegretCol(game, _)).toList
    val rowAlg = new MWAlgorithm[ZSGNoRegret](0, .1, rowActions, game)
    val colAlg = new MWAlgorithm[ZSGNoRegret](1, .2, colActions, game)

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
    
  }
    
  def testGraph() {
    val adjM = DenseMatrix((0, 1, 0, 1), (0, 0, 1, 0), (0, 0, 0, 1), (0, 0, 0, 0))
    val graph = DirectedGraph(adjM)
    
    println(graph)
    
  }
  
  
  def main(args: Array[String]): Unit = {
//    testZSGAdversarialConvergence()
    testZSGNoRegretConvergence()
//    testGraph()
  }
}