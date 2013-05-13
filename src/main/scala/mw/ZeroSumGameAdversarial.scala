package mw
import breeze.linalg._
import util.Visualizer

class ZeroSumGameAdversarial(A: DenseMatrix[Double]) extends ZSG(A) {
  var bestColResponse: DenseVector[Double] = DenseVector.fill[Double](nCols) { 1. / nCols }

  def update(id: Int, rowStrategy: DenseVector[Double]) {
    bestColResponse = computeBestColResponse(rowStrategy)
    // update the average strategies
    cumulativeRowStrategy += rowStrategy
    cumulativeColStrategy += bestColResponse
  }
}

class ZeroSumGameAdversarialRowExpert(game: ZeroSumGameAdversarial, row: Int) extends Expert[ZeroSumGameAdversarial](game) {
  val pureStrat = DenseVector.zeros[Double](game.nRows)
  pureStrat(row) = 1.
  def nextLoss(): Double = {
    game.computeOutcome(pureStrat, game.bestColResponse)
  }
}

// average specifies whether we should plot the average strategy of the last strategy
class ZeroSumGameAdversarialSim(
    A: DenseMatrix[Double], 
    average: Boolean,
    randomize: Boolean) {
  var eps: Int => Double = t => .1
  val game = new ZeroSumGameAdversarial(A)
  val nbRows = A.rows

  def launch(T: Int) {
    val experts = (0 to nbRows - 1).map(new ZeroSumGameAdversarialRowExpert(game, _)).toList
    val alg = new MWAlgorithm[ZeroSumGameAdversarial](0, eps, experts, game, randomize)

    val xs = DenseMatrix.zeros[Double](nbRows, T)
    val deltas = DenseMatrix.zeros[Double](1, T)
    val rowNames = (0 to nbRows-1).map("row " + _.toString).toArray
    
    for (t <- 0 to T - 1) {
      alg.next()
      val x = if(average) game.getAvgRowStrategy() else alg.strategy
      val y = if(average) game.getAvgColStrategy() else game.bestColResponse 
      deltas(0, t) = game.getDelta(x, y)
      xs(::, t) := x
    }
    new Visualizer("t", "mu(t)", "Average row strategy").plotData(xs, rowNames)
    new Visualizer("t", "mu(t)", "delta").plotData(deltas, Array("delta"))
    new Visualizer("", "", "Strategies").plotStrategies(xs, true)
  }
}