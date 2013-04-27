package mw

import breeze.linalg._
import breeze.numerics._

class ZeroSumGame(val A: DenseMatrix[Double]) extends Nature {
  // A is the payoff matrix, assumed to be normalized (entries in [0, 1])
  // row minimizes, column maximizes
  val (nRows, nCols) = (A.rows, A.cols)
  var rounds = 0
  var bestColResponse = 0
  
  val cumulativeColStrategy = DenseVector.zeros[Double](nCols)
  val cumulativeRowStrategy = DenseVector.zeros[Double](nRows)
  
  def getAvgColStrategy() = cumulativeColStrategy/cumulativeColStrategy.norm(1)
  def getAvgRowStrategy() = cumulativeRowStrategy/cumulativeRowStrategy.norm(1)
  
  def computeOutcome(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    val outcome = x.t * (A * y)
    outcome(0)
  }
  
  def computeBestColResponse(rowStrategy: DenseVector[Double]): (Int, Double) = {
    // compute the best column response
    val payoffs = A.t * rowStrategy
    (payoffs.argmax, payoffs.max)
  }
  
  def computeBestRowResponse(colStrategy: DenseVector[Double]): (Int, Double) = {
    val payoffs = A * colStrategy
    (payoffs.argmin, payoffs.min)
  }
  
  def update(rowStrategy: DenseVector[Double]) {
    bestColResponse = computeBestColResponse(rowStrategy)._1
    rounds += 1
    // update the average strategies
    for(i <- 0 to nRows-1)
      cumulativeRowStrategy(i) += rowStrategy(i)
    
    cumulativeColStrategy(bestColResponse) += 1
  }
}

class ZeroSumGameAction(game: ZeroSumGame, row: Int) extends Action[ZeroSumGame](game){
  def nextLoss(): Double = {
    val colResponse = game.bestColResponse
    return game.A(row, colResponse)
  }
}
