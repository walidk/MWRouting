package mw
import util.linAlgebra._

class ZeroSumGame(val A: Array[Array[Double]]) extends Nature {
  // A is the payoff matrix, assumed to be normalized (entries in [0, 1])
  // row minimizes, column maximizes
  val nRows = A.length
  val nCols = A(0).length
  var rounds = 0
  var bestColResponse = 0
  
  val cumulativeColStrategy = new Array[Double](nCols)
  val cumulativeRowStrategy = new Array[Double](nRows)
  
  private def getNormalized(cumulativeStrategy: Array[Double]): Array[Double] = {
    val norm = cumulativeStrategy.sum
    cumulativeStrategy.map(_/norm)
  }
  
  def getAvgColStrategy() = getNormalized(cumulativeColStrategy)
  def getAvgRowStrategy() = getNormalized(cumulativeRowStrategy)
  
  def computeOutcome(x: Array[Double], y: Array[Double]): Double = {
    x.times(A).times(y.transp()).doubleValue
  }
  
  def computeBestColResponse(rowStrategy: Array[Double]): (Int, Double) = {
    // compute the best column response
    val ((_, argmax), max) = rowStrategy.times(A).argMax()
    (argmax, max)
  }
  
  def computeBestRowResponse(colStrategy: Array[Double]): (Int, Double) = {
    val ((_, argmin), min) = A.times(colStrategy.transp()).argMin()
    (argmin, min)
  }
  
  def update(rowStrategy: Array[Double]) {
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
    return game.A(row)(colResponse)
  }
}
