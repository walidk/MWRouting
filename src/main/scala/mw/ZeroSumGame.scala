package mw

import breeze.linalg._

abstract class ZeroSumGame(val A: DenseMatrix[Double]) extends Nature {
  // A is the payoff matrix, assumed to be normalized (entries in [0, 1])
  // row minimizes, column maximizes
  val (nRows, nCols) = (A.rows, A.cols)
  
  val cumulativeColStrategy = DenseVector.zeros[Double](nCols)
  val cumulativeRowStrategy = DenseVector.zeros[Double](nRows)
  
  def getAvgColStrategy() = cumulativeColStrategy/cumulativeColStrategy.norm(1)
  def getAvgRowStrategy() = cumulativeRowStrategy/cumulativeRowStrategy.norm(1)
  
  def computeOutcome(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    val outcome = x.t * (A * y)
    outcome(0)
  }
  
  def computeBestColResponse(rowStrategy: DenseVector[Double]): DenseVector[Double] = {
    // compute the best column response
    val payoffs = A.t * rowStrategy
    val resp = DenseVector.zeros[Double](nCols)
    resp(payoffs.argmax) = 1
    resp
  }
  
  def computeBestRowResponse(colStrategy: DenseVector[Double]): DenseVector[Double] = {
    // compute the best column response
    val payoffs = A * colStrategy
    val resp = DenseVector.zeros[Double](nRows)
    resp(payoffs.argmax) = 1
    resp
  }
  
}

class ZSGAdversarial(A: DenseMatrix[Double]) extends ZeroSumGame(A) {
  var bestColResponse: DenseVector[Double] = DenseVector.fill[Double](nCols){1./nCols}
  
  def update(id: Int, rowStrategy: DenseVector[Double]) {
    bestColResponse = computeBestColResponse(rowStrategy)
    // update the average strategies
    cumulativeRowStrategy += rowStrategy
    cumulativeColStrategy += bestColResponse
  }
}

class ZSGNoRegret(A: DenseMatrix[Double]) extends ZeroSumGame(A) {
  var colStrategy: DenseVector[Double] = DenseVector.fill[Double](nCols){1./nCols}
  var rowStrategy: DenseVector[Double] = DenseVector.fill[Double](nRows){1./nRows}
  
  def update(id: Int, strategy: DenseVector[Double]) {
    if(id == 0){ // row is playing
      cumulativeRowStrategy += strategy
      rowStrategy = strategy
    }else if(id == 1){
      cumulativeColStrategy += strategy
      colStrategy = strategy
    }
    
  }
}

class ZSGNoRegretRow(game: ZSGNoRegret, row: Int) extends Expert[ZSGNoRegret](game){
  val pureStrat = DenseVector.zeros[Double](game.nRows)
  pureStrat(row) = 1.
  def nextLoss(): Double = {
    game.computeOutcome(pureStrat, game.colStrategy)
  }
}

class ZSGNoRegretCol(game: ZSGNoRegret, col: Int) extends Expert[ZSGNoRegret](game){
  val pureStrat = DenseVector.zeros[Double](game.nCols)
  pureStrat(col) = 1.
  def nextLoss(): Double = {
    -game.computeOutcome(game.rowStrategy, pureStrat)
  }
}

class ZSGAdversarialRow(game: ZSGAdversarial, row: Int) extends Expert[ZSGAdversarial](game){
  val pureStrat = DenseVector.zeros[Double](game.nRows)
  pureStrat(row) = 1.
  def nextLoss(): Double = {
    game.computeOutcome(pureStrat, game.bestColResponse)
  }
}
