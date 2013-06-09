package mw

import breeze.linalg._
import util.Visualizer

class ZeroSumGame(PayoffMatrix: DenseMatrix[Double]) extends Nature {
  case class GameState(
      rowStrategy: DenseVector[Double], 
      colStrategy: DenseVector[Double])
  
  type State = GameState
  
  val (nRows, nCols) = (PayoffMatrix.rows, PayoffMatrix.cols)
  
  def computeOutcome(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    x dot (PayoffMatrix * y)
  }
  
  def computeBestColResponse(rowStrategy: DenseVector[Double]): DenseVector[Double] = {
    // compute the best column response
    val payoffs = PayoffMatrix.t * rowStrategy
    val resp = DenseVector.zeros[Double](nCols)
    resp(payoffs.argmax) = 1
    resp
  }
  
  def computeBestRowResponse(colStrategy: DenseVector[Double]): DenseVector[Double] = {
    // compute the best column response
    val payoffs = PayoffMatrix * colStrategy
    val resp = DenseVector.zeros[Double](nRows)
    resp(payoffs.argmin) = 1
    resp
  }
  
  def getDelta(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    math.max(
        computeOutcome(x, computeBestColResponse(x)) - computeOutcome(x, y),
        computeOutcome(x, y) - computeOutcome(computeBestRowResponse(y), y))
  }
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    GameState(strategies(0), strategies(1))
  }
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case ZeroSumGameRowExpert(row) => {
      val pureStrat = DenseVector.zeros[Double](nRows)
      pureStrat(row) = 1.
      computeOutcome(pureStrat, state.colStrategy)
    }
    case ZeroSumGameColExpert(col) => {
      val pureStrat = DenseVector.zeros[Double](nCols)
      pureStrat(col) = 1.
      -computeOutcome(state.rowStrategy, pureStrat)
    }
  }
}

case class ZeroSumGameRowExpert(row: Int) extends Expert
case class ZeroSumGameColExpert(col: Int) extends Expert

class ZeroSumGameSim(
    payoffMatrix: DenseMatrix[Double],
    updateRule: UpdateRule,
    randomizedStart: Boolean){
  
  private val defaultEpsilon = (t:Int) => 10. / (10 + t)
  private val game = new ZeroSumGame(payoffMatrix)
  private val (nRows, nCols) = (game.nRows, game.nCols)
  private val rowExperts: Array[Expert] = (0 to nRows-1).map(ZeroSumGameRowExpert(_)).toArray
  private val colExperts: Array[Expert] = (0 to nCols-1).map(ZeroSumGameColExpert(_)).toArray
  val algorithms = new Array[MWAlgorithm](2) 
  algorithms(0) = new MWAlgorithm(defaultEpsilon, rowExperts, updateRule)
  algorithms(1) = new MWAlgorithm(defaultEpsilon, colExperts, updateRule)
    
  val legend = 
    Array(
      (0 to nRows-1).map("row " + _.toString).toArray,
      (0 to nCols-1).map("column " + _.toString).toArray
    )

  val coordinator = new MWCoordinator(game, algorithms, randomizedStart)
  val strategies = coordinator.strategiesStream
  val averageStrategies = coordinator.averageStrategiesStream
  val losses = coordinator.lossStream
  val averageLosses = coordinator.averageLossStream
  val deltas = averageStrategies.map(s => coordinator.nature.getDelta(s(0), s(1)))
  
  def runFor(T: Int) {
    Visualizer.plotStrategies(strategies.take(T))
    Visualizer.plotLineGroups(strategies.take(T), "t", "mu(t)", "Strategies", legend)
    Visualizer.plotLineGroups(averageStrategies.take(T), "t", "mu(t)", "Average strategies", legend)
    Visualizer.plotLineGroups(averageLosses.take(T), "t", "mu(t)", "Average strategy losses", legend)
    Visualizer.plotLine(deltas.take(T), "t", "mu(t)", "Deltas of average strategies")
  }
}

