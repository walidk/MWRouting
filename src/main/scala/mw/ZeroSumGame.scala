//package mw
//
//import breeze.linalg._
//import util.Visualizer
//
//
//abstract class ZSG(val A: DenseMatrix[Double]) extends Nature {
//  // A is the payoff matrix, assumed to be normalized (entries in [0, 1])
//  // row minimizes, column maximizes
//  val (nRows, nCols) = (A.rows, A.cols)
//  
//  val cumulativeColStrategy = DenseVector.zeros[Double](nCols)
//  val cumulativeRowStrategy = DenseVector.zeros[Double](nRows)
//  
//  def getAvgColStrategy() = cumulativeColStrategy/cumulativeColStrategy.norm(1)
//  def getAvgRowStrategy() = cumulativeRowStrategy/cumulativeRowStrategy.norm(1)
//  
//  def computeOutcome(x: DenseVector[Double], y: DenseVector[Double]): Double = {
//    val outcome = x.t * (A * y)
//    outcome(0)
//  }
//  
//  def computeBestColResponse(rowStrategy: DenseVector[Double]): DenseVector[Double] = {
//    // compute the best column response
//    val payoffs = A.t * rowStrategy
//    val resp = DenseVector.zeros[Double](nCols)
//    resp(payoffs.argmax) = 1
//    resp
//  }
//  
//  def computeBestRowResponse(colStrategy: DenseVector[Double]): DenseVector[Double] = {
//    // compute the best column response
//    val payoffs = A * colStrategy
//    val resp = DenseVector.zeros[Double](nRows)
//    resp(payoffs.argmin) = 1
//    resp
//  }
//  
//  def getDelta(x: DenseVector[Double], y: DenseVector[Double]): Double = {
//    math.max(
//        computeOutcome(x, computeBestColResponse(x)) - computeOutcome(x, y),
//        computeOutcome(x, y) - computeOutcome(computeBestRowResponse(y), y))
//  }
//}
//
//
//
//class ZeroSumGame(A: DenseMatrix[Double]) extends ZSG(A) {
//  var colStrategy: DenseVector[Double] = DenseVector.fill[Double](nCols){1./nCols}
//  var rowStrategy: DenseVector[Double] = DenseVector.fill[Double](nRows){1./nRows}
//  
//  def update(id: Int, strategy: DenseVector[Double]) {
//    if(id == 0){ // row is playing
//      cumulativeRowStrategy += strategy
//      rowStrategy = strategy
//    }else if(id == 1){
//      cumulativeColStrategy += strategy
//      colStrategy = strategy
//    }
//    
//  }
//}
//
//class ZeroSumGameRowExpert(game: ZeroSumGame, row: Int) extends Expert[ZeroSumGame](game){
//  val pureStrat = DenseVector.zeros[Double](game.nRows)
//  pureStrat(row) = 1.
//  def nextLoss(): Double = {
//    game.computeOutcome(pureStrat, game.colStrategy)
//  }
//}
//
//class ZeroSumGameColExpert(game: ZeroSumGame, col: Int) extends Expert[ZeroSumGame](game){
//  val pureStrat = DenseVector.zeros[Double](game.nCols)
//  pureStrat(col) = 1.
//  def nextLoss(): Double = {
//    -game.computeOutcome(game.rowStrategy, pureStrat)
//  }
//}
//
//class ZeroSumGameSim(
//    A: DenseMatrix[Double], 
//    average: Boolean,
//    randomize: Boolean){
//  val eps: Array[Int => Double] = Array(t => .1, t => .1)
//  val (nbRows, nbCols) = (A.rows, A.cols)
//  val game = new ZeroSumGame(A)
//  
//  def launch(T: Int) {
//    val rowExperts = (0 to nbRows-1).map(new ZeroSumGameRowExpert(game, _)).toList
//    val colExperts = (0 to nbCols-1).map(new ZeroSumGameColExpert(game, _)).toList
//    val rowAlg = new ExponentialMWAlgorithm[ZeroSumGame](0, eps(0), rowExperts, game, randomize)
//    val colAlg = new ExponentialMWAlgorithm[ZeroSumGame](1, eps(1), colExperts, game, randomize)
//
//    val xs = DenseMatrix.zeros[Double](nbRows, T)
//    val ys = DenseMatrix.zeros[Double](nbCols, T)
//    val rowNames = (0 to nbRows-1).map("row " + _.toString).toArray
//    val colNames = (0 to nbCols-1).map("column " + _.toString).toArray
//    val deltas = DenseMatrix.zeros[Double](1, T)
//
//    for (t <- 0 to T - 1) {
//      rowAlg.next()
//      colAlg.next()
//      val x = if(average) game.getAvgRowStrategy else rowAlg.strategy
//      val y = if(average) game.getAvgColStrategy else colAlg.strategy
//      deltas(0, t) = game.getDelta(x, y)
//      xs(::, t) := x
//      ys(::, t) := y
//    }
//
//    new Visualizer("t", "mu(t)", "row strategy").plotData(xs, rowNames)
//    new Visualizer("t", "mu(t)", "col strategy").plotData(ys, colNames)
//    new Visualizer("", "", "row strategy").plotStrategies(xs)
//    new Visualizer("", "", "col strategy").plotStrategies(ys)
//    
//    new Visualizer("t", "mu(t)", "delta").plotData(deltas, Array("delta"))
//    println(deltas(0, T - 1))
//  }
//}
//
