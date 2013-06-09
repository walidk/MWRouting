package mw
import breeze.linalg._
import util.Visualizer

class ZeroSumGameAdversarial(payoffMatrix: DenseMatrix[Double]) extends ZeroSumGame(payoffMatrix) {

  // The only thing that changes (compared to the parent class ZeroSumGame) 
  // is the fact that the column player is responding adversarially. 
  // So we override the update function to reflect that change
  override def update(state: State, strategies: Array[DenseVector[Double]]): GameState = {
    val rowStrategy = strategies(0)
    val bestColResponse = computeBestColResponse(rowStrategy)
    GameState(rowStrategy, bestColResponse)
  }
}

class ZeroSumGameAdversarialSim(
  payoffMatrix: DenseMatrix[Double],
  updateRule: UpdateRule,
  randomizedStart: Boolean) {
  val defaultEpsilon: Int => Double = t => .1
  val game = new ZeroSumGameAdversarial(payoffMatrix)
  val nbRows = payoffMatrix.rows

  val rowExperts: Array[Expert] = (0 to nbRows - 1).map(ZeroSumGameRowExpert(_)).toArray
  val algorithms = Array(new MWAlgorithm(defaultEpsilon, rowExperts, updateRule))
  val legend = Array((0 to nbRows - 1).map("row " + _.toString).toArray)
  val coordinator = new MWCoordinator(game, algorithms, randomizedStart)

  val strategies = coordinator.strategiesStream
  val averageStrategies = coordinator.averageStrategiesStream
  val losses = coordinator.lossStream
  val averageLosses = coordinator.averageLossStream

  def runFor(T: Int) {
    Visualizer.plotStrategies(strategies.take(T))
    Visualizer.plotLineGroups(strategies.take(T), "t", "mu(t)", "Strategies", legend)
  }
}