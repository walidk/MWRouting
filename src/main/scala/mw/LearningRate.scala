package mw

abstract class LearningRate {
  def apply(stateHistory: List[GameState]): Int => Double
}

case class ConstantLearningRate(maxLoss: Double) extends LearningRate {
  private val f: Int=>Double = t=>1./maxLoss
  def apply(stateHistory: List[GameState]) = f 
}

case class HarmonicLearningRate(maxLoss: Double) extends LearningRate {
  private val f: Int=>Double = t=>10./(10.+t)/maxLoss
  def apply(stateHistory: List[GameState]) = f
}