package mw

import breeze.linalg.DenseVector

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

abstract class FlowAdaptiveLearningRate(maxLoss: Double, historySize: Int, commodityId: Int) extends LearningRate {
  private var alpha = 1.
  private val f: Int=>Double = t=>alpha*10./(10.+t)/maxLoss
  
  // use a duck type here: all we need is that the state provides
  type FlowState = {val pathLatencies: Array[DenseVector[Double]]}  
  
  // a measure of how much progress is being made, and how much the system is oscillating.
  def updateHeuristic(pathLatencies: List[DenseVector[Double]]): Double
  
  private def latencyHisory(stateHistory: List[GameState], n: Int): List[DenseVector[Double]] = {
    if(n == 0)
      Nil
    else stateHistory match {
      case (h:FlowState) :: t => h.pathLatencies(commodityId)::latencyHisory(t, n-1)
      case _ => Nil
    }
  }
  
  def apply(stateHistory: List[GameState]) = {
    val lastLatencies = latencyHisory(stateHistory, historySize)
    alpha = alpha*updateHeuristic(lastLatencies)
    f
  }
}

case class SimpleFlowAdaptiveLearningRate(maxLoss: Double, historySize: Int, commodityId: Int) 
  extends FlowAdaptiveLearningRate(maxLoss, historySize, commodityId) 
{
  def updateHeuristic(pathLatencies: List[DenseVector[Double]]): Double = {
    
    1.
  }  
}


