package routing

import mw._
import breeze.linalg.DenseVector

abstract class AdaptiveRoutingLearningRate(maxLoss: Double, historySize: Int, commodityId: Int) extends LearningRate {
  protected var alpha = 1.0
  private val f: Int=>Double = t=>alpha
  
  // use a duck type here: all we need is that the state provides
  type RoutingGameState = {val pathLatencies: Array[DenseVector[Double]]; val pathFlows: Array[DenseVector[Double]]}  
  
  // a measure of how much progress is being made, and how much the system is oscillating.
  def updateHeuristic(pathLatencies: List[DenseVector[Double]], pathFlows: List[DenseVector[Double]]): Double
  
  private def lastStates(stateHistory: List[GameState], n: Int): List[RoutingGameState] = {
    def lastStatesAcc(stateHistory: List[GameState], acc: List[RoutingGameState], n: Int): List[RoutingGameState] = {
      if(n == 0)
        acc
      else stateHistory match {
        case (h: RoutingGameState) :: t => lastStatesAcc(t, h::acc, n-1)
        case _ => Nil
      }
    }
    lastStatesAcc(stateHistory, Nil, n)
  }
  
  def apply(stateHistory: List[GameState]) = {
    val states = lastStates(stateHistory, historySize)
    val latencies = states.map(_.pathLatencies(commodityId))
    val flows = states.map(_.pathFlows(commodityId))
    alpha = alpha*updateHeuristic(latencies, flows)
    f
  }
}

case class SimpleAdaptiveRoutingLearningRate(maxLoss: Double, commodityId: Int) 
  extends AdaptiveRoutingLearningRate(maxLoss, 3, commodityId) 
{
  def updateHeuristic(pathLatencies: List[DenseVector[Double]], pathFlows: List[DenseVector[Double]]): Double = (pathLatencies, pathFlows) match {
    case (Nil, _) | (_, Nil) => 1.0
    case _ => {
      val strategies = pathFlows.map(flows => flows/flows.sum).map(_.toArray)
      val oscillations = strategies.transpose.map(x => 
        if((x(0)-x(1))*(x(1)-x(2))>=0)
          0.0
        else
          math.min(math.abs(x(0) - x(1)),math.abs(x(1) - x(2))) 
        )
      if(oscillations.max > 0.001){
        .5
      } else {
        val meanLatencies = for((flow, lat)<-pathFlows.zip(pathLatencies)) yield flow.dot(lat)/(flow.sum)
        val minLatencies = pathLatencies.map(_.min)
        val maxLatency = pathLatencies.map(_.max).max
        val distances = for((meanLat, minLat) <- meanLatencies.zip(minLatencies)) yield (meanLat - minLat)/maxLatency
        1.0 + distances.min/10
      }
    }
  }
}


