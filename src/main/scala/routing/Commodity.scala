package routing

import mw._

case class Commodity(source: Int, sink: Int, demand: FlowDemand, epsilon: LearningRate, updateRule: UpdateRule, paths: Array[List[Int]]) {
  def toMWAlgorithm[T <: Expert](id: Int)(implicit idsToExpert: (Int, Int)=>T) = {
    val experts = for(pathId <- paths.indices) yield idsToExpert(id, pathId)
    new MWAlgorithm(epsilon, experts.toArray, updateRule)
  }
}