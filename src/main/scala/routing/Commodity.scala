package routing

import mw._

case class Commodity(source: Int, sink: Int, demand: FlowDemand, epsilon: LearningRate, updateRule: UpdateRule, paths: Array[List[Int]])