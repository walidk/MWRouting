package mw
import breeze.linalg._
import util.Visualizer

class ParallelRoutingSim(
    val latencyFunctions: Array[Double => Double],
    totalFlow: Double, 
    updateRule: UpdateRule,
    randomizedStart: Boolean) extends 
  RoutingGameSim(
    adj = Map(0->latencyFunctions.toList.map(lat => (1, lat)))+(1->Nil), 
    sourceSinkPairs = Array((0, 1)),
    totalFlows = Array(totalFlow),
    updateRule,
    randomizedStart) {
  override def runFor(T:  Int) {
    import Visualizer._ // for implicit conversions
    super.runFor(T)
    Visualizer.plotFunctions(latencyFunctions, (0, totalFlow), "f", "l(f)", "Latency functions", 100)
  }

}