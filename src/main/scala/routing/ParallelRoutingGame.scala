package routing

import scala.collection.mutable.HashMap
import util.Visualizer

class ParallelRoutingGameSim(
    graph: DirectedGraph,
    latencyFunctions: HashMap[Int, LatencyFunction],
    commodities: Array[Commodity],
    randomizedStart: Boolean) extends RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart){

  override def runFor(T: Int){
//    Visualizer.plotFunctions(latencyFunctions.values.map(), (0, ), "f", "l(f)", "Latency Functions", 300)
    super.runFor(T)
  }
}