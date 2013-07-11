package routing

import mw.{Expert, MWAlgorithm}

class RoutingGameSimBase(graph: DirectedGraph) {
  protected def pathToString(edgeList: List[Int]): String = edgeList match {
    case Nil => ""
    case h :: Nil => { val edge = graph.edges(h); edge.from.id + "->" + edge.to.id }
    case h :: t => { val edge = graph.edges(h); edge.from.id + "->" + pathToString(t) }
  }

  implicit def routingExpertConstructor(commodityId: Int, pathId: Int) = RoutingExpert(commodityId, pathId)
  implicit def noRegretSocialRoutingExpertConstructor(commodityId: Int, pathId: Int) = NoRegretSocialRoutingExpert(commodityId, pathId)
  
  protected def MWAlgorithmsFromCommodities[T<:Expert](commodities: Array[Commodity])(implicit expertConstructor:(Int, Int)=>T) = {
    val algorithms = commodities.zipWithIndex.map({case (commodity, idx) => commodity.toMWAlgorithm(idx)(expertConstructor)})
    algorithms
  }

}