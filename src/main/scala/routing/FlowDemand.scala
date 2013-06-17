package routing

abstract class FlowDemand {
  def apply(): Double
}

case class ConstantFlowDemand(flow: Double) extends FlowDemand {
  def apply() = flow
}

case class TimeVaryingFlowDemand(flow: Iterator[Double]) extends FlowDemand {
  def apply() = flow.next()
}