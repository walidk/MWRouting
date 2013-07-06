package routing

abstract class FlowDemand {
  def apply(): Double
  
  def +(thatDemand: FlowDemand): FlowDemand = { 
    val thisDemand = this
    new FlowDemand(){def apply() = thisDemand.apply()+thatDemand.apply()}
  }
  
  def *(a: Double): FlowDemand = {
    val thisDemand = this
    new FlowDemand(){def apply() = a*thisDemand.apply()}
  }
}

class TimeVaryingFlowDemand(val flowStream: Stream[Double]) extends FlowDemand {
  private val flowIterator = flowStream.iterator
  def apply() = flowIterator.next()
  
  def until(t: Int) = new TimeVaryingFlowDemand(flowStream.take(t))
  
  def then(other: TimeVaryingFlowDemand) = new TimeVaryingFlowDemand(flowStream#:::other.flowStream)
}

object TimeVaryingFlowDemand {
  def apply(flowStream: Stream[Double]) = new TimeVaryingFlowDemand(flowStream)
}


class ConstantFlowDemand(flow: Double)
  extends TimeVaryingFlowDemand(Stream.continually(flow))

object ConstantFlowDemand {
  def apply(flow: Double) = new ConstantFlowDemand(flow)
}
