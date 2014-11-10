package routing

trait LatencyFunction {  
  def apply(x: Double): Double
  
  def derivative: LatencyFunction = {
    val thisLatency = this
    val epsilon = 1E-9
    new LatencyFunction(){def apply(x: Double) = (thisLatency.apply(x+epsilon) - thisLatency.apply(x))/epsilon}
  }
  
  def antiDerivative: LatencyFunction = {
    val thisLatency = this
    val intervals = 150
    new LatencyFunction(){def apply(x: Double) = (1 to intervals).map(i => thisLatency(x*(i-.5)/intervals)*x/intervals).sum}
  }
  
  def +(thatLatency: LatencyFunction): LatencyFunction = { 
    val thisLatency = this
    new LatencyFunction(){def apply(x: Double) = thisLatency.apply(x)+thatLatency.apply(x)}
  }
  
  def *(a: Double): LatencyFunction = this * StaticLatencyFunction(x=>a)
  
  def *(thatLatency: LatencyFunction): LatencyFunction = {
    val thisLatency = this
    new LatencyFunction(){def apply(x: Double) = thisLatency(x)*thatLatency(x)}
  }
}


class TimeDependentLatencyFunction(val latencyStream: Stream[Double=>Double]) extends LatencyFunction{
  private val latencyIterator = latencyStream.iterator 
  override def apply(x: Double) = {
    val lat = latencyIterator.next()
    lat(x)
  }
  
  def until(t: Int): TimeDependentLatencyFunction = { 
    return new TimeDependentLatencyFunction(latencyStream.take(t))
  }
  def then(other: TimeDependentLatencyFunction) = 
    new TimeDependentLatencyFunction(latencyStream#:::other.latencyStream)
}

object TimeDependentLatencyFunction {
  def apply(latencyStream: Stream[Double=>Double]) = new TimeDependentLatencyFunction(latencyStream) 
}

class StaticLatencyFunction(val latency: Double=>Double) extends TimeDependentLatencyFunction(Stream.continually(latency))

object StaticLatencyFunction {
  def apply(latency: Double=>Double) = new StaticLatencyFunction(latency) 
  
}

// shorthand for the StaticLatencyFunction class, because this is used a lot
object SLF{
  def apply(lat: Double=>Double) = StaticLatencyFunction(lat)
}
  