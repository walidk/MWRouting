package routing

abstract class LatencyFunction { 
  def apply(x: Double): Double
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
  