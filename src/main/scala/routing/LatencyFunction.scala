package routing

abstract class LatencyFunction { 
  def apply(x: Double): Double
  
  def +(thatLatency: LatencyFunction) = { 
    val thisLatency = this
    new LatencyFunction(){def apply(x: Double) = thisLatency.apply(x)+thatLatency.apply(x)}
  }
  
  def *(a: Double) = 
    new LatencyFunction(){def apply(x: Double) = a*this.apply(x)}
}

class GaussianNoise(sigmas: Stream[Double]) extends LatencyFunction {
  val rand = new scala.util.Random()
  val sigmaIter = sigmas.iterator
  def apply(x: Double) = sigmaIter.next()*rand.nextGaussian()
}
object GaussianNoise{
  def apply(sigma: Double = 1) = new GaussianNoise(Stream.continually(sigma))
  def apply(sigmaStream: Stream[Double]) = new GaussianNoise(sigmaStream)
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
  