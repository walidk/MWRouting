package routing

class GaussianNoise(sigmas: Stream[Double]) extends LatencyFunction with FlowDemand {
  val rand = new scala.util.Random()
  val sigmaIter = sigmas.iterator
  def apply(x: Double) = apply()
  def apply() = sigmaIter.next()*rand.nextGaussian()
  override def *(a: Double) = new GaussianNoise(sigmas.map(_*a))
}

object GaussianNoise{
  def apply(sigma: Double = 1) = new GaussianNoise(Stream.continually(sigma))
  def apply(sigmaStream: Stream[Double]) = new GaussianNoise(sigmaStream)
}