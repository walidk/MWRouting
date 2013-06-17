package mw

import breeze.linalg._

/** Abstract class, used to specify which update rule to use in the 
 *  Multiplicative Weights algorithm.
 */
abstract class UpdateRule {
  val nextWeights: (DenseVector[Double], DenseVector[Double], Double)=>DenseVector[Double]
}

case class ExponentialUpdate() extends UpdateRule {
  val nextWeights = (strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) => 
    losses.map(loss => math.exp(-epsilon*loss))
}
case class PolyUpdate(exponent: Double) extends UpdateRule {
  val nextWeights = (strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) => 
    losses.map(loss => epsilon/math.pow(loss, exponent))
}
case class FollowTheMeanUpdate() extends UpdateRule {
  val nextWeights = (strategy: DenseVector[Double], losses: DenseVector[Double], epsilon: Double) => {
    val avgLoss = sum(strategy :* losses)
    losses.map(loss => 1+epsilon*(avgLoss - loss))
  }
}