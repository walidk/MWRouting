package mw

object main {
  def testZeroSumConvergence() {
    val epsilon = .1
    val T = 1000
    
    // payoff matrix
    val A = Array[Array[Double]](
      Array[Double](.5, 1, 0),
      Array[Double](0, .5, 1),
      Array[Double](1, 0, .5),
      Array[Double](.5, 0, 0))
    val game = new ZeroSumGame(A)
    val actions = (0 to 3).map(new ZeroSumGameAction(game, _)).toList
    val alg = new MWAlgorithm[ZeroSumGame](epsilon, actions, game)

    for (t <- 1 to T) {
      alg.next()
      val y = game.getAvgColStrategy()
      //    val x = game.getAvgRowStrategy()
      val x = alg.strategy
      val delta = math.max(
        game.computeBestColResponse(x)._2 - game.computeOutcome(x, y),
        game.computeOutcome(x, y) - game.computeBestRowResponse(y)._2)
      println(delta)
      println(x.toList)
    }
    
  }
  
  
  def main(args: Array[String]): Unit = {
    testZeroSumConvergence()
  }
}