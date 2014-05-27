package routing

import mw._
import breeze.linalg.DenseVector
import scala.collection.mutable.HashMap
import util.Visualizer

class ConstantTollRoutingGame(network: LatencyNetwork, pathTolls: Array[DenseVector[Double]]) extends RoutingGame(network) {
  override def update(state: State, strategies: Array[DenseVector[Double]]): State = {
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val pathLosses = for ((tolls, latencies) <- pathTolls.zip(pathLatencies)) yield tolls + latencies
    NetworkState(pathFlows, pathLosses)
  }
}

class OptimalConstantTollRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  randomizedStart: Boolean) extends RoutingGameSimBase(graph) {

  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val optimizer = new SocialOptimizer(network)
  private val optimalStrategy = optimizer.optimalStrategy
  private val optimalTollsRoughgarden = network.pathTollsFromPathFlows(network.pathFlowsFromStrategies(optimalStrategy))
  private val optimalLatencies = network.pathLatenciesFromPathFlows(network.pathFlowsFromStrategies(optimalStrategy))
  private val optimalTollsBlandin = optimalLatencies.map(lat => -lat + lat.min)

  def runFor(T: Int) {
    def runConstantTolls(constantTolls: Array[DenseVector[Double]], title: String) {
      val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
      val legend = commodities.map(_.paths.map(pathToString))

      val optimalTollsRoughgarden = network.pathTollsFromPathFlows(network.pathFlowsFromStrategies(optimalStrategy))
      val game = new ConstantTollRoutingGame(network, constantTolls)
      val coordinator = new MWCoordinator[ConstantTollRoutingGame](game, algorithms, randomizedStart)

      val strategies = coordinator.strategiesStream
      val flows = coordinator.gameStateStream.map(state => state.pathFlows)
      val socialCosts = flows.map(network.socialCostFromPathFlows(_))
      val losses = coordinator.lossStream
      //  val avgLatencies = coordinator.averageLossStream
      val optCost = optimizer.optimalCost

      println(optCost)
      Visualizer("Path Flows" + title).plotLineGroups(flows.take(T), "t", "f(t)", legend)
      //    Visualizer("Path Latencies").plotLineGroups(latencies.take(T), "t", "lat(t)", legend)
      Visualizer("Path Losses (latency + toll)" + title).plotLineGroups(losses.take(T), "t", "loss(t)", legend)
      Visualizer("Social cost" + title).plotLine(socialCosts.take(T), "t", "social cost", "Under tolls")
        .plotLine(Stream.continually(optCost).take(T), "t", "social cost", "Social optimum")
      Visualizer("Strategies" + title).plotStrategies(strategies.take(T))
    }

    //    System.out.println(network.toJSON())
    runConstantTolls(optimalTollsRoughgarden, " - Roughgarden")
    runConstantTolls(optimalTollsBlandin, " - Blandin")
  }
}