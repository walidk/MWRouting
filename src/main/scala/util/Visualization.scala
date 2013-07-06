package util

import breeze.plot._
import breeze.linalg._
import routing.LatencyFunction

// Companion object
object Visualizer {
  def apply(title: String) = new Visualizer(title)  
}

class Visualizer(title: String) {
  val fig = new Figure(title)
  fig.clear()

  def getPlot(i: Int) = fig.subplot(i + 1, 1, i)

  // main plot functions
  def addLine(pl: Plot, xs: Stream[Double], ys: Stream[Double], legend: String = "") {
    pl += plot(xs, ys, name = legend)
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
  }

  def addLine(pl: Plot, ys: Stream[Double], legend: String) {
    val xs = Stream.range(0, ys.size).map(_.doubleValue)
    addLine(pl, xs, ys, legend)
  }

  def addPoints(pl: Plot, xs: Stream[Double], ys: Stream[Double], legend: String = "") {
    pl += plot(xs, ys, '.', "black")
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
  }
  
  // other plot function
  def plotLine(
    line: Stream[Double],
    xLabel: String,
    yLabel: String) = {
    val pl = getPlot(0);
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    addLine(pl, line, "")
    this
  }
  
  def plotLineGroups(
    linesStream: Stream[Array[DenseVector[Double]]],
    xLabel: String,
    yLabel: String,
    legend: Array[Array[String]]) = {
    val lineGroups = linesStream.map(_.map(_.toArray))
    for (
      (lineGroup, i) <- lineGroups.transpose zipWithIndex;
      pl = getPlot(i);
      (line, leg) <- lineGroup.transpose zip legend(i)
    ) {
      pl.xlabel = xLabel
      pl.ylabel = yLabel
      addLine(pl, line, leg)
    }
    this
  }

  def plotFunctions(
    functions: Array[Double => Double],
    domain: (Double, Double),
    xLabel: String,
    yLabel: String,
    nbPoints: Int = 100) = {
    val pl = getPlot(0)
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    val xs = linspace(domain._1, domain._2, nbPoints).toArray.toStream
    for (f <- functions)
      addLine(pl, xs, xs map f)
    this
  }

  def plotLatencies(
    latencyFunctions: Array[LatencyFunction],
    domain: (Double, Double),
    xLabel: String,
    yLabel: String,
    nbPoints: Int = 100) = {
    val functions = latencyFunctions.map(lat => x=>lat(x))
    plotFunctions(functions, domain, xLabel, yLabel, nbPoints)
    this
  }
  
  def plotStrategies(
    strategies: Stream[Array[DenseVector[Double]]],
    usePoints: Boolean = false) = {
    for ((st, i) <- strategies.transpose zipWithIndex) {
      val support = st.head.length
      val verticesx = new Array[Double](support)
      val verticesy = new Array[Double](support)
      for (k <- 0 to support - 1) {
        val theta = k * 2 * math.Pi / support
        verticesx(k) = math.cos(theta)
        verticesy(k) = math.sin(theta)
      }
      val pl = fig.subplot(1, i + 1, i)
      val pointsx = st.map(_ dot DenseVector(verticesx))
      val pointsy = st.map(_ dot DenseVector(verticesy))
      addLine(pl, verticesx.toStream ++ verticesx.take(1), verticesy.toStream ++ verticesy.take(1))
      if (usePoints)
        addPoints(pl, pointsx, pointsy)
      else
        addLine(pl, pointsx, pointsy)
    }
    this
  }
  
  
}