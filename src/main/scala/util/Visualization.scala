package util

import breeze.plot._
import breeze.linalg._

// Companion object
object Visualizer {
  def plotLine(
    line: Stream[Double],
    xLabel: String,
    yLabel: String,
    figTitle: String) {
    val vis = new Visualizer(figTitle)
    val pl = vis.fig.subplot(1, 1, 0);
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    vis.addLine(pl, line, "")
  }

  def plotLineGroups(
    linesStream: Stream[Array[DenseVector[Double]]],
    xLabel: String,
    yLabel: String,
    figTitle: String,
    legend: Array[Array[String]]) {
    val vis = new Visualizer(figTitle)
    val lineGroups = linesStream.map(_.map(_.toArray))
    for (
      (lineGroup, i) <- lineGroups.transpose zipWithIndex;
      pl = vis.fig.subplot(i + 1, 1, i);
      (line, leg) <- lineGroup.transpose zip legend(i)
    ) {
      pl.xlabel = xLabel
      pl.ylabel = yLabel
      vis.addLine(pl, line, leg)
    }
  }

  def plotFunctions(
    functions: Array[Double => Double],
    domain: (Double, Double),
    xLabel: String,
    yLabel: String,
    title: String,
    nbPoints: Int = 100) {
    val vis = new Visualizer(title)
    val pl = vis.fig.subplot(0)
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    val xs = linspace(domain._1, domain._2, nbPoints).toArray.toStream
    for (f <- functions)
      vis.addLine(pl, xs, xs map f)
  }

  def plotStrategies(
    strategies: Stream[Array[DenseVector[Double]]],
    usePoints: Boolean = false) {
    val vis = new Visualizer("Strategies")
    val fig = vis.fig

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
      vis.addLine(pl, verticesx.toStream ++ verticesx.take(1), verticesy.toStream ++ verticesy.take(1))
      if (usePoints)
        vis.addPoints(pl, pointsx, pointsy)
      else
        vis.addLine(pl, pointsx, pointsy)
    }
    fig.refresh()
  }
}

private class Visualizer(title: String) {
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
}