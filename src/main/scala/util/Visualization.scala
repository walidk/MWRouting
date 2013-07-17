package util

import breeze.plot._
import breeze.linalg._
import routing.LatencyFunction
import java.awt.BasicStroke


// Companion object
object Visualizer {
  def apply(title: String) = new Visualizer(title)  
}

class Visualizer(title: String) {
  private val dashedStroke = 
    new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f, Array(1.0f, 6.0f), 0.0f)
  
  val fig = new Figure(title)
  private var nbPlots = 1;
  fig.clear()

  def getPlot(i: Int) = {
    nbPlots = math.max(nbPlots, i+1)
    fig.subplot(nbPlots, 1, i)
  }
  
  // main plot functions
  private def addLine(i: Int, xs: Stream[Double], ys: Stream[Double], legend: String = "", dashed: Boolean = false) {
    val pl = getPlot(i)
    pl += plot(xs, ys, name = legend)
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
    
    if(dashed){
      val xyPlot = pl.chart.getXYPlot()
      val renderer = xyPlot.getRenderer(xyPlot.getRendererCount()-1);
      if(renderer != null)
        renderer.setSeriesStroke(0, dashedStroke)
    }
  }

  private def addLineDefault(i: Int, ys: Stream[Double], legend: String = "", dashed: Boolean = false) {
    val xs = Stream.range(0, ys.size).map(_.doubleValue)
    addLine(i, xs, ys, legend, dashed)
  }

  def addPoints(i: Int, xs: Stream[Double], ys: Stream[Double], legend: String = "") {
    val pl = getPlot(i)
    pl += plot(xs, ys, '.', "black")
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
  }
  
  // other plot function
  def plotLine(
    line: Stream[Double],
    xLabel: String,
    yLabel: String,
    legend: String = "",
    dashed: Boolean = false) = {
    val pl = getPlot(0);
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    addLineDefault(0, line, legend, dashed)
    this
  }
  
  def plotLineGroups(
    linesStream: Stream[Array[DenseVector[Double]]],
    xLabel: String,
    yLabel: String,
    legend: Array[Array[String]],
    dashed: Boolean = false) = {
    val lineGroups = linesStream.map(_.map(_.toArray))
    for (
      (lineGroup, i) <- lineGroups.transpose zipWithIndex;
      pl = getPlot(i);
      (line, leg) <- lineGroup.transpose zip legend(i)
    ) {
      pl.xlabel = xLabel
      pl.ylabel = yLabel
      addLineDefault(i, line, leg, dashed)
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
      addLine(0, xs, xs map f)
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
      val pointsx = st.map(_ dot DenseVector(verticesx))
      val pointsy = st.map(_ dot DenseVector(verticesy))
      addLine(i, verticesx.toStream ++ verticesx.take(1), verticesy.toStream ++ verticesy.take(1))
      if (usePoints)
        addPoints(i, pointsx, pointsy)
      else
        addLine(i, pointsx, pointsy)
    }
    this
  }
  
  def exportToPdf(title: String){
    fig.saveas("fig_"+title+".pdf", 300)
  }
}