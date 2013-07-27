package util

import breeze.plot._
import breeze.linalg._
import routing.LatencyFunction
import java.awt.{ BasicStroke, GradientPaint, Color }

// Companion object
object Visualizer {
  def apply(title: String) = new Visualizer(title)
}

// Each instance of Visualizer corresponds to one figure (i.e. one window)
// the figure can contain several sub-figures, created using getPlot(i)
class Visualizer(title: String) {
  private val dashedStroke =
    new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f, Array(1.0f, 6.0f), 0.0f)

  val fig = new Figure(title)
  private var nbPlots = 1;
  fig.clear()

  def getPlot(i: Int) = {
    nbPlots = math.max(nbPlots, i + 1)
    fig.height = nbPlots * 300;
    fig.subplot(nbPlots, 1, i)
  }

  def setDashed(i: Int) {
    val xyPlot = getPlot(i).chart.getXYPlot()
    val renderer = xyPlot.getRenderer(xyPlot.getRendererCount() - 1);
      renderer.setSeriesStroke(0, dashedStroke)
  }
  
  // main plot functions
  private def addLine(i: Int, xs: Stream[Double], ys: Stream[Double], legend: String = "") {
    val pl = getPlot(i)
    pl += plot(xs, ys, name = legend)
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
  }

  private def addLineDefault(i: Int, ys: Stream[Double], legend: String = "") {
    val xs = Stream.range(0, ys.size).map(_.doubleValue)
    addLine(i, xs, ys, legend)
  }

  def addPoints(i: Int, xs: Stream[Double], ys: Stream[Double], legend: String = "") {
    val pl = getPlot(i)
    pl += plot(xs, ys, '.', "black")
    if (!legend.isEmpty())
      pl.legend = true
    fig.refresh()
  }

  // other plot function
  def plotLine(line: Stream[Double], xLabel: String, yLabel: String, legend: String = "") = {
    val pl = getPlot(0)
    pl.xlabel = xLabel
    pl.ylabel = yLabel
    addLineDefault(0, line, legend)
    this
  }
  
  def plotDashedLine(line: Stream[Double], xLabel: String, yLabel: String, legend: String = "") = {
    plotLine(line, xLabel, yLabel, legend)
    setDashed(0)
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
      addLineDefault(i, line, leg)
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
    val functions = latencyFunctions.map(lat => x => lat(x))
    plotFunctions(functions, domain, xLabel, yLabel, nbPoints)
    this
  }

  def plotStrategies(
    strategies: Stream[Array[DenseVector[Double]]],
    usePoints: Boolean = false) = {
    
    def makeCycle(array: Array[Double]) = array.toStream ++ array.take(1)
    
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
      
      addLine(i, makeCycle(verticesx), makeCycle(verticesy))
      getPlot(i).chart.getXYPlot().getRenderer().setSeriesPaint(0, Color.GRAY)
      
      if (usePoints)
        addPoints(i, pointsx, pointsy)
      else {
        addLine(i, pointsx, pointsy)
        val xyPlot = getPlot(i).chart.getXYPlot()
        val renderer = new GradientRenderer(Color.RED, pointsx.length)
        renderer.setSeriesShapesVisible(0, false)
        xyPlot.setRenderer(1, renderer)
      }
    }
    
    
    import org.jfree.chart.renderer.xy.{XYItemRenderer, XYLineAndShapeRenderer}
    class GradientRenderer(color: Color, totalPoints: Int) extends XYLineAndShapeRenderer {
      private val alphaStream = (0 to totalPoints - 2).map(x => (x * 1. / totalPoints)).toStream
      private val alphaStreamRec: Stream[Double] = alphaStream #::: alphaStreamRec
      private val alphaIterator = alphaStreamRec.toIterator

      override def getItemPaint(row: Int, column: Int) = {
        new Color(color.getRed(), color.getGreen(), color.getBlue(), 40 + (215 * alphaIterator.next()).intValue)
      }
    }
    
    fig.width = 300
    this
  }
  
  def exportToPdf(title: String){
    fig.saveas("fig_"+title+".pdf", 300)
  }
}