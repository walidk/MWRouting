package util

import breeze.plot._
import breeze.linalg._

class Visualizer(xlabel: String, ylabel: String, title: String) {
  val fig = Figure()
  fig.clear()
  val pl = fig.subplot(0)
  pl.xlabel = xlabel
  pl.ylabel = ylabel
  pl.title = title
  
  // main plot functions
  def plotData(xs: DenseVector[Double], ys: DenseVector[Double], name: String = ""): Visualizer = {
    pl+=plot(xs, ys, name = name)
    if(!name.isEmpty())
      pl.legend = true
    fig.refresh()
    this
  }
  
  def plotPoints(xs: DenseVector[Double], ys: DenseVector[Double]): Visualizer = {
    pl+=plot(xs, ys, '.',  "black")
    fig.refresh()
    this
  }
  
  // other plot functions
  def plotData(data: DenseMatrix[Double], names: Array[String]): Visualizer = {
    val xs = linspace(0, data.cols, data.cols)
    plotData(xs, data, names)
  }

  def plotData(dataArray: Array[DenseMatrix[Double]], names: Array[Array[String]]): Visualizer = {
    val rows = dataArray.map(_.rows).sum
    val cols = dataArray(0).cols
    val data = DenseMatrix.zeros[Double](rows, cols)
    var currentRow = 0
    for(k <- 0 to dataArray.length-1){
      val dat = dataArray(k)
      data(currentRow to currentRow+dat.rows-1, 0 to cols-1):=dataArray(k)
      currentRow += dat.rows
    }
    
    plotData(data, names.flatten)
  }
  
  def plotData(xs: DenseVector[Double], data: DenseMatrix[Double], names: Array[String]): Visualizer = {
    for(k <- 0 to data.rows-1){
      plotData(xs, data.t(::, k), names(k))
    }
    this
  }
  
  def plotFunctions(functions: Array[Double => Double], domain: (Double, Double), names: Array[String], nbPoints: Int = 100): Visualizer = {
    val x = linspace(domain._1, domain._2, nbPoints)
    for(k <- 0 to functions.size-1){
      plotData(x, x.map(functions(k)), names(k))
    }
    this
  }
  
  def plotStrategies(data: DenseMatrix[Double], continuous: Boolean = false): Visualizer = {
    val support = data.rows
    val vertices = DenseMatrix.zeros[Double](2, support)
    for(k<- 0 to support-1){
      val theta = k*2*math.Pi/support
      vertices(0, k) = math.cos(theta)
      vertices(1, k) = math.sin(theta)  
    }
    val verticesCycle = DenseMatrix.horzcat(vertices, vertices(0 to 1, 0 to 0))
    val points = (vertices*data)
    
    plotData(verticesCycle.t(::,0), verticesCycle.t(::,1))
    if(continuous)
      plotData(points.t(::, 0), points.t(::, 1))
    else
      plotPoints(points.t(::, 0), points.t(::, 1))
    
    fig.refresh()
    this
  }

}