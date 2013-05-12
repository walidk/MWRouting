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
    
  def plotData(data: DenseMatrix[Double]): Visualizer = {
    val xs = linspace(0, data.cols, data.cols)
    plotData(xs, data)
  }

  def plotData(dataArray: Array[DenseMatrix[Double]]): Visualizer = {
    val rows = dataArray.map(_.rows).sum
    val cols = dataArray(0).cols
    val data = DenseMatrix.zeros[Double](rows, cols)
    var currentRow = 0
    for(k <- 0 to dataArray.length-1){
      val dat = dataArray(k)
      data(currentRow to currentRow+dat.rows-1, 0 to cols-1):=dataArray(k)
      currentRow += dat.rows
    }
    
    plotData(data)
  }
  
  def plotData(xs: DenseVector[Double], data: DenseMatrix[Double]): Visualizer = {
    for(k <- 0 to data.rows-1){
      pl+=plot(xs, data.t(::, k))
    }
    fig.refresh()
    this
  }
  
  def plotPoints(xs: DenseVector[Double], ys: DenseVector[Double]): Visualizer = {
    pl+=plot(xs, ys, '+',  "black", "bla", true)
    fig.refresh()
    this
  }
  
  def plotFunctions(functions: Array[Double => Double], domain: (Double, Double)): Visualizer = {
    val x = linspace(domain._1, domain._2, 10)
    for(k <- 0 to functions.size-1){
      pl+=plot(x, x.map(functions(k)))
    }
    fig.refresh()
    this
  }
  
  def plotStrategies(data: DenseMatrix[Double]): Visualizer = {
    val support = data.rows
    val vertices = DenseMatrix.zeros[Double](2, support)
    for(k<- 0 to support-1){
      val theta = k*2*math.Pi/support
      vertices(0, k) = math.cos(theta)
      vertices(1, k) = math.sin(theta)  
    }
    val verticesCycle = DenseMatrix.horzcat(vertices, vertices(0 to 1, 0 to 0))
    val points = (vertices*data)
    
    plotData(verticesCycle.t(::,0), verticesCycle.t(::,1 to 1).t)
    plotPoints(points.t(::, 0), points.t(::, 1))
    
    fig.refresh()
    this
  }

}