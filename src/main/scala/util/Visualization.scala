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
    
  def printData(data: DenseMatrix[Double]): Visualizer = {
    val x = linspace(0, data.cols, data.cols)
    printData(data, x)
  }
  
  def printData(data: DenseMatrix[Double], x: DenseVector[Double]): Visualizer = {
    for(k <- 0 to data.rows-1){
      pl+=plot(x, data.t(::, k))
    }
    fig.refresh()
    this
  }
  
  def plotPoints(points: Array[(Double, Double)]): Visualizer = {
    for((x, y) <- points){
      pl+=plot(List(x), List(y), '+',  "black", "bla", true)
    }
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

}