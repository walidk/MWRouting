package util

import breeze.plot._
import breeze.linalg._

object Visualization {
  def printData(data: DenseMatrix[Double], xlabel: String = "x", ylabel: String = "y", title: String = ""){
    val T = data.cols
    val A = data.rows
    
    val fig = Figure()
    fig.clear()
    val p = fig.subplot(0)
    val t = linspace(0, T, T)

    for(a <- 0 to A-1){
      val avgS: DenseVector[Double] = data.t(::, a)
      p+=plot(t, data.t(::, a))
    }
    p.xlabel = xlabel
    p.ylabel = ylabel
    p.title = title
  }

}