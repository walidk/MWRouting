package util

class InvalidDimensionException(message: String) extends Exception(message)

object linAlgebra {
  
  class Matrix(val M: Array[Array[Double]]) {
    val nRows = M.length
    val nCols = M(0).length
    
    // common attributes
    def apply(i:Int, j:Int): Double = {
      M(i)(j)
    }
    def apply(i:Int): Array[Double] = {
      M(i)
    }
    def size(): (Int, Int) = {(nRows, nCols)}
    
    // conversions (explicit)
    def arrayValue() = {
      M(0)
    }
    def doubleValue() = {
      M(0)(0)
    }
    
    // linear algebra operations
    def plus (a: Matrix): Matrix = {
      if(a.nRows != nRows || a.nCols != nCols)
        throw new InvalidDimensionException(
            "matrices have incompatible dimensions: " + size() + " and " + a.size())
      val s = Array.ofDim[Double](nRows, nCols)
      for (i <- 0 to nRows-1; j <- 0 to nCols-1)
        s(i)(j) = M(i)(j) + a.M(i)(j)
      new Matrix(s)  
    }
    
    def times(a: Matrix): Matrix = {
      if(a.nRows != nCols)
        throw new InvalidDimensionException("matrices have incompatible dimensions: " + size() + " and " + a.size())
      val p = Array.ofDim[Double](nRows, a.nCols)
      for (i <- 0 to nRows-1; j <- 0 to a.nCols-1; k<- 0 to nCols-1)
        p(i)(j) += M(i)(k)*a.M(k)(j)
      new Matrix(p)
    }
    
    def times(a: Double) = {
      val p = Array.ofDim[Double](nRows, nCols)
      for (i <- 0 to nRows-1; j <- 0 to nCols-1)
        p(i)(j) += M(i)(j)*a
      new Matrix(p)
    }
    
    def dotMult(a: Matrix): Matrix = {
      if(a.nRows != nRows || a.nCols != nCols)
        throw new InvalidDimensionException("matrices have incompatible dimensions: " + size() + " and " + a.size())
      val d = Array.ofDim[Double](nRows, nCols)
      for (i <- 0 to nRows-1; j <- 0 to nCols-1)
        d(i)(j) = M(i)(j)*a.M(i)(j)
      new Matrix(d)
    }
    
    def transp(): Matrix = {
      val t = Array.ofDim[Double](nCols, nRows)
      for (i <- 0 to nRows-1; j <- 0 to nCols-1)
        t(j)(i) = M(i)(j)
      new Matrix(t)
    }
    
    // other operations
    def argM(comp: (Double, Double)=>Boolean): ((Int, Int), Double) = {
      var m = M(0)(0)
      var arg = (0, 0)
      for(i<- 0 to nRows-1; j <- 0 to nCols-1)
        if(comp(M(i)(j), m)){m = M(i)(j); arg = (i, j)}
      (arg, m)
    }
    def argMin(): ((Int, Int), Double) = argM(_<_)
    def argMax(): ((Int, Int), Double) = argM(_>_)
  }
  
  
  // implicit conversions
  implicit def AToMatrix(a: Array[Double]): Matrix = {
    new Matrix(Array[Array[Double]](a))
  }
  
  implicit def AAToMatrix(aa: Array[Array[Double]]): Matrix = {
    new Matrix(aa)
  }
  
  
}