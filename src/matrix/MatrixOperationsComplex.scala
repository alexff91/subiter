package matrix

import sun.reflect.generics.reflectiveObjects.NotImplementedException


trait MatrixOperationsComplex {
  def apply(items: Array[Array[Complex]], f: (Complex) => Complex): Array[Array[Complex]] = small_apply(items, f)

  def small_apply(items: Array[Array[Complex]], f: (Complex) => Complex): Array[Array[Complex]] = items.map(_.map(f(_)))

  def multiply(m1: Array[Array[Complex]], m2: Array[Array[Complex]]): Array[Array[Complex]] = throw new NotImplementedException
}

object MyMatrixOperationsComplex extends MatrixOperationsComplex {


  //  override def apply(items: Array[Array[Complex]], f: (Complex) => Complex) = items.par.map(_.map(f(_))).toArray
  override def apply(items: Array[Array[Complex]], f: (Complex) => Complex) = {
    val rows = items.length
    val cols = items(0).length

    val res = Array.ofDim[Complex](rows, cols)
    @inline def distribute_by_rows = {
      (0 until rows).par.foreach {
        row =>
          var col = 0
          while (col < cols) {
            res(row)(col) = f(items(row)(col))
            col += 1
          }
      }
      res
    }

    @inline def distribute_by_cols = {
      (0 until cols).par.foreach {
        col =>
          var row = 0
          while (row < rows) {
            res(row)(col) = f(items(row)(col))
            row += 1
          }
      }
      res
    }

    if (rows < cols)
      distribute_by_rows
    else
      distribute_by_cols

  }


  override def multiply(m1: Array[Array[Complex]], m2: Array[Array[Complex]]): Array[Array[Complex]] = {
    val res = Array.ofDim[Complex](m1.length, m2(0).length)
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    @inline def singleThreadedMultiplicationFAST(start_row: Int, end_row: Int) {
      var col, i = 0
      var sum = Complex(0.,0.)
      var row = start_row

      // while statements are much faster than for statements
      while (row < end_row) {
        col = 0
        while (col < M2_COLS) {
          i = 0;
          sum = Complex(0.,0.)
          while (i < M1_COLS) {
            sum = sum +  m1(row)(i) * m2(i)(col)
            i += 1
          }

          res(row)(col) = sum
          col += 1

        };
        row += 1
      }
    }

    (0 until M1_ROWS).par.foreach {
      i =>
        singleThreadedMultiplicationFAST(i, i + 1)
    }

    res

  }

}



