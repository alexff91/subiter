package matrix

import scala.Array._
import scala.math._
import io.Source
import annotation.tailrec
import java.io.{File, PrintWriter}

import collection.mutable
import org.apache.commons.math.linear.{FieldVector, Array2DRowFieldMatrix}

//операции в попытке сделать их походими по синтаксису на матлаб
trait MultiplyComplex[M1, M2, RESULT] {
  def apply(m1: M1, m2: M2): RESULT
}

object MultiplyComplex {

  //различные способы перемножения
  implicit object MultiplyRowVectorByVector extends MultiplyComplex[RowVectorcomplex, VectorComplex, Complex] {
    def apply(m1: RowVectorcomplex, m2: VectorComplex): Complex = (m1.multiply(m2)).apply(0).apply(0)
  }

  implicit object MultiplyRowVectorByMatrix extends MultiplyComplex[RowVectorcomplex, MatrixComplex, RowVectorcomplex] {
    def apply(m1: RowVectorcomplex, m2: MatrixComplex): RowVectorcomplex = new RowVectorcomplex(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByVector extends MultiplyComplex[MatrixComplex, VectorComplex, VectorComplex] {
    def apply(m1: MatrixComplex, m2: VectorComplex) = new VectorComplex(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByRowVector extends MultiplyComplex[MatrixComplex, RowVectorcomplex, MatrixComplex] {
    def apply(m1: MatrixComplex, m2: RowVectorcomplex) = new MatrixComplex(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByMatrix extends MultiplyComplex[MatrixComplex, MatrixComplex, MatrixComplex] {
    def apply(m1: MatrixComplex, m2: MatrixComplex) = new MatrixComplex(m1.multiply(m2))
  }

}

abstract class MatrixLikeComplex(var items: Array[Array[Complex]])(implicit operations: MatrixOperationsComplex = MyMatrixOperationsComplex) {
  type Repr <: MatrixLikeComplex


  def rows = items.map(RowVectorcomplex(_))

  def swap(vector: RowVectorcomplex, i: Int) = {
    val toswap = (items.transpose)
    toswap(i) = vector.items(0)
    items = toswap.transpose
    val t = 0

  }

  lazy val row_count = items.length
  lazy val cols = if (items.length == 0) 0 else items(0).length
  lazy val fast_apply = if (row_count * cols > 2000) operations.apply _ else operations.small_apply _


  def row(i: Int): Array[Complex] = items(i)

  def apply(row: Int, col: Int) = if (row >= row_count || col >= cols) throw new Exception("Index out of bound (%d,%d) matrix size is %dx%d".format(row, col, row_count, cols)) else items(row)(col)

  def *[A, B](m: A)(implicit multiply: MultiplyComplex[Repr, A, B]) = multiply(this.asInstanceOf[Repr], m)

  def unary_-() = apply(-_)

  /**
   * Multiplies the matrix by the scalar, ie.
   * <pre>
   * scala> MatrixComplex((1,2), (3,4)) * 2
   * res1: matrix.MatrixComplex = MatrixComplex(2x2):
   * 2         4
   * 6         8
   * </pre>
   */
  def *(scalar: Complex) = apply(_ * scalar)

  def +(scalar: Complex) = apply(_ + scalar)

  def -(scalar: Complex) = apply(_ - scalar)

  def /(scalar: Complex) = apply(_ / scalar)

  def /:(scalar: Complex) = apply(scalar / _)

  def *:(scalar: Complex) = this * scalar

  def +:(scalar: Complex) = this + scalar

  def -:(scalar: Complex) = apply(scalar - _)


  @inline def apply(f: Complex => Complex) = instance(fast_apply(items, f))

  @inline protected def instance(items: Array[Array[Complex]]): Repr

  def +(m: MatrixLikeComplex) = combine(m, _ + _)

  def -(m: MatrixLikeComplex) = combine(m, _ - _)

  def @*(m: MatrixLikeComplex) = combine(m, _ * _)

  def @/(m: MatrixLikeComplex) = combine(m, _ / _)

  def flatten: RowVectorcomplex = RowVectorcomplex(items.transpose.flatten)


  @inline private def combine(v1: Array[Complex], v2: Array[Complex])(fun: (Complex, Complex) => Complex): Array[Complex] = v1.zip(v2).par.map({
    case (x, y) => fun(x, y)
  }).toArray


  @inline def combine(m: MatrixLikeComplex, fun: (Complex, Complex) => Complex): Repr = {
    if (m.row_count != row_count || m.cols != cols) throw new Exception("Not the same matrix sizes %dx%d != %dx%d".format(row_count, cols, m.row_count, m.cols))
    val rows_combined = items.zip(m.items)
    instance(rows_combined.par.map {
      case (row1, row2) => combine(row1, row2)(fun)
    }.toArray)
  }

  def multiply(m: MatrixLikeComplex): Array[Array[Complex]] = {
    if (cols != m.row_count) throw new Exception("Can not multipy matrix %dx%d by %dx%d".format(row_count, cols, m.row_count, m.cols))
    operations.multiply(items, m.items)
  }

  def dropFirstColumn = instance(items.par.map(_.drop(1)).toArray)

  def toScalar: Complex = if (row_count == 1 && cols == 1) this(0, 0) else throw new Exception("MatrixComplex is not a scalar!")

  override def toString = mkString



  protected def magicFormat(value: Complex): String = {
    (try {
      val res = BigDecimal.valueOf(value.re).bigDecimal.stripTrailingZeros().toEngineeringString
      val res2 = BigDecimal.valueOf(value.im).bigDecimal.stripTrailingZeros().toEngineeringString
      if (res.length() > 8) "%8f".format(value) else res + "" + res2
    } catch {
      case e: NumberFormatException => "%8f".format(value)
    }).take(9).reverse.padTo(9, ' ').reverse
  }

  def mkString = "MatrixComplex(%dx%d):\n%s\n".format(row_count, cols, items.map(_.map(magicFormat).mkString(" ")).mkString("\n"))

  override def equals(p1: Any) = p1 match {
    case m: MatrixLikeComplex => items.deep.equals(m.items.deep)
    case _ => false
  }


  override def hashCode = throw new Error("Really?")

  def saveToFile(filePath: String) = {
    val f = new File(filePath).delete()
    val p = new PrintWriter(new File(filePath))
    p.write(items.map(_.mkString(" ")).mkString("\n"))
    p.close()
    this
  }
}


class MatrixComplex(items: Array[Array[Complex]]) extends MatrixLikeComplex(items) {
  type Repr = MatrixComplex

  def sum: RowVectorcomplex = row_count match {
    case 1 => RowVectorcomplex(items(0).reduce(_ + _))
    case _ => RowVectorcomplex(items.transpose.par.map(col => col.reduce(_ + _)).toArray)
  }

  def data: Array[Complex] = (List[Complex]() /: this.items)((x: List[Complex], y: Array[Complex]) => x ++ y.toList).toArray

  def tata: Array[Complex] = {
    val m = new mutable.MutableList[Complex]
    this.items.foreach(t => t.foreach(v => m.+=(v)))
    m.toArray
  }


  def T = new MatrixComplex(items.transpose)

  def ᵀ = this.T

  def ::(scalar: Complex): MatrixComplex = new MatrixComplex(items.par.map(scalar +: _).toArray)

  def ::(m: MatrixLikeComplex): MatrixComplex = new MatrixComplex(m.items.par.zip(items).map {
    case (r1, r2) => r1 ++ r2
  }.toArray)

  def instance(items: Array[Array[Complex]]) = new MatrixComplex(items)
}

class VectorComplex(items: Array[Array[Complex]]) extends MatrixLikeComplex(items) {
  type Repr = VectorComplex
  if (this.cols != 1) throw new Exception("VectorComplex can only have one column but it has %d!" format (this.cols))

  def T: RowVectorcomplex = new RowVectorcomplex(items.transpose)

  def ᵀ = this.T

  def sum: Complex = items.transpose.apply(0).reduce(_ + _)


  def ::(scalar: Complex): MatrixComplex = scalar :: new MatrixComplex(items)

  def ::(m: MatrixLikeComplex): MatrixComplex = m :: new MatrixComplex(items)

  def instance(items: Array[Array[Complex]]) = new VectorComplex(items)
}

class RowVectorcomplex(items: Array[Array[Complex]]) extends MatrixLikeComplex(items) {
  type Repr = RowVectorcomplex
  if (this.row_count != 1) throw new Exception("RowVectorcomplex can only have one row but it has %d!" format (this.row_count))

  def T: VectorComplex = new VectorComplex(items.transpose)

  def ᵀ = this.T


  //  def *(v:VectorComplex) : Complex = multiply(v).apply(0).apply(0)

 // def sum: Complex = items(0).sum

  def instance(items: Array[Array[Complex]]) = new RowVectorcomplex(items)

  def ::(scalar: Complex): RowVectorcomplex = new RowVectorcomplex(items.map(scalar +: _))

  def ::(r: RowVectorcomplex): RowVectorcomplex = new RowVectorcomplex(Array(r.items(0) ++ items(0)))

  /**
   * Converts RowVectorcomplex to the list of matricies given the dimensions list. i.e.
   * <pre>
   * scala> RowVectorcomplex(1,2,3,4,5,6,7,8,9).reshape((2,2),(1,5))
   * res0: List[matrix.MatrixComplex] =
   * List(MatrixComplex(2x2):
   * 1         3
   * 2         4
   * , MatrixComplex(1x5):
   * 5         6         7         8         9
   * )</pre>
   *
   **/
  def reshape(dimensions: (Int, Int)*): List[MatrixComplex] = {
    @tailrec def r(dimensions: List[(Int, Int)], data: Seq[Complex], accumulator: List[MatrixComplex]): List[MatrixComplex] = dimensions match {
      case (rows, cols) :: dimTail => {
        if (data.length < rows * cols) throw new Exception("Not enough elements. Dimensions left: " + dimensions)
        val res = Array.ofDim[Complex](rows, cols)
        val (current, dataTail) = data.splitAt(rows * cols)
        for (row <- 0 until rows; col <- 0 until cols) res(row)(col) = current(row + col * rows)
        r(dimTail, dataTail, new MatrixComplex(res) :: accumulator)
      }
      case Nil => {
        if (data.length > 0) throw new Exception("There are %d elements left! Not enough dimensions.".format(data.length))
        accumulator.reverse
      }
    }
    r(dimensions.toList, items(0), Nil)
  }

}

object VectorComplex {
  def apply(data: Complex*) = new VectorComplex(Array(data.toArray).transpose)

  def apply(data: Array[Complex]) = new MatrixComplex(Array(data).transpose)
}

object RowVectorcomplex {
  def apply(data: Complex, xs: Complex*) = new RowVectorcomplex(Array((data +: xs).toArray))

  def apply(data: Array[Complex]) = new RowVectorcomplex(Array(data))
}

object MatrixComplex {

  def fromFile(filename: String) = {
    def readMatrix(filePath: String): Array[Array[Complex]] = {
      Source.fromFile(filePath).getLines().filter(!_.startsWith("#")).filter(!_.trim().isEmpty).map(_.trim().split(" ").map(_.replace(',', '.')).map(x => new Complex(x.toDouble, 0.0))).toArray
    }

    new MatrixComplex(readMatrix(filename))
  }
  def diag(x: MatrixComplex) = {
    val res = Array.fill(x.cols, x.cols) (Complex(.0,.0) )
    (0 to x.cols - 1).par.foreach(i => res(i)(i) = x(0, i))
    new MatrixComplex(res)
  }
  def apply(t2: Product*) = new MatrixComplex(t2.toArray.map(t => t.productIterator.map(_ match {
    case d: {def toDouble: Complex} => d.toDouble
    case d: Complex => d
    case x => throw new Exception("Element " + x + "is not convertible to double")
  }).toArray))

  def apply(row: Complex, rest: Complex*) = new MatrixComplex(Array((row +: rest).toArray))

  implicit def arrayToVector(items: Array[Complex]): VectorComplex = VectorComplex(items: _*)

  implicit def arrayToRowVector(items: Array[Complex]): RowVectorcomplex = RowVectorcomplex(items)

  //  implicit def scalarToMatrix(scalar:Complex): MatrixComplex = new MatrixComplex(Array(Array(scalar)))


}