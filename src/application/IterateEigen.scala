package application

import matrix.{RowVector, MyMatrixOperations, Matrix}
import collection.mutable
import org.apache.commons.math.linear.RealMatrixImpl
import java.util
import java.io.IOException


/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 18.09.12
 * Time: 22:02
 * To change this template use File | Settings | File Templates.
 */
object IterateEigen extends App {


  /*  Следующий алгоритм реализует модицифированный процесс Грама-Шмадта
  ортонормализации векторов относитьельно матрицы В
    for i from 1 to k do
    (normalize)
    for j from i+1 to k do
    (remove component in direction vi)
    next j
      next i
      Асимптотическая оценка алгоритма 2nk^2 floating point operations,
  где n - размерность матрицы, k- размерность векторов (Golub & Van Loan 1996, §5.2.8).
  */
  def mgs(xs: Matrix, b: Matrix) = {
    println("Force B-orthogonal columns in X")
    //Считываем вектора
    var x = new Matrix(xs.items)
    //получаем размерности
    val m = x.row_count
    val n = x.cols
    //выходные ортонормарованные вектора


    var Y_return = new Matrix(Array(Array(0.0)))
    var Y = new Matrix(new Array[Array[Double]](n, n))
    var R = new Matrix(new Array[Array[Double]](n, n))
    //проходим по всем столбцам
    (0 to n - 1).foreach {
      i => {
        //i-й столбец

        val singleColumn = allRows(x, i)
        //нормализуем его
        val acc = ((singleColumn).*(b)).*(singleColumn.T)
        R.row(i)(i) = Math.sqrt(Math.abs(acc.row(0)(0)))
        //добавляем нормализованный столбец в конец матрицы У

        Y = repeat(singleColumn / R.row(i)(i), i, Y_return)
        (i + 1 to n - 1).foreach {
          j => {
            //вычисляем следующие ортоганальные векторa
            val rowy = allRows(Y, i)
            val rowx = allRows(x, j)
            val acc = (rowy) * b
            R.row(i)(j) = (acc * rowx.T).row(0)(0)
            forAll(x, j, rowx - rowy * R.row(i)(j))

            //x.swap(new RowVector((rowx -  rowy * R.row(i)(j)).items),j)
          }
        } //end for
        //println(i)
        Y_return = Y
      }
    } //end for
    Y_return
  }

  def mgsMy(x: Matrix, b: Matrix) = {
    println("Force B-orthogonal columns in X")

    //получаем размерности
    val m = x.row_count
    val n = x.cols
    //выходные ортонормарованные вектора


    var R = new Matrix(new Array[Array[Double]](n, n))
    //проходим по всем столбцам
    (0 to n - 1).foreach {
      i => {
        //i-й столбец

        val singleColumn = allRows(x, i)
        //нормализуем его
        val acc = ((singleColumn).*(b)).*(singleColumn.T)
        R.row(i)(i) = Math.sqrt(Math.abs(acc.row(0)(0)))
        //добавляем нормализованный столбец в конец матрицы У

        forAll(x, i, singleColumn / R.row(i)(i))
        (i + 1 to n - 1).par.foreach {
          j => {

            val rowy = allRows(x, i)
            val rowx = allRows(x, j)
            val acc = (rowy) * b
            R.row(i)(j) = (acc * rowx.T).row(0)(0)
            forAll(x, j, rowx - rowy * R.row(i)(j))

            //x.swap(new RowVector((rowx -  rowy * R.row(i)(j)).items),j)
          }
        } //end for
        //println(i)

      }
    } //end for
    x
  }

  /*
 * Метод принимает значения сдвига,двух матриц положительно определенных и симметричных - А и В,
 * так же принимает значение коэффициента уверенности - признак окончания итераций
 * */
  def subspaceIter(A: Matrix, B: Matrix, sigma: Double, tol: Double, allEigs: Int): (Matrix, Matrix) = {
    //размерность квадратной матрицы
    val lenghtA = A.row_count;
    //матрица собственных значений
    var v: Matrix = new Matrix(Array(Array(0.0)))
    var dro: Matrix = new Matrix(Array.fill(1, lenghtA)(0))
    A.saveToFile("newA.txt")
    B.saveToFile("newB.txt")
    isSingular(A, B)
    var X = new Generator(lenghtA, 1).eye(allEigs)
    //  X = mgs(X, B) раз задаю ортоганальными, то не нужно вначале ортоганализировать
    val K = A - B.*(sigma)
    //сдвигиваем)
    val copyOfarrayK = new Matrix(LUDecomposition.copyOf(K.items))

    var flag = true
    var incopyOFK = copyOfarrayK.inverse
    //LU-разложение на потом
    //    val lu = new LUDecomposition(copyOfarrayK);
    //    var incopyOFK = new Matrix(lu.inverseUL().items)
    // if (!lu.isNonsingular) throw new Exception("Matrix is Singular,try again")
    //можем заменить обычную обратную матрицу
    //на произведение обратных к треугольным A^-1 = U^-1 * L ^-1
    val whatToMultiply = incopyOFK * B
    println("Precondition computed")
    //val whatToMultiply = (ul* B)
    var i = 0
    //итерируем
    while (i < 30 && flag) {
      //  var Q = (u * l * B) * X //  Compute next subspace using shift-invert strategy
      var Q = whatToMultiply * X

      val X0 = X //  Store matrix X for later use
      //  val eval = QR.qr(Q.items) не то
      Q = mgsMy(Q, B) //  Force B-orthogonal columns in Q

      if (i % 5 == 0) {
        println("Ritz step!")
        //проецирование на матрицы гораздо меньших размерностей
        val K_bar = Q.T * K * Q // projectiong of K onto Q
        val B_bar = Q.T * B * Q //  projectiong of B onto Q
        //здесь будух храниться и собст. вектора и знач.
        var wd = (new Matrix(Array.fill(1, 1)(0.0)), new Matrix(Array.fill(1, 1)(0.0)))

        //для малой проблемы находятся собств. значения и вектора при помощи QZ алгоритма
        val res = QZ.qz(K_bar.items, B_bar.items)
        // (ALPHAR(j) + ALPHAI(j)*i)/BETA(j)
        //вычисление собственных значений
        val realEigenValues = for (alphar <- 0 until (res.get(2)(0)).length)
        yield (res.get(2)(0)(alphar) / res.get(2)(2)(alphar));
        wd = (new Matrix(res.get(1)), new Matrix(Array(realEigenValues.toArray)))
        //матрицы собственных векторов и значений
        val W = wd._1
        val D = wd._2
        println("Eigens founded!")
        //транспонируем вектор собств.знач
        var d = Array.fill(D.cols, 1)(0.0)
        for (i <- 0 until d.length) {
          d(i)(0) = D.row(0)(i)
        }
        var dd = new Matrix(d)

        //сортируем собств. значения в порядке убывания при этом сохраняя их индексы
        val d1ind = sort(dd);
        dd = select(dd, d1ind._2)
        dro = dd
        val indTemp = d1ind._2.items.map(_.map(_.toInt))
        val Wtemp = new Matrix(W.items.map(_.clone))
        //переставляем вектора в соответствии с собст. значениями
        for (j <- 0 until X.cols) {
          forAll(W, j, allRows(Wtemp, (indTemp(0)(j)).toInt))
        }
        //домножаем на вектора ритца
        X = Q * W
      }
      else X = Q

      val res = X0 - X * (X.T * B * X0)
      //  val Bnorm = (res.T * B * res).@^(0.5)
      //норма разницы векторов должна быть меньше заданной пользователем точности
      val ququ = math.sqrt((res).items.foldLeft(0.0)((a, b) => b.foldLeft(0.0)((a1, b1) => b1 * b1 + a1) + a))
      println("Good values?")
      // val Bnorm = (X0 - X).@^(0.5)
      if (ququ < tol) {
        v = X
        dro = dro + sigma
        flag = false
      }
      i = i + 1
    }
    if (v.row_count <= 1) {
      v = X
      dro = dro + sigma
    }
    (v, dro.T)
  }

  //select one column
  def allRows(x: Matrix, i: Int): Matrix = {

    new Matrix(Array(x.items.par.map(_.apply(i)).toArray))
  }

  //заменяем столбец матрицы на другой
  def forAll(x: Matrix, n: Int, change: Matrix) = {
    (0 until x.row_count).par.foreach {
      i => x.row(i)(n) = change.row(0)(i)
    }
  }

  //сортировка с запоминанием индексов
  def sort(x: Matrix): (Matrix, Matrix) = {
    val leng = length(x)
    val result = new mutable.MutableList[Array[Double]]()
    val ind = new mutable.MutableList[Array[Double]]()
    (0 until x.cols).par.foreach {
      i => {
        val col = allRows(x, i)
        val acc = (col.data.zipWithIndex.sortBy(_._1))
        result.+=(acc.unzip._1.toArray)
        ind.+=(acc.unzip._2.map(_.toDouble).toArray)
      }
    }
    (new Matrix(result.toArray).T, new Matrix(ind.toArray))
  }

  def length(x: Matrix) = math.max(x.row_count, x.cols)

  //добавление вектора или матрицы к исходной
  def repeat(x: Matrix, n: Int, y: Matrix): Matrix = {
    if (n > 0) y :: (x.T) else x.T

  }

  //выборка элементов по матрице индексов
  def select(x: Matrix, y: Matrix): Matrix = {
    val acc = allRows(x, 0)
    val tik = acc.items(0)
    val yTemp = y.items.map(_.map(_.toInt))
    val res = for (i <- 0 until yTemp.length) yield (yTemp(i).foldLeft(List[Double]())((y, c) => y ::: List(tik(c)))).toArray
    new Matrix(res.toArray)
  }

  //выход
  def closes() = {
    System.exit(1)
  }

  def isSingular(A: Matrix, B: Matrix) {
    try {
      val testSingular = A.inverse
    }
    catch {
      case e: Exception => {
        throw new Exception("Matrix is singular")
      }
    }
    val i = 0
    try {
      val testSingular1 = B.inverse
    }
    catch {
      case e: Exception => {
        throw new Exception("Matrix is singular")
      }
    }
  }
}

//for later use
class MyExeption extends Exception {

  def getMessage(mes: String) = {
    mes
  }

  override def printStackTrace() {
    println("Matrix is singular or is too big try o use 17 symbols! I don't want to work")
  }
}
