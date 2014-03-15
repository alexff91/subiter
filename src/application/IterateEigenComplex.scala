package application

import matrix.{Complex, RowVector, MyMatrixOperations, MatrixComplex, Matrix}
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
object IterateEigenComplex extends App {


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
  def mgs(xs: MatrixComplex, b: MatrixComplex) = {
    println("Force B-orthogonal columns in X")
    //Считываем вектора
    var x = new MatrixComplex(xs.items)
    //получаем размерности
    val m = x.row_count
    val n = x.cols
    //выходные ортонормарованные вектора


    var Y_return = new MatrixComplex(Array(Array(Complex(0.0, 0.0))))
    var Y = new MatrixComplex(new Array[Array[Complex]](n, n))
    var R = new MatrixComplex(new Array[Array[Complex]](n, n))
    //проходим по всем столбцам
    (0 to n - 1).foreach {
      i => {
        //i-й столбец

        val singleColumn = allRows(x, i) * (b)
        //нормализуем его
        val acc = (singleColumn) * (singleColumn.T)
        R.row(i)(i) = Complex(Math.sqrt(Math.abs((acc.row(0)(0)).re)), Math.sqrt(Math.abs((acc.row(0)(0)).im)))
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


  /*
 * Метод принимает значения сдвига,двух матриц положительно определенных и симметричных - А и В,
 * так же принимает значение коэффициента уверенности - признак окончания итераций
 * */
  def subspaceIter(A: Matrix, B: Matrix, sigma: Double, tol: Double, allEigs: Int): (MatrixComplex, MatrixComplex) = {
    //размерность квадратной матрицы
    val lenghtA = A.row_count;
    //матрица собственных значений
    var v: MatrixComplex = new MatrixComplex(Array(Array(Complex(.0, .0))))
    var dro: MatrixComplex = new MatrixComplex(Array.fill(1, lenghtA)(Complex(.0, .0)))
    val testSingular = A.inverse
    val testSingular1 = B.inverse
    var X = MatrixComplex.diag(new MatrixComplex(Array.fill(lenghtA, allEigs)(Complex(1.0, 0.0))))
    //  X = mgs(X, B) раз задаю ортоганальными, то не нужно вначале ортоганализировать
    val K = A - B.*(sigma)
    //сдвигиваем)
    //val copyOfarrayK = new MatrixComplex(LUDecomposition.copyOf(K.items))

    var flag = true
    var incopyOFK = K.inverse
    //LU-разложение на потом
    //    val lu = new LUDecomposition(copyOfarrayK);
    //    var incopyOFK = new MatrixComplex(lu.inverseUL().items)
    // if (!lu.isNonsingular) throw new Exception("MatrixComplex is Singular,try again")
    //можем заменить обычную обратную матрицу
    //на произведение обратных к треугольным A^-1 = U^-1 * L ^-1
    val whatToMultiply = incopyOFK * B
    println("Precondition computed")
    //val whatToMultiply = (ul* B)
    var i = 0
    //итерируем
    while (i < 6 && flag) {
      //  var Q = (u * l * B) * X //  Compute next subspace using shift-invert strategy
      var Q = toComplex(whatToMultiply) * X

      val X0 = X //  Store matrix X for later use
      //  val eval = QR.qr(Q.items) не то
      Q = mgs(Q, toComplex(B)) //  Force B-orthogonal columns in Q

      if (i % 5 == 0) {
        //проецирование на матрицы гораздо меньших размерностей
        val K_bar = Q.T * (toComplex(K) * Q) // projectiong of K onto Q
        val B_bar = Q.T * (toComplex(B) * Q) //  projectiong of B onto Q
        //здесь будух храниться и собст. вектора и знач.
        var wd = (new MatrixComplex(Array.fill(1, 1)(Complex(.0, .0))), new MatrixComplex(Array.fill(1, 1)(Complex(.0, .0))))

        //для малой проблемы находятся собств. значения и вектора при помощи QZ алгоритма
        val res = QZ.qzIM(toReal(K_bar).items, toReal(B_bar).items)
        // (ALPHAR(j) + ALPHAI(j)*i)/BETA(j)
        //вычисление собственных значений
        var countOfIms = 0
        val realEigenValues = for (alphar <- 0 until (res.get(2)(0)).length)
        yield if (res.get(2)(1) == 0) Complex(res.get(2)(0)(alphar) / res.get(2)(2)(alphar), 0.0)
          else {
            countOfIms = countOfIms + 1
            if (countOfIms == 1) Complex(res.get(2)(0)(alphar) / res.get(2)(2)(alphar), res.get(2)(1)(alphar) / res.get(2)(2)(alphar));
            else {
              countOfIms = 0
              Complex(res.get(2)(0)(alphar - 1) / res.get(2)(2)(alphar - 1), -res.get(2)(1)(alphar - 1) / res.get(2)(2)(alphar - 1));

            }
          }
        //If the j-th eigenvalue is real, then
        //    u(j) = VL(:,j), the j-th column of VL. If the j-th and
        //          (j+1)-th eigenvalues form a complex conjugate pair, then
        //         u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
        val vIm = new MatrixComplex(new Array[Array[Complex]](X.row_count, X.cols))
        var isComplex = 0
        val resac1 = res.get(1).transpose
        for (ind <- 0 until realEigenValues.length) {
          if ((resac1(ind)) == 0.0) forAll(vIm, ind, toComplex(new Matrix(Array(resac1(ind)))))
          else {
            isComplex = isComplex + 1
            if (isComplex == 1) forAll(vIm, ind, new MatrixComplex(
              Array((for (t <- 0 until resac1(ind).length) yield Complex(resac1(ind)(t), resac1(ind + 1)(t))).toArray)))
            else {
              forAll(vIm, ind, new MatrixComplex(
                Array((for (t <- 0 until resac1(ind).length) yield Complex(resac1(ind - 1)(t), -resac1(ind)(t))).toArray)))
              isComplex = 0
            }
          }
        }
        wd = (vIm, new MatrixComplex(Array(realEigenValues.toArray)))
        //матрицы собственных векторов и значений
        val W = wd._1
        val D = wd._2
        println("Eigens founded!")
        //транспонируем вектор собств.знач
        var d = Array.fill(D.cols, 1)(Complex(0.0, 0.0))
        for (i <- 0 until d.length) {
          d(i)(0) = D.row(0)(i)
        }
        var dd = new MatrixComplex(d)

        //сортируем собств. значения в порядке убывания при этом сохраняя их индексы
        val d1ind = sort(dd);
        dd = select(dd, d1ind._2)
        dro = dd
        val indTemp = d1ind._2.items.map(_.map(_.toInt))
        val Wtemp = new MatrixComplex(W.items.map(_.clone))
        //переставляем вектора в соответствии с собст. значениями
        for (j <- 0 until X.cols) {
          forAll(W, j, allRows(Wtemp, (indTemp(0)(j)).toInt))
        }
        //домножаем на вектора ритца
        X = Q * W
      }
      else X = Q

      val res = X0 - X * (X.T * toComplex(B)) * X0
      //  val Bnorm = (res.T * B * res).@^(0.5)
      //норма разницы векторов должна быть меньше заданной пользователем точности
      val toSQRT = ((res).items.foldLeft(Complex(.0, .0))((a, b) => b.foldLeft(Complex(.0, .0))((a1, b1) => ((b1 * b1) + a1)) + a))
      val ququ = Complex(math.sqrt(toSQRT.re), math.sqrt(toSQRT.im))

      // val Bnorm = (X0 - X).@^(0.5)
      if (ququ.re < tol && ququ.im < tol) {
        v = X
        dro = dro + Complex(sigma, 0.0)
        flag = false
      }
      i = i + 1
    }
    if (v.row_count <= 1) {
      v = X
      dro = dro + Complex(sigma, 0.0)
    }
    (v, dro.T)
  }

  //select one column
  def allRows(x: MatrixComplex, i: Int): MatrixComplex = {

    new MatrixComplex(Array(x.items.par.map(_.apply(i)).toArray))
  }

  //заменяем столбец матрицы на другой
  def forAll(x: MatrixComplex, n: Int, change: MatrixComplex) = {
    (0 until x.row_count).par.foreach {
      i => x.row(i)(n) = change.row(0)(i)
    }
  }

  //сортировка с запоминанием индексов
  def sort(x: MatrixComplex): (MatrixComplex, Matrix) = {
    val leng = length(x)
    val result = new mutable.MutableList[Array[Complex]]()
    val ind = new mutable.MutableList[Array[Double]]()
    (0 until x.cols).par.foreach {
      i => {
        val col = allRows(x, i)
        val acc = (col.data.zipWithIndex.sortBy(_._1.re))
        result.+=(acc.unzip._1.toArray)
        ind.+=(acc.unzip._2.map(_.toDouble).toArray)
      }
    }
    (new MatrixComplex(result.toArray).T, new Matrix(ind.toArray))
  }

  def length(x: MatrixComplex) = math.max(x.row_count, x.cols)

  //добавление вектора или матрицы к исходной
  def repeat(x: MatrixComplex, n: Int, y: MatrixComplex): MatrixComplex = {
    if (n > 0) y :: (x.T) else x.T

  }

  def toComplex(x: Matrix) = {
    new MatrixComplex(x.items.map(_.map(y => Complex(y, .0))))
  }

  def toReal(x: MatrixComplex) = {
    new Matrix(x.items.map(_.map(y => y.re)))
  }

  //выборка элементов по матрице индексов
  def select(x: MatrixComplex, y: Matrix): MatrixComplex = {
    val acc = allRows(x, 0)
    val tik = acc.items(0)
    val yTemp = y.items.map(_.map(_.toInt))
    val res = for (i <- 0 until yTemp.length) yield (yTemp(i).foldLeft(List[Complex]())((y, c) => y ::: List(tik(c)))).toArray
    new MatrixComplex(res.toArray)
  }

  //выход
  def closes() = {
    System.exit(1)
  }

}


