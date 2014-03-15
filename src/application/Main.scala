package application

import matrix.Matrix


/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 09.10.12
 * Time: 14:25
 * To change this template use File | Settings | File Templates.
 */
object Main extends App{

//  %   Ax = lambda*B*X общая проблема
//  %матрица жесткости
//  A = [2 -1 0 0;-1 2 -1 0; 0 -1 2 -1; 0 0 -1 1]
//  %матрица масс ансамблей собственных значений
//    B = diag([0 2 0 1])

//System.setProperty("jblas_arch_flavor","C:\\Users\\Aleksandr\\Desktop\\СПО\\IterEigen\\jblas_arch_flavor.dll")
//System.setProperty("jblas","C:\\Users\\Aleksandr\\Desktop\\СПО\\IterEigen\\jblas.dll")
  val A = Array(Array(2,-1,0,0),Array(-1,2,-1,0),Array(0,-1,2,-1),Array(0,0,-1,1))
  val b = Matrix.fromFile("B.txt")
  val a = Matrix.fromFile("A.txt")
  println(b)
  val (e:Matrix,v:Matrix) = IterateEigen.subspaceIter(a,b,-0.01,0.001,10)
 //val (e:Matrix,v:Matrix) = IterateEigen.sort(a)
  println(e.toString)
  println(v.toString)

  //println((e.T * a * e).toString)

  /*
  * a =

    0.2500   -0.2500
    0.5000   -0.5000
    0.6036    0.1036
    0.7071    0.7071


b =

    0.1464
    0.8536

  * */


}
