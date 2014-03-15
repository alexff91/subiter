package application

import matrix.Matrix

/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 20.11.12
 * Time: 15:46
 * All rights recieved.(c)
 * Генератор матриц - симметричных с шириной ленты задаваемой пользоваетелем
 * */

class Generator(n: Int, m: Int) {
  def generateMatrix(): Matrix = {
    //заполняем рандомными числами ленту,чтобы матрица была симметрична
    val r = new java.util.Random();
    val rang = (0 until m / 2 + 1).map(_ + r.nextInt(40).toDouble)
    val range = rang.tail.reverse.toList ::: rang.toList

    val myMatrix = {for (i <- 0 until n) yield {
      for (j <- 0 until n) yield {

        val left = (i - m / 2)
        val right = (i + m / 2)
        if (j <= right && j >= i) if(i == j) i*2+1.0 else range(m / 2 + (i - j))
        else
        if (j >= left && j < i) range(m / 2 - (i - j))
        else 0

      }
    }.toArray}.toArray
    new Matrix(myMatrix)
  }

  def eye(size: Int): Matrix =
    new Matrix({
      for (i <- 0 until n) yield {
        for (j <- 0 until size) yield if (i == j) 1.0 else 0.0
      }.toArray
    }.toArray)

}
