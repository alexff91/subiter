package matrix

/**
 *for later use
 * @param re  the real part
 * @param im  the imaginary part
 */
case class Complex (re: Double, im: Double = 0.)
  extends Numeric [Complex]
  with Fractional [Complex]
  with Ordered [Complex]
{

  /**
   * Compute the unary minus (-).
   */
  def unary_- () = Complex (-re, -im)

  def negate (c: Complex) = -c


  /**
   * Add two complex numbers.
   * @param c  add c to this
   */
  def + (c: Complex) = Complex (re + c.re, im + c.im)

  def plus (c: Complex, d: Complex) = c + d

  /**
   * Substract two complex numbers.
   * @param c  subtract c from this
   */
  def - (c: Complex) = Complex (re - c.re, im - c.im)

  def minus (c: Complex, d: Complex) = c - d


  /**
   * Multiply two complex numbers.
   * @param c  multiply this times c
   */
  def * (c: Complex) = Complex (re * c.re - im * c.im, re * c.im + im * c.re)

  def times (c: Complex, d: Complex) = c * d

  /**
   * Divide two complex numbers.
   * @param c  divide this by c
   */
  def / (c: Complex) = Complex ((re * c.re + im * c.im) / (c.re * c.re + c.im * c.im),
    (im * c.re - re * c.im) / (c.re * c.re + c.im * c.im))

  def div (c: Complex, d: Complex) = c / d

  /**
   * Compute the complex conjugate: if z = (a + bi) then z.bar = (a - bi).
   */
  def bar = Complex (re, -im)


  /**
   * Determine whether the complex number is real (no imaginary part).
   */
  def isReal = im == 0


  /**
   * Compare two complex numbers (negative for <, zero for ==, positive for >).
   * @param c  the first complex number to compare
   * @param d  the second complex number to compare
   */
  def compare (c: Complex, d: Complex) =
  {
    if (c.re == d.re) c.im compare d.im else c.re compare d.re
  } // compare


  def compare (d: Complex) =
  {
    if (re == d.re) im compare d.im else re compare d.re
  } // compare


  def toDouble (c: Complex) = re


  def toFloat (c: Complex) = re.asInstanceOf [Float]


  def toLong (c: Complex) = re.asInstanceOf [Long]


  def toInt (c: Complex) = re.asInstanceOf [Int]


  def fromInt (n: Int) = Complex (n, 0.)


  override def toString = if(im!=0.0) "( " + re + " , " + im + "i )" else re.toString

} // Complex class

object ComplexTest extends Application
{

  val c = Complex (2., 0.)
  val d = Complex (4., 5.)
  println ("c = " + c)
  println ("d = " + d)
  println ("c + d = " + (c + d))
  println ("c - d = " + (c - d))
  println ("c * d = " + (c * d))
  println ("c / d = " + (c / d))

  //val v = Vec(c, d)
  //println ("v = " + v)
  //val u = Vec(2)
  //println ("u = " + u)

  //val cm = new MatrixN [Complex] (2, Complex (1.), Complex (0.))    // 2 by 2 identity matrix
  //println ("cm = " + cm)

} // ComplexTest