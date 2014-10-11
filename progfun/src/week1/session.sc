package week1

object session {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(0.001)                                     //> res0: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res1: Double = 3.1633394544890125E-11
  sqrt(1.0e20)                                    //> res2: Double = 1.0000021484861237E10
  sqrt(1.0e50)                                    //> res3: Double = 1.0000003807575104E25

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }                                               //> factorial: (n: Int)Int

  factorial(4)                                    //> res4: Int = 24
}