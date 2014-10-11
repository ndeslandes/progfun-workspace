package week2

object rationals {
  val x = new Rational(1, 2)                      //> x  : week2.Rational = 1/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2

  def addRational(r: Rational, s: Rational): Rational =
    new Rational(r.numer * s.denom + s.numer * r.denom, r.denom * s.denom)
                                                  //> addRational: (r: week2.Rational, s: week2.Rational)week2.Rational

  def makeString(r: Rational) =
    r.numer + "/" + r.denom                       //> makeString: (r: week2.Rational)String

  makeString(addRational(new Rational(1, 2), new Rational(2, 3)))
                                                  //> res2: String = 7/6

  val y = new Rational(2, 3)                      //> y  : week2.Rational = 2/3
  x + y                                           //> res3: week2.Rational = 7/6

  -x                                              //> res4: week2.Rational = 1/-2

  x - y                                           //> res5: week2.Rational = 1/-6

  val x1 = new Rational(1, 3)                     //> x1  : week2.Rational = 1/3
  val y1 = new Rational(5, 7)                     //> y1  : week2.Rational = 5/7
  val z1 = new Rational(3, 2)                     //> z1  : week2.Rational = 3/2

  x1 - y1 - z1                                    //> res6: week2.Rational = -79/42

  y1 + y1                                         //> res7: week2.Rational = 10/7

  x1 < y1                                         //> res8: Boolean = true

  x1 max y1                                       //> res9: week2.Rational = 5/7

  new Rational(2)                                 //> res10: week2.Rational = 2/1

}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def +(that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational): Rational = this + -that

  override def toString = {
    numer + "/" + denom
  }
}