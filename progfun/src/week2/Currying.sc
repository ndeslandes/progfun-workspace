package week2

object Currying {
  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)     //> sum: (f: Int => Int)(a: Int, b: Int)Int

  sum(x => x * x)(1, 10)                          //> res0: Int = 385

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1 else f(a) * product(f)(a + 1, b) //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x * x)(3, 4)                       //> res1: Int = 144

  def fact(n: Int): Int = product(x => x)(1, n)   //> fact: (n: Int)Int

  fact(5)                                         //> res2: Int = 120

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int

  def newProduct(f: Int => Int)(a: Int, b: Int): Int = mapReduce((x) => x * x, (x, y) => x * y, 1)(a, b)
                                                  //> newProduct: (f: Int => Int)(a: Int, b: Int)Int

  newProduct(x => x * x)(3, 4)                    //> res3: Int = 144

  def newSum(f: Int => Int)(a: Int, b: Int): Int = mapReduce((x) => x * x, (x, y) => x + y, 0)(a, b)
                                                  //> newSum: (f: Int => Int)(a: Int, b: Int)Int

  newSum(x => x * x)(1, 10)                       //> res4: Int = 385
}