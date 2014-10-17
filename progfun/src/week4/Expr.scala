package week4

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2:Expr) extends Expr

object expr {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l,r) => show(l) + " + " + show(r)
  }
}