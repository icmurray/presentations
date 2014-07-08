sealed trait Expr
case class Constant(v: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr

object Expr {

  def eval(e: Expr): Int = e match {
    case Constant(value) => value
    case Add(l, r)       => eval(l) + eval(r)
  }

  def simplify(e: Expr): Expr = e match {
    case Add(Constant(0), r) => simplify(r)
    case Add(l, Constant(0)) => simplify(l)
    case Add(l, r)           => Add(simplify(l), simplify(r))
    case otherwise           => otherwise
  }

}
