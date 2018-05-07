import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
  def eval(e: Expr): Double = e match {
    case Num(x) => x
    case Exponent(x, y) => Math.pow(eval(x), eval(y))
    case Mul(x, y) => eval(x) * eval(y)
    case Div(x, y) => eval(x) / eval(y)
    case Add(x, y) => eval(x) + eval(y)
    case Sub(x, y) => eval(x) - eval(y) 
  }
}

object ArithParser extends ArithParserLike {

  // number: PackratParser[Double] is defined in ArithParserLike

  lazy val atom: PackratParser[Expr] = "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e } | number ^^ { case x => Num(x) }

  lazy val exponent: PackratParser[Expr] = exponent ~ "^" ~ atom ^^ { case e ~ _ ~ a => Exponent(e, a) } | atom

  lazy val add: PackratParser[Expr] = add ~ "+" ~ mul ^^ { case a ~ _ ~ m => Add(a, m) } |
                                      add ~ "-" ~ mul ^^ { case a ~ _ ~ m => Sub(a, m) } | mul

  lazy val mul: PackratParser[Expr] = mul ~ "*" ~ exponent ^^ { case m ~ _ ~ e => Mul(m, e) } |
                                      mul ~ "/" ~ exponent ^^ { case m ~ _ ~ e => Div(m, e) } | exponent

  lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike {
  def print(e: Expr): String = e match {
    case Num(x) => x.toString()
    case Exponent(x, y) => x.toString() + "^" + y.toString()
    case Mul(x, y) => x.toString() + "*" + y.toString()
    case Div(x, y) => x.toString() + "/" + y.toString()
    case Add(x, y) => x.toString() + "+" + y.toString()
    case Sub(x, y) => x.toString() + "-" + y.toString()
  }
}