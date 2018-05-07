import hw.parsing._
import ArithEval._
import ArithParser._
import ArithPrinter._

class TrivialTestSuite extends org.scalatest.FunSuite {

  test("several objects must be defined") {
    val parser: hw.parsing.ArithParserLike = ArithParser
    val printer: hw.parsing.ArithPrinterLike = ArithPrinter
    val eval: hw.parsing.ArithEvalLike = ArithEval
    
  }

  test("test1"){
    assert(eval(parseArith("1+2/2+5*(2^2)+7-7/1*(18-17-1+5)/5*72/8+18/2*2^3-18")) == eval(Num(20)))
  }
  test("test1.5"){
    assert(parseArith("1") == Num(1))
  }
  test("test2"){
    assert(print(Num(1)) == "1.0")
    assert(eval(Add(Num(1),Num(1))) == eval(Num(2)))
  }
  test("test3"){
    assert(eval(parseArith("2^2")) == eval(Num(4)))
  }


}