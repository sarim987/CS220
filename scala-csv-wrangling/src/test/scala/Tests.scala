import hw.csv._
import Main._

class TestSuite extends org.scalatest.FunSuite {
    test("readSSARow Test 1"){
        val s = SSARow(1880, "Mary", Female(), 7065)
        assert(readSSARow(List("1880", "Mary", "F", "7065")) == s)
    }
    test("readCDCRow Test 1"){
        val s = CDCRow(2010, 76, 81)
        assert(readCDCRow(List("2010", "76", "81")) == s)
    }
    test("YearIs"){
        val s = SSARow(1880, "Mary", Female(), 7064)
        val r = SSARow(1900, "Mary", Female(), 3244)
        assert(yearIs(List(s, r), 1900) == List(r))
    }
    test("YearGT"){
        val s = SSARow(1880, "Mary", Female(), 7064)
        val r = SSARow(1900, "Mary", Female(), 3244)
        assert(yearGT(List(s, r), 1890) == List(r))
    }
    test("YearLT"){
        val s = SSARow(1880, "Mary", Female(), 7064)
        val r = SSARow(1900, "Mary", Female(), 3244)
        assert(yearLT(List(s, r), 1890) == List(s))
    }
    test("YearOnlyName"){
        val s = SSARow(1880, "Mary", Female(), 7064)
        val r = SSARow(1900, "John", Female(), 3244)
        assert(onlyName(List(s, r), "John") == List(r))
    }
    test("mostPopular"){
        val s = SSARow(1880, "Mary", Female(), 7064)
        val a = SSARow(1909, "Jane", Female(), 3457)
        val r = SSARow(1900, "John", Female(), 3244)
        val t = SSARow(1901, "John", Female(), 4245)
        val u = SSARow(1904, "William", Female(), 9432)
        val w = SSARow(1919, "Jane", Female(), 2469)
        val x = SSARow(1910, "Jane", Female(), 5457)
        assert(mostPopular(List(a, s, r, t, u, w, x)) == ("Jane", 11383))
    }
    test("findMax"){
        val s = SSARow(1880, "Jane", Female(), 10)
        val a = SSARow(1909, "Jane", Female(), 20)
        assert(findMax(List(s, a)) == ("Jane", 30))
    }

}