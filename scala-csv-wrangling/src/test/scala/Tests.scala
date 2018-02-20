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
    test("count"){
        val s = SSARow(1880, "Jane", Female(), 4000)
        val a = SSARow(1809, "Rando", Female(), 2000)
        val r = SSARow(1809, "Rando", Female(), 2000)
        val i = SSARow(1809, "Rando", Female(), 2000)
        val m = SSARow(1809, "Rando", Female(), 2000)
        assert(count(List(s, a, r, i, m)) == 12000)
    }
    test("countGirlsBoys"){
        val s = SSARow(1880, "Mary", Female(), 7000)
        val a = SSARow(1909, "Jane", Female(), 3000)
        val r = SSARow(1900, "John", Male(), 3500)
        val u = SSARow(1904, "William", Male(), 9000)
        assert(countGirlsAndBoys(List(a, s, r, u)) == (10000, 12500))
    }
    test("genderNeutral"){
        val s = SSARow(1880, "Pam", Female(), 7000)
        val q = SSARow(1880, "Phil", Female(), 7000)
        val w = SSARow(1880, "Phil", Male(), 7000)
        val a = SSARow(1909, "Jane", Female(), 3000)
        val r = SSARow(1900, "Pam", Male(), 3500)
        val u = SSARow(1904, "William", Male(), 9000)
        assert(genderNeutralNames(List(a, s, w, r, u, q)) == Set("Pam", "Phil"))
    }
    test("expectedAlive"){
        val n = CDCRow(1930, 58, 62)
        val s = CDCRow(1948, 61, 65)
        val r = CDCRow(1997, 74, 79)
        assert(expectedAlive(Male(), 1930, 2018, List(n, s, r)) == false)
    }
    test("estimatePopulation"){
        val n = CDCRow(1930, 58, 62)
        val p = CDCRow(1934, 58, 62)
        val u = CDCRow(1958, 61, 65)
        val y = CDCRow(1997, 74, 79)
        val s = SSARow(1920, "Pam", Male(), 7000)
        val q = SSARow(1934, "Phil", Male(), 7000)
        val w = SSARow(1958, "Philly", Female(), 7000)
        val a = SSARow(1997, "Jane", Female(), 3000)
        val cdcr = List(n, p, u, y)
        val ssr = List(a, s, w, q)
        assert(estimatePopulation(ssr, 2018, cdcr) == 10000)
    }

}