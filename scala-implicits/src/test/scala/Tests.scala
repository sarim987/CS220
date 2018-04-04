class Tests extends org.scalatest.FunSuite {

  import PathImplicits._
  import TimeImplicits._
  import java.time.LocalDate;
  import java.nio.file._

  test("Path Test 1"){
    assert(Paths.get("home", "user") == "home"/"user")
  }

  test("Path Test 2"){
    val p1 = "user"/"local"
    val p2  = "bin"/"scala"
    assert(Paths.get("user", "local", "bin", "scala") == p1 / p2)
  }

  test("Write") {
    Paths.get("greeting.txt").write("Hello World\n")
  }
  
  test("Read") {
    assert(Paths.get("greeting.txt").read().equals("Hello World\n"))
  }

  test("Append") {
    Paths.get("greeting2.txt").append("Hello There!\n")
  }

  test("Date") {
    assert(12.jan == LocalDate.of(2018, 1, 12))
    assert(12.jan(2018) == LocalDate.of(2018, 1, 12))
    val a = 2 jan 2018
    assert(a == LocalDate.of(2018, 1, 2))

  }
  test("addDays") {
    //val a = LocalDate.of(2018, 1, 12)
    //assert(a.days(2) == LocalDate.of(2018, 1, 14))
    //val a = 10 days
    //assert(a == 10)
  }
  test("PlusDays") {
    val a = LocalDate.of(2018, 1, 12)
    val b = a + 10.days
    assert(b == LocalDate.of(2018, 1, 22))
  }
    test("PlusMonths") {
    val a = LocalDate.of(2018, 1, 12)
    val b = a + 10.months
    assert(b == LocalDate.of(2018, 11, 12))
  }
      test("years") {
    val a = LocalDate.of(2018, 1, 12)
    val b = a + 2.years
    assert(b == LocalDate.of(2020, 1, 12))
  }

}
