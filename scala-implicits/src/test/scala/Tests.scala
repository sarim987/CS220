class Tests extends org.scalatest.FunSuite {

  import PathImplicits._
  import TimeImplicits._

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

}
