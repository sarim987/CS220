import HOF._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  test("map2 with add test 1") {
    def add(x: Int, y: Int): Int = x + y
    assert(map2(add, List(), List()) == List())
    assert(map2(add, List(1), List(2)) == List(3))
    assert(map2(add, List(1, 5), List(1, 5)) == List(2, 10))
    assert(map2(add, List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(map2(add, List(1, 1, 1, 5), List(1, 1, 1, -5)) == List(2, 2, 2, 0))
    assert(map2(add, List(-1), List(-2)) == List(-3))
  }
  test("map2 with sub test 2") {
    def subtract(x: Int, y: Int): Int = x - y
    assert(map2(subtract, List(), List()) == List())
    assert(map2(subtract, List(4, 5, 6), List(1, 2, 3)) == List(3, 3, 3))
    assert(map2(subtract, List(0, 0, 0), List(-1, -2, -3)) == List(1, 2, 3))
  }
  test("map2 with concat test 3") {
    def concat(x: String, y: String): String = x + y
    assert(map2(concat, List(), List()) == List())
    assert(map2(concat, List("Hi "), List("There")) == List("Hi There"))
  }
  test("zip test 1") {
    assert(zip(List(1, 2, 3), List(4, 5, 6)) == List((1,4), (2, 5), (3, 6)))
  }

  test("zip test 2") {
    assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) ==
          List(("George", "Washington"), ("Teddy", "Roosevelt")))
  }
  test("zip test 3") {
    assert(zip(List(), List()) == List())
  }
  test("flatten test") {
    assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
  }
  test("flatten test 1") {
    assert(flatten(List(List())) == List())
  }
  test("flatten test 2") {
    assert(flatten(List(List(1), List(2))) == List(1, 2))
  }
  test("flatten test 3") {
    assert(flatten(List(List(), List(1, 2, 3), List(4, 5, 6), List(7))) == List(1, 2, 3, 4, 5, 6, 7))
  }
  test("flatten test 4") {
    assert(flatten(List(List(1))) == List(1))
  }
  test("flatten3 test 1") {
    assert(flatten3(List(List(List()))) == List())
  }
  test("flatten3 test 2") {
    assert(flatten3(List(List(List(1)), List(List(1), List(1)))) == List(1, 1, 1))
  }
  test("flatten3 test 3") {
    assert(flatten3(List(List(List(1, 2), List(3, 4)))) == List(1, 2, 3, 4))
  }
  test("flatten3 test 4") {
    assert(flatten3(List(List(List(1, 2), List(3, 4), List(5, 6, 7)))) == List(1, 2, 3, 4, 5, 6, 7))  
  }
  test("flatten3 test 5") {
    assert(flatten3(List(List(List(1, 2), List(3, 4)))) == List(1, 2, 3, 4))
  }  
  test("flatten3 test 6") {
    assert(flatten3(List(List(List("I", "am"), List("really", "tired", "of doing this")))) == List("I", "am", "really", "tired", "of doing this"))
  }  
  test("buildList test 1") {
    def f(x: Int) = x
    assert(buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
  test("buildList test 2") {
    def f(x: Int) = x + "A"
    assert(buildList(3, f) == List("0A", "1A", "2A"))
  }
  test("buildList test 3") {
    def f(x: Int) = x
    assert(buildList(0, f) == List())
  }
  test("buildList test 4") {
    def f(x: Int) = x
    assert(buildList(1, f) == List(0))
  }
  test("buildList test 5") {
    def f(x: Int) = x % 2
    assert(buildList(5, f) == List(0, 1, 0, 1, 0))
  }
  test("mapList test 1") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(), f) == List())
  }
 test("mapList test 2") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(1, 2, 3), f) == List(1, 2, 2, 3, 3, 3))
  }
   test("mapList test 3") {
    def f(x: String): List[String] = List("CS" + x)
    assert(mapList(List("220", "230", "240", "250"), f) == List("CS220", "CS230", "CS240", "CS250"))
  }
    test("mapList test 4") {
    def f(n: Int): List[Boolean] = List(false)
    assert(mapList(List(2, 3, 4), f) == List(false, false, false))
  }
    test("mapList test 5") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(3, 4, 5), f) == List(3,3,3,4,4, 4, 4, 5, 5, 5, 5, 5))
  }


  def isEven(n: Int): Boolean = n % 2 == 0
  def isOdd(n: Int): Boolean = n % 2 == 1
  test("partition test 0") {
    assert(partition(isEven, List()) == (List(), List()))
  }
    test("partition test 01") {
    assert(partition(isEven, List(1)) == (List(), List(1)))
  }

  test("partition test 1") {
    assert(partition(isEven, List(1,2,3,4,5,6)) == (List(2,4,6), List(1,3,5)))
  }

  test("partition test 2") {
    assert(partition(isEven, List(2,4,6)) == (List(2,4,6), Nil))
  }

  test("partition test 3") {
    assert(partition(isEven, List(1,3,5)) == (Nil, List(1,3,5)))
  }
  test("partition test 4") {
    assert(partition(isEven, List(1, 2)) == (List(2), List(1)))
  }
  def isStartingWithS(n: String): Boolean = n.substring(0, 1) == "S"
  test("partition test 5") {
    assert(partition(isStartingWithS, List("Apple", "Google", "Sarim", "Tesla", "Sprint", "Samsung")) == (List("Sarim","Sprint","Samsung"), List("Apple", "Google", "Tesla")))
  }
  test("partition test 6") {
    assert(partition(isOdd, List(1, 2)) == (List(1), List(2)))
  }
  

def lt(x: Int, y: Int): Boolean = x < y
def startS(x: String, y: String): Boolean = x.substring(0, 1) == "S"

test("merge test 1") {
  assert(merge(startS, List("Sarim","Sprint","Samsung"), List("Apple", "Google", "Tesla")) == 
         List("Sarim","Sprint","Samsung", "Apple", "Google", "Tesla"))
  }
test("merge test 2") {
  assert(merge(lt, List(1,3,5), List(0,6,10)) == 
         List(0,1,3,5,6,10))
  }
  test("merge test 3") {
  assert(merge(lt, List(), List()) == 
         List())
  }
    test("merge test 4") {
  assert(merge(lt, List(1), List(2, 3)) == 
         List(1, 2, 3))
  }



test("sort test 1") {
  assert(sort(lt, List(5,1,2,3,4,5)) == 
         List(1,2,3,4,5,5))
}
test("sort test 2") {
  assert(sort(startS, List("E","R","T","G","S","S")) == 
         List("S", "S", "G", "T", "R", "E"))
}
test("sort test 3") {
  assert(sort(startS, List()) == 
         List())
}

}