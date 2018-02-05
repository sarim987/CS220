import HOF._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  test("map2 with add") {
    def add(x: Int, y: Int): Int = x + y
    //assert(map2(add, List(), List()) == List())
    assert(map2(add, List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    //def subtract(x: Int, y: Int): Int = x - y
    //assert(map2(subtract, List(4, 5, 6), List(1, 2, 3)) == List(3, 3, 3))
  }

  test("zip test 1") {
    assert(zip(List(1, 2, 3), List(4, 5, 6)) == List((1,4), (2, 5), (3, 6)))
  }

  test("zip test 2") {
    assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) ==
          List(("George", "Washington"), ("Teddy", "Roosevelt")))
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
    assert(flatten(List(List(List()))) == List())
  }
  test("flatten3 test 2") {
    assert(flatten(List(List(List(1)))) == List(1))
  }
  test("flatten3 test 3") {
    assert(flatten(List(List(List(1, 2), List(3, 4)))) == List(1, 2, 3, 4))
  }

  test("buildList test") {
    def f(x: Int) = x
    assert(buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("mapList test") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(1, 2, 3), f) == List(1, 2, 2, 3, 3, 3))
  }
    test("mapList test2") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(2, 3, 4), f) == List(2,2,3,3,3, 4, 4, 4, 4))
  }
    test("mapList test3") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    assert(mapList(List(3, 4, 5), f) == List(3,3,3,4,4, 4, 4, 5, 5, 5, 5, 5))
  }


  def isEven(n: Int): Boolean = n % 2 == 0

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

def lt(x: Int, y: Int): Boolean = x < y

test("merge test 1") {
  assert(merge(lt, List(1,3,5), List(0,6,10)) == 
         List(0,1,3,5,6,10))
}

test("sort test 1") {
  assert(sort(lt, List(5,1,2,3,4,5)) == 
         List(1,2,3,4,5,5))
}

}