import HOF._

class TestSuite extends org.scalatest.FunSuite {
test("map2 with add") {
    def subtract(x: Int, y: Int): Int = x - y
    HOF.map2(subtract, List(4, 5, 6), List(1, 2, 3)) == List(3, 3, 3)
}
test("map23 with add") {
    HOF.zip(Nil, Nil) == Nil
    //HOF.zip(List(1, 0) == List(true, false))
    //HOF.zip(List(1, 0, 5, 6), Nil) == List(3, 3, 3)
}
def isEven(n: Int): Boolean = n % 2 == 0
test("partition test 4") {
    HOF.partition(isEven, List(1, 2)) == (List(5, 8), List(6))
}

}