import Lists._

class TestSuite extends org.scalatest.FunSuite {

  test("oddNumbers properly defined") {
    assert(oddNumbers == List(1, 3, 5))
  }

  test("sumDouble") {
    assert(sumDouble(List()) == 0)
    assert(sumDouble(List(0)) == 0)
    assert(sumDouble(List(0, 1)) == 2)
    assert(sumDouble(List(0, 1, 2)) == 6)
    assert(sumDouble(List(0, 1, 2, 3, -1, -2, -3)) == 0)
    assert(sumDouble(List(-1, -2)) == -6)
  }

  test("removeZeroes") {
    assert(removeZeroes(List()) == List())
    assert(removeZeroes(List(0)) == List())
    assert(removeZeroes(List(0, 1)) == List(1))
    assert(removeZeroes(List(0, -1, 0)) == List(-1))
    assert(removeZeroes(List(0, 1, 2, 3, 0, -2, 0)) == List(1, 2, 3, -2))
    assert(removeZeroes(List(0, 0, 0, 0, 0, 0)) == List())
  }

  test("countEvens") {
    assert(countEvens(List()) == 0)
    assert(countEvens(List(0)) == 1)
    assert(countEvens(List(0, -2)) == 2)
    assert(countEvens(List(-2, -3, -4, 0, 1, 2, 3)) == 4)
    assert(countEvens(List(0, 1, 2, 3, 0, -2, 0)) == 5)
    assert(countEvens(List(2, 4, 6, 8, 10, -12, 13)) == 6)

  }

  test("removeAlternating") {
    assert(removeAlternating(List("A", "B")) == List("A")) 
    assert(removeAlternating(List("A", "B")) != List("B"))
    assert(removeAlternating(List()) == List())
    assert(removeAlternating(List("0")) == List("0"))
    assert(removeAlternating(List("0", "-2")) == List("0"))
    assert(removeAlternating(List("-2", "-3", "-4", "0", "1", "2", "3")) == List("-2", "-4", "1", "3"))
    assert(removeAlternating(List("Ant", "Bat", "Cat", "Dog", "Elephant", "Fox")) == List("Ant", "Cat", "Elephant"))

  }


  test("isAscending") {
    assert(isAscending(List()) == true)
    assert(isAscending(List(-1)) == true)
    assert(isAscending(List(-1, 0)) == true)
    assert(isAscending(List(0, -1)) == false)
    assert(isAscending(List(0, 0, 0)) == true)
    assert(isAscending(List(0, 0, 0, -1)) == false)
    assert(isAscending(List(-1, -1, 0)) == true)
    assert(isAscending(List(-1, -1, -1)) == true)
    assert(isAscending(List(4, 4, -1, 4)) == false)
    assert(isAscending(List(0, 0, -2)) == false)
    assert(isAscending(List(-3, -2, -1)) == true)
    assert(isAscending(List(-2, -3, -4, 0, 1, 2, 3)) == false)
    assert(isAscending(List(0, 1, 2, 3, 4, 6265, 1205678)) == true)
    assert(isAscending(List(2, 4, 6, 8, 10, -12, 13)) == false)
    assert(isAscending(List(2, 4, 6, 8, 10, 12, 13)) == true)

  }

  test("addSub") {
    assert(addSub(List()) == 0)
    assert(addSub(List(-1)) == -1)
    assert(addSub(List(0)) == 0)
    assert(addSub(List(0, -2)) == 2)
    assert(addSub(List(1, 2)) == -1)
    assert(addSub(List(-3, -2, -1)) == -2)
    assert(addSub(List(0, 2, 4)) == 2)
    assert(addSub(List(0, 2, 4, 2)) == 0)
    assert(addSub(List(-2, -3, -4, 0, 1, 2, 3)) == -1)
    assert(addSub(List(2, -3, 4, -5, 6, -7)) == 27)

  }

  test("alternate") {
    assert(alternate(List(), List()) == List())
    assert(alternate(List(1), List(2)) == List(1, 2))
    assert(alternate(List(1), List(1)) == List(1, 1))
    assert(alternate(List(1, 3), List(2, 4)) == List(1, 2, 3, 4))
    assert(alternate(List(1, 3, 5), List(2, 4, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(alternate(List(-1, 3, -5, 0), List(-2, -4, 6, 0)) == List(-1, -2, 3, -4, -5, 6, 0, 0))
  }

  test("fromTo") {
    assert(fromTo(0, 1) == List(0))
    assert(fromTo(-1, 0) == List(-1))
    assert(fromTo(-1, 1) == List(-1, 0))
    assert(fromTo(-3, 0) == List(-3, -2, -1))
    assert(fromTo(9, 13) == List(9, 10, 11, 12))
    assert(fromTo(-3, 5) == List(-3, -2, -1, 0, 1, 2, 3, 4))
  }
  
  test("insertOrdered") {
    assert(insertOrdered(0, List()) == List(0))
    assert(insertOrdered(0, List(0)) == List(0, 0))
    assert(insertOrdered(2, List(0, 1)) == List(0, 1, 2))
    assert(insertOrdered(-9, List(0, 1)) == List(-9, 0, 1))
    assert(insertOrdered(2, List(1, 2)) == List(1, 2, 2))
    assert(insertOrdered(0, List(5, 6)) == List(0, 5, 6))
  }

  test("sort") {
    assert(sort(List()) == List())
    assert(sort(List(0)) == List(0))
    assert(sort(List(-2)) == List(-2))
    assert(sor(List(2, 1, 0)) == List(0, 1, 2))
    assert(sort(List(0, 1, 2)) == List(0, 1, 2))
    assert(sort(List(0, 0, 0, 0, 1, 0, 0)) == List(0, 0, 0, 0, 0, 0, 1))
    assert(sort(List(-1, 6, -3, 64, 324, -4, 1, 0)) == List(-4, -3, -1, 0, 1, 6, 64, 324))
  }
  

}