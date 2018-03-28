class Tests extends org.scalatest.FunSuite {

  import Main._

  test("Implemented the Solution interface") {
    val main: hw.streams.SolutionLike = Main
  }

  test("From") {
    val ones = from(0)
    assert(ones.next()._1 == 0)
  }

  test("Map") {
    val ones = from(1)
    def hey(x: Int): String = x.toString() + "hey"
    
    val stream = map(hey, ones)
    assert(stream.next()._1 == "1hey")
    assert(stream.next()._2.next()._1 == "2hey")
  }

  test("power") {
    val ones = from(0)
    val stream = pow


    assert(stream.next()._1 == 1)
    assert(stream.next()._2.next()._1 == 2)
    assert(stream.next()._2.next()._2.next()._1 == 4)
    assert(stream.next()._2.next()._2.next()._2.next()._1 == 8)
  }

  test("Filter") {
    val ones = from(1)
    def hey(x: Int): String = x.toString() + "hey"
    def isEven(n: Int): Boolean = n % 2 == 0
    val stream = filter(isEven, ones)
    assert(stream.next()._1 == 2)
    assert(stream.next()._2.next()._1 == 4)
    assert(stream.next()._2.next()._2.next()._1 == 6)
    assert(stream.next()._2.next()._2.next()._2.next()._1 == 8)

  }
    test("Sift") {
    val ones = from(0)
    
    
    val stream = sift(2, ones)


    assert(stream.next()._1 == 1)
    assert(stream.next()._2.next()._1 == 3)
    assert(stream.next()._2.next()._2.next()._1 == 5)
    assert(stream.next()._2.next()._2.next()._2.next()._1 == 7)

  }

  test("interleave") {
    val one = from(0)
    val two = from(0)
    val stream = interleave(one, two)
    //assert(nth(one, 0) == 0)

    assert(stream.next()._1 == 0)
    assert(stream.next()._2.next()._1 == 0)
    assert(stream.next()._2.next()._2.next()._1 == 1)
    assert(stream.next()._2.next()._2.next()._2.next()._1 == 1)
    assert(stream.next()._2.next()._2.next()._2.next()._2.next()._1 == 2)

  }

    test("total") {
    //val one = from2(0.0)
    
    //val stream = total(one)
    //assert(nth(one, 0) == 0)


    //assert(stream.next()._1 == 0.0)
    //assert(stream.next()._2.next()._1 == 1.0)
    //assert(stream.next()._2.next()._2.next()._1 == 3.0)
    //assert(stream.next()._2.next()._2.next()._2.next()._1 == 6.0)
    //assert(stream.next()._2.next()._2.next()._2.next()._2.next()._1 == 10.0)

  }

      test("prime") {
    val one = from(0)
    
    val stream = prime
    //assert(nth(one, 0) == 0)

    print(stream.next()._1)
    print(stream.next()._2.next()._1)
    print(stream.next()._2.next()._2.next()._1)
    print(stream.next()._2.next()._2.next()._2.next()._1)
    print(stream.next()._2.next()._2.next()._2.next()._2.next()._1)

    assert(stream.next()._1 == 2)
    assert(stream.next()._2.next()._1 == 3)
    assert(stream.next()._2.next()._2.next()._1 == 5)
    assert(stream.next()._2.next()._2.next()._2.next()._1 == 7)
    assert(stream.next()._2.next()._2.next()._2.next()._2.next()._1 == 11)
    assert(stream.next()._2.next()._2.next()._2.next()._2.next()._2.next()._1 == 13)
    assert(stream.next()._2.next()._2.next()._2.next()._2.next()._2.next()._2.next()._1 == 17)

  }

}
