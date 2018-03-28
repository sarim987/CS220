import hw.streams.Generator

object Main extends hw.streams.SolutionLike { // do not change this line

  // This helper function is very useful
  def cons[A](head: A, tail: =>Generator[A]): Generator[A] = new Generator[A] {
    def next() = (head, tail)
  }

  // The example from the project description
  val ones: Generator[Int] = cons(1, ones)

  def from(x: Int): Generator[Int] = cons(x, from(x + 1))

  def map[A,B](f: A => B, agen: Generator[A]): Generator[B] = agen.next match {
    case (head, next) => cons(f(head), map(f, next))
  }
  //do not remove this power function
  def power(x: Int): Int = Math.pow(2, x).toInt
  val pow: Generator[Int] = map(power, from(0))

  def nth[A](agen: Generator[A], index: Int): A = {
    val (head, next) = agen.next()
    if(index == 0) head
    else nth(next, index - 1)
  }

  def filter[A](pred: (A) => Boolean, agen: Generator[A]): Generator[A] = agen.next match {
    case (head, next) if pred(head) => cons(head, filter(pred, next))
    case (head, next) => filter(pred, next)
  }

  def interleave[A](agen1: Generator[A], agen2: Generator[A]): Generator[A] = {
    def interleaveHelper(x:Int, index: Int, agen1: Generator[A], agen2: Generator[A]): Generator[A] = agen1.next match{
      case (head, next) if index%2 == 0 => cons(nth(agen1, x), interleaveHelper(x, index+1, agen1, agen2))
      case (head, next) if index%2 == 1 => cons(nth(agen2, x), interleaveHelper(x+1, index+1, agen1, agen2))
    }
  interleaveHelper(0, 0, agen1, agen2)
  }

  def sift(n: Int, agen: Generator[Int]): Generator[Int] = {
    filter(_ % n != 0, agen)
  }

  val prime: Generator[Int] = {
    def primeHelp(agen1: Generator[Int]): Generator[Int] = {
      cons(agen1.next()._1, primeHelp(sift(agen1.next()._1, agen1)))
    }
    primeHelp(from(2))
  }

  def total(agen: Generator[Double]): Generator[Double] = {
    def totalHelper(a: Int): Generator[Double] = {
      cons(totalCount(a), totalHelper(a+1))
    }
    def totalCount(index: Int): Double = {
      def sum(idx: Int, totalsum: Double): Double = {
        if(idx == 0) totalsum
        else sum(idx-1, totalsum + nth(agen, idx))
      }
      sum(index, agen.next()._1)
    }
  totalHelper(0)
  }

}