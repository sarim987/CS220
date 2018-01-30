// Sarim Ahmed 30678858
// CS220
// Project 1 List Processing
// 01-24-18
object Lists {

  val oddNumbers = 1 :: 3 :: 5 :: Nil

  //  Consumes a List[Int] and produces an Int. 
  //  The produced value is double the sum of the list of integers.
  def sumDouble(alist : List[Int]): Int = alist match {
    case Nil => 0
    case item :: tail => (2 * item) + sumDouble(tail)
  }

  //  Consumes a List[Int] and produces a List[Int]. 
  //  The produced list is the same as the input list, but with all zeroes removed. 
  def removeZeroes(alist : List[Int]): List[Int] = alist match {
    case Nil => alist
    case item :: tail if item == 0 => removeZeroes(tail)
    case item :: tail if item != 0 => item :: removeZeroes(tail)

  }
  //  Consumes a List[Int] and produces an Int that represents 
  //  the number of even numbers in the input list.
  def countEvens(alist : List[Int]): Int = alist match {
    case Nil => 0
    case item :: tail if item % 2  == 0 => 1 + countEvens(tail)
    case item :: tail if item % 2  != 0 => countEvens(tail)
  }

  //  Consumes a List[String] and produces a List[String] 
  //  that has every other element in the input list.
  def removeAlternating(alist : List[String]): List[String] = alist match {
    case Nil => Nil
    case item::Nil => alist
    case item1 :: item2 :: tail => item1 :: removeAlternating(tail)
  }

  //  Consumes a List[Int] and produces a Boolean that is 
  //  true if the numbers in the input list are in ascending order. 
  //  Note that the input may have repeated numbers.
  def isAscending(alist : List[Int]): Boolean = alist match {
    case Nil => true
    case item :: Nil => true
    case item :: tail => isAscendingHelper(item, tail)
    case _ => false    
  }
  def isAscendingHelper(prev : Int, alist : List[Int]): Boolean = alist match{
    case item :: Nil if prev <= item => true
    case item :: tail if prev <= item => true && isAscendingHelper(item, tail)
    case _ => false
  }
  //  Consumes a List[Int] and produces an Int. The function adds all
  //  the elements in even position and subtract all the elements in odd position.
  //  Note, first element of a list is considered “zeroth” element, thus it is in even position.
  def addSub(alist : List[Int]): Int ={   
    
    def addSubEven(alist : List[Int]): Int = alist match {
      case Nil => 0
      case item::Nil => item
      case item1 :: item2 :: tail => item1 + addSubEven(tail)
    }
    def addSubOdd(alist : List[Int]): Int = alist match {
      case Nil => 0
      case item::Nil => 0
      case item1 :: item2 :: tail => item2 + addSubOdd(tail)
    }
    addSubEven(alist) - addSubOdd(alist)
  }

  //  Consumes two List[Int] arguments and produces a List[Int].  
  //  The elements of the resulting list alternate between the elements of the arguments. 
  //  We assume that the two arguments have the same length.
  def alternate (alist : List[Int], blist : List[Int]): List[Int] = (alist, blist) match {
    case (Nil, Nil) => Nil
    case (item1 :: Nil, Nil) => alist
    case (Nil, item2 :: Nil) => blist
    case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: alternate(tail1, tail2)
    case _ => throw new Exception("BAD")
  }

  //  Takes two Ints as arguments and produces a List[Int]. The value of fromTo (x, y) 
  //  is the list of consecutive integers that start from and include x, going up to and excluding y. 
  //  We assume that x < y.
  def fromTo(low : Int, high: Int): List[Int] = {
    def fromToHelper(num: Int): List[Int] = num match {
      case num if num < high => num :: fromToHelper(num + 1) 
      case _ => Nil
    }
    fromToHelper(low)
  }

  //  We assume that lst is in ascending order. 
  //  insertOrdered produces a list that is the same as the input, but with n inserted
  //  such that the order is preserved. We assume that lst is in ascending order.
  def insertOrdered(n: Int, lst: List[Int]): List[Int] = lst match{
    case Nil => n :: Nil
    case item :: tail if n <= item => n :: lst
    case item :: tail => item :: insertOrdered(n, tail)
  }
  
  //  The result is a sorted input list.
  def sort(lst: List[Int]): List[Int] = {
    def sortHelper(unsorted: List[Int], sorted: List[Int]): List[Int] = unsorted match{
      case Nil => sorted
      case item :: tail => sortHelper(tail, insertOrdered(item, sorted))
    }
    sortHelper(lst, List())
  }

}