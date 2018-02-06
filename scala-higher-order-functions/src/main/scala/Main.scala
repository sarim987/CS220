object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match{
    case (Nil, Nil) => Nil
    case (_, Nil) | (Nil, _) => throw new IllegalArgumentException("lists should be same size")
    case (h1 :: tail1, h2 :: tail2) => f(h1, h2) :: map2[A, B, C](f, tail1, tail2)

  }

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match{
    case (Nil, Nil) => Nil
    case (_, Nil) | (Nil, _) => throw new IllegalArgumentException("lists should be same size")
    case (h1 :: tail1, h2 :: tail2) => (h1, h2) :: zip(tail1, tail2)

  }

  def flatten[A](lst: List[List[A]]): List[A] = lst match{
    case Nil => Nil
    case Nil :: tail => flatten(tail)
    case (item :: tail2) :: tail => item :: flatten(tail2 :: tail)
  }

  def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match {
    case Nil => Nil
    case Nil :: tail => Nil
    case (Nil :: tail) :: tail2 => Nil
    case item :: tail => flatten(flatten(lst))
  }
  

  def buildList[A](length: Int, f: Int => A): List[A] ={
    def buildListHelper[A](curr: Int, end: Int, f: Int => A): List[A] = curr match{
      case _ if curr != end => f(curr) :: buildListHelper(curr + 1, end, f)
      case end => Nil
    }
    buildListHelper(0, length, f)
  }
  
  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] ={
      def mapListHelper[A, B](lst: List[A], f: A => List[B]): List[List[B]] = lst match {
        case Nil => Nil
        case head :: tail => f(head) :: mapListHelper(tail, f)
      } 
      flatten(mapListHelper(lst, f))
  }



  def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) ={

    def partitionMatch[A](f: A => Boolean, lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case head :: tail =>
      f(head) match {
        case true => head :: partitionMatch(f, tail)
        case false => partitionMatch(f, tail)
      }
    }
    def partitionNOMatch[A](f: A => Boolean, lst: List[A]): List[A] = lst match {
      case Nil => Nil
      case head :: tail =>
      f(head) match {
        case true => partitionNOMatch(f, tail)
        case false => head :: partitionNOMatch(f, tail)
      }
    }
    (partitionMatch(f, lst), partitionNOMatch(f, lst))
  
  }

  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = (alist1, alist2) match {
    case (Nil, Nil) => Nil
    case (alist1, Nil) => alist1
    case (Nil, alist2) => alist2
    case (item1 :: tail1, item2 :: tail2) if lessThan(item1, item2) => item1 :: merge(lessThan, tail1, alist2)
    case (item1 :: tail1, item2 :: tail2) if !lessThan(item1, item2) => item2 :: merge(lessThan, alist1, tail2)
  }

  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match{
    case Nil => Nil
    case item :: tail => sortHelper(lessThan, item, sort(lessThan, tail))

  }

  def sortHelper[A](lessThan: (A, A) => Boolean, element: A, alist: List[A]): List[A] = alist match {
    case Nil => element :: Nil
    case item :: tail if lessThan(element, item) => element :: item :: tail
    case item :: tail if !lessThan(element, item) => item :: sortHelper(lessThan, element, tail)
  }
}
