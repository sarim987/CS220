import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

 def calcAllPos(ix: Int): List[(Int, Int)] = {
    if (ix == 81) Nil else (ix / 9, ix % 9) :: calcAllPos(ix + 1)
  }

  val allPos = calcAllPos(0)
  val oneTo9 = 1.to(9).toList
  val emptyBoard = new Board(allPos.map(coord => coord -> oneTo9).toMap)

  def parseHelper(alist: List[(Char, (Int, Int))]): Board = alist match {
    case Nil => emptyBoard
    case ('.', _) :: rest => parseHelper(rest)
    case (digit, (row, col)) :: rest => {
      val n = digit.toString.toInt
      parseHelper(rest).place(row, col, n)
    }
  }

  def parse(str: String): Board = parseHelper(str.toList.zip(allPos))

  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map(r => (r,col))
    val colPeers = 0.to(8).map(c => (row, c))
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap(r =>
      boxCol.to(boxCol + 2).map(c => (r, c)))
    // Remove duplicates and (row, col)
    (rowPeers ++ colPeers ++ boxPeers).toSet.diff(Set((row, col))).toList
  }

  val peersTbl = allPos.map(pos => {
    val (row, col) = pos
    pos -> calcPeers(row, col)
  }).toMap

  def peers(row: Int, col: Int): Seq[(Int, Int)] = peersTbl((row, col))
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    available.get((row, col)) match {
      case None => None
      case Some(v) => if(v.length == 1) Some(v.head) else None

    }
  }

  def isSolved(): Boolean = {
    def solRow(row: Int, col: Int): Boolean = {
      def solCol(col: Int): Boolean = {
        if(row == 8 && col == 8){if(valueAt(row, col) != None) true else false}
        else if(col < 9){ if(valueAt(row, col) != None) solCol(col + 1) else false} 
        else solRow(row + 1, 0)
      }
      solCol(0)
    }
    solRow(0, 0)
  }

  def isUnsolvable(): Boolean = {
    def solRow(row: Int, col: Int): Boolean = {
      def solCol(col: Int): Boolean = {
        if(row == 8 && col == 8){if(available(row, col).length == 0) true else false}
        else if(col < 9){ if(available(row, col).length == 0) solCol(col + 1) else false} 
        else solRow(row + 1, 0)
      }
      solCol(0)
    }
    solRow(0, 0)
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val newBoard = new Board(available + ((row, col) -> (value :: Nil)))
    val lst = Solution.peers(row, col)
    def removevals(lst: Seq[(Int, Int)], valmap: Map[(Int, Int), List[Int]], value: Int): Board = lst match {
      //case tup :: tail
      case tup :: tail => removevals(tail, valmap + (tup -> valmap((tup._1,tup._2)).filter(_ != value)), value)
      case Nil => new Board(valmap)
    }
    removevals(lst, available + ((row, col) -> (value :: Nil)), value)
  }



  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    def nextBoards(lst: List[(Int, Int)]): List[Board] = /**lst match**/ {
      //case tup :: tail => available.map(tup._1, tup._2) -> place(tup._1, tup._2, _)).toList ++ nextBoards(tail)
      //case Nil => Nil
      ???
    }
    if (isUnsolvable()) {
      List()
    }
    else {
      //nextBoards(0.to(8).flatMap(row => 0.to(8).map(col => (row, col))).toList)
      ???
    }
  }

  def solve(): Option[Board] = ???
}