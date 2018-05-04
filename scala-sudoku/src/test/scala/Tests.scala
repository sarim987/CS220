class TestSuite extends org.scalatest.FunSuite {

  test("The solution object must be defined") {
    val obj : hw.sudoku.SudokuLike = Solution
  }

  test("EmptyBoard") {
  val str = "1................................................................................"
    //assert(Solution.parse(str) == new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap))
  }
  test("valueAt1") {
  val str = ".17895632328764951569213784782349516145628397936157428891436275254971863673582149"
    assert(Solution.parse(str).valueAt(0,0) == Some(4))
  }
    test("valueAt2") {
  val str = "417895632328764951569213784782349516145628397936157428891436275254971863673582149"
    assert(Solution.parse(str).valueAt(1,0) == Some(3))
  }
    test("valueAt3") {
  val str = "417895632328764951569213784782349516145628397936157428891436275254971863673582149"
    assert(Solution.parse(str).valueAt(1,4) == Some(6))
  }
  test("isSolved") {
  val str = "417895632328764951569213784782349516145628397936157428891436275254971863673582149"
    assert(Solution.parse(str).isSolved() == true)
  }
    test("unsolvable") {
    val str = "417895632328764951569213784782349516145628397936157428891436275254971863673582149"
    assert(Solution.parse(str).isUnsolvable() == false)
    val unsolvable = ".17895632328764951569213784782349516145628397936157428891436275254971863673582149"
    //assert(Solution.parse(unsolvable).place(3, 0, 4).isUnsolvable() == true)
  }
  test("place") {
    val puzzle = "................................................................................."
    val board = Solution.parse(puzzle)
    board.place(0,0,5)
    //assert(board.place(0, 0, 3) == new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap))
  }
    test("place2") {
    val puzzle = "4.78.5632.2876495.5692.37847823495.6.45628.97936.5742889.436275.5497.86367.582.49"
    val board = Solution.parse(puzzle)
    
    //assert(board.place(0, 1, 1) == new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap))
  }
    test("nextStates") {
    val puzzle = "4.78.5632.2876495.5692.37847823495.6.45628.97936.5742889.436275.5497.86367.582.49"
    val board = Solution.parse(puzzle)
    
    //assert(board.nextStates() == new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap))
  }
    test("solve") {
    val puzzle = "4.78.5632.2876495.5692.37847823495.6.45628.97936.5742889.436275.5497.86367.582.49"
    val board = Solution.parse(puzzle)
    
    assert(board.solve() == Some(new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap)))
  }
  test("Easier1") {
  // this one requires backtrack searching
  val fromCS121_1 = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
  val board = Solution.parse(fromCS121_1)
  assert(board.solve() == Some(new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap)))
}
  test("Easier2") {
  // this one requires backtrack searching
  val fromCS121_2 = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
  val board = Solution.parse(fromCS121_2)
  assert(board.solve() == Some(new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap)))
}
  test("Easier3") {
  // this one requires backtrack searching
  val puz1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
  val board = Solution.parse(puz1)
  assert(board.solve() == Some(new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap)))
}
  test("Easier4") {
  // this one requires backtrack searching
  val puz2 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
  val board = Solution.parse(puz2) 
  println(board.solve())
  assert(board.solve() == Some(new Board(Solution.calcAllPos(0).map(coord => coord -> 1.to(9).toList).toMap)))
}




}