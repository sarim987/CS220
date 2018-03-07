class Tests extends org.scalatest.FunSuite {

  import Solution._
  import hw.tictactoe._

  test("CreateBoard") {
    val gameBoard = Solution.createGame(O, 3, Map((0, 0) -> X, (0, 1) -> X, (0, 2) -> X, (2, 2) -> O, (1, 1) -> O))
    //assert(minimax(new Game(X, 3, Map())) == None)
    //assert(gameBoard.winning(List( ((0, 0), X), ((0, 1), X), ((0, 2), X) ), X) == true)
    //assert(gameBoard.winning(List( ((0, 0), O), ((0, 1), O), ((0, 2), O) ), O) == true)
    //assert(gameBoard.winning(List( ((0, 0), X), ((0, 1), X), ((0, 2), O) ), X) == false)

  }
  test("CreateBoard2") {
     val gameBoard = Solution.createGame(O, 3, Map((0, 0) -> X, (1, 0) -> X, (2, 0) -> X, (1, 1) -> X, (2, 2) -> X, (2, 1) -> O, (0, 1) -> O, (0, 2) -> O, (1, 2) -> O))


    val gameBoard2 = Solution.createGame(O, 3, Map((0, 0) -> O, (1, 0) -> X, (2, 0) -> O,
                                                   (0, 1) -> O, (1, 1) -> X, (2, 1) -> X,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> X))

    val gameBoard3 = Solution.createGame(O, 3, Map((0, 0) -> O, (1, 0) -> X, (2, 0) -> X,
                                                   (0, 1) -> O, (1, 1) -> O, (2, 1) -> X,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> X))  

    val gameBoard4 = Solution.createGame(O, 3, Map((0, 0) -> O, (1, 0) -> O, (2, 0) -> O,
                                                   (0, 1) -> X, (1, 1) -> X, (2, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> X)    )                                           
    //assert(minimax(new Game(X, 3, Map())) == None)
    assert(gameBoard4.isFinished() == true)


  }
  test("nextBoards") {
    val gameBoard = new Game(O, 3,             Map((0, 0) -> X, (1, 0) -> X, 
                                                                (1, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> O))

    val sol1 = new Game(X, 3,                  Map((0, 0) -> X, (1, 0) -> X, 
                                                   (0, 1) -> O, (1, 1) -> O, 
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> O)) 

    val sol2 = new Game(X, 3,                  Map((0, 0) -> X, (1, 0) -> X, 
                                                                (1, 1) -> O, (2, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> O)) 

    val sol3 = new Game(X, 3,                  Map((0, 0) -> X, (1, 0) -> X, (2, 0) -> O,
                                                                (1, 1) -> O, 
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> O))                             
    //assert(minimax(new Game(X, 3, Map())) == None)
    //gameBoard.drawBoard()
    //assert(List(new Game(X, 3, Map())).toString() == List(new Game(X, 3, Map())).toString())
    
    assert(gameBoard.checkRow(0) == List[Game](sol3))


  }

}
