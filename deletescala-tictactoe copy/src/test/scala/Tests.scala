
class Tests extends org.scalatest.FunSuite {

  import Solution._
  import hw.tictactoe._
     val gameBoard = Solution.createGame(O, 3, Map((0, 0) -> X, (1, 0) -> X, (2, 0) -> X, (1, 1) -> X, (2, 2) -> X, (2, 1) -> O, (0, 1) -> O, (0, 2) -> O, (1, 2) -> O))


    val gameBoard2 = Solution.createGame(X, 3, Map(             (1, 0) -> O,
                                                   (0, 1) -> O, (1, 1) -> X, (2, 1) -> X,
                                                   (0, 2) -> X, (1, 2) -> X, (2, 2) -> X))

  test("isFinished1") {
    //assert(minimax(new Game(X, 3, Map())) == None)
    assert(gameBoard2.getWinner() == Some(X))
    

  }
  // test("isFinished2") {
  //   //assert(minimax(new Game(X, 3, Map())) == None)
  //   assert(gameBoard3.isFinished() == false)
  // }
      val win1 = new Game(X, 3,                Map((0, 0) -> X, (1, 0) -> O, (2, 0) -> X,
                                                   (0, 1) -> O, (1, 1) -> O, (2, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> X, (2, 2) -> O))

      val win2 = new Game(X, 3,                Map((0, 0) -> X, (1, 0) -> O, (2, 0) -> X,
                                                   (0, 1) -> X, (1, 1) -> O, (2, 1) -> O,
                                                   (0, 2) -> O, (1, 2) -> O, (2, 2) -> O))

      val win3 = new Game(X, 3,                Map((0, 0) -> X, (1, 0) -> O, (2, 0) -> O,
                                                   (0, 1) -> O, (1, 1) -> X, (2, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> X))

      val win4 = new Game(X, 3,                Map((0, 0) -> O, (1, 0) -> O, (2, 0) -> X,
                                                   (0, 1) -> O, (1, 1) -> X, (2, 1) -> O,
                                                   (0, 2) -> X, (1, 2) -> O, (2, 2) -> X)) 

      val draw = new Game(X, 3,      Map((0, 0) -> X, (1, 0) -> X, (2, 0) -> O,
                                     (0, 1) -> O, (1, 1) -> X, (2, 1) -> X,
                                     (0, 2) -> X, (1, 2) -> O, (2, 2) -> O))     

                                     
  test("winner1") {
    //assert(minimax(new Game(X, 3, Map())) == None)
   //assert(win1.getWinner() == Some(O))
  }
  //   test("winner2") {
  //   //assert(minimax(new Game(X, 3, Map())) == None)
  //   assert(win2.getWinner() == Some(O))
  // }
  //   test("winner3") {
  //   //assert(minimax(new Game(X, 3, Map())) == None)
  //   assert(win3.getWinner() == Some(X))
  // }
  // test("winner4") {
  //   //assert(minimax(new Game(X, 3, Map())) == None)
  //   assert(win4.getWinner() == Some(X))
  // }
  // test("winner5") {
  //   //assert(minimax(new Game(X, 3, Map())) == None)
  //   assert(draw.getWinner() == None)
  // }
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
    
   assert(gameBoard.nextBoards() == List(sol1, sol2, sol3))



  }
  // test("minimax") {
                           
  //   assert(minimax(new Game(X, 3, Map())) == None)
  //   //assert(List(new Game(X, 3, Map())).toString() == List(new Game(X, 3, Map())).toString())
  //   val sol1 = new Game(X, 3,      Map((0, 0) -> O, (1, 0) -> X, (2, 0) -> O,
  //                                                   (1, 1) -> X, (2, 1) -> X,
  //                                      (0, 2) -> O, (1, 2) -> O, (2, 2) -> X)) 
  // val sol2 = new Game(X, 3,      Map((0, 0) -> X, (1, 0) -> X, (2, 0) -> O,
  //                                    (2, 0) -> O, (1, 1) -> X, (2, 1) -> X,
  //                                    (0, 2) -> X, (1, 2) -> O  )      ) 

  // val draw = new Game(O, 3,      Map((0, 0) -> X, (1, 0) -> X, 
  //                                    (0, 1) -> O, (1, 1) -> X, (2, 1) -> X,
  //                                    (0, 2) -> X, (1, 2) -> O, (2, 2) -> O))                                                                 
    
  //   assert(Solution.minimax(sol1) == Some(X))
  //   assert(Solution.minimax(sol2) == Some(X))
  //   assert(Solution.minimax(draw) == None)


  // }

}