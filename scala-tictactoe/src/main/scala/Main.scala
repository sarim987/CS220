import hw.tictactoe._

class Game(val turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike[Game] {
  def isFinished(): Boolean = {
    if(board.size == (dim*dim)) true
    else if(getWinner() != None) true
    else false
  }
  /** Assume that isFinished is true **/
  def getWinner(): Option[Player] =  {
    if(isWinningRow(0, X) || isWinningCol(0, X) || isWinningAnyRDiag(0, X) || isWinningAnyLDiag(0, dim - 1, X)) Some(X)
    else if(isWinningRow(0, O) || isWinningCol(0, O) || isWinningAnyRDiag(0, O) || isWinningAnyLDiag(0, dim - 1, O)) Some(O)
    else None
  }
  def isWinningRow(col: Int, player: Player): Boolean ={
    def isWinningAnyRow(row: Int, col: Int, play: Player): Boolean = {
      if(row < dim && board.contains((row, col)) && board((row, col)) == play) {
        true && isWinningAnyRow(row+1, col, play)
      }else if(row == dim){
        true
      }else{
        false
      }
  }
    if(col < dim){
      if(isWinningAnyRow(0, col, player)) true
      else isWinningRow(col+1, player)
    }
    else false    
  }
  def isWinningCol(row: Int, player: Player): Boolean ={
    def isWinningAnyCol(row: Int, col: Int, play: Player): Boolean = {
      if(col < dim && board.contains((row, col)) && board((row, col)) == play) {
        true && isWinningAnyCol(row, col+1, play)
      }else if(col == dim){
        true
      }else{
        false
      }
    }
    if(row < dim){
      if(isWinningAnyCol(row, 0, player)) true
      else isWinningCol(row+1, player)
    }
    else false    
  }

  def isWinningAnyRDiag(index: Int, play: Player): Boolean = {
      if(index < dim && board.contains((index, index)) && board((index, index)) == play) {
        true && isWinningAnyRDiag(index+1, play)
      }else if(index == dim){
        true
      }else{
        false
      }
    }
  
  def isWinningAnyLDiag(min: Int, max: Int, play: Player): Boolean = {
      if(min < dim && board.contains((min, max)) && board((min, max)) == play) {
        true && isWinningAnyLDiag(min+1, max-1, play)
      }else if(min == dim){
        true
      }else{
        false
      }
  }

  def drawBoard(): Unit = {
    
    board foreach (x => println((x._1 + "-->" + x._2)))
    

  }
  def nextBoards(): List[Game] = {
    def checkRowHelper(row: Int, col: Int, ls: List[Game]): List[Game] = {
      if(row < dim && col < dim && !board.contains((row, col))){
        if(turn == X){
          val newmap = board + ((row, col) -> X)
          val g1 = new Game(O, dim, newmap)
          checkRowHelper(row+1, col, g1 :: ls)
        }else{
          val newmap = board + ((row, col) -> O)
          val g1 = new Game(X, dim, newmap)
          checkRowHelper(row+1, col, g1 :: ls)
        }
      }else if(row < dim && col < dim && board.contains((row, col))){
        checkRowHelper(row+1, col, ls)
      }
      else{
        if(col < dim) checkRowHelper(0, col+1, ls)
        else ls
      }
    }
  checkRowHelper(0,0, List()) 
  }
}
object Solution extends MinimaxLike {
  type T = Game // T is an "abstract type member" of MinimaxLike
  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
    new Game(turn, dim, board)
  }

  def minimax(board: Game): Option[Player] = {
    if(board.isFinished) board.getWinner()
    else{
      val next = board.nextBoards().toStream.map(b => minimax(b))
        if(next.contains(Some(board.turn))) Some(board.turn)
        else if(next.contains(None)) None
        else{
          if(board.turn == O) Some(X)
          else Some(O)
        }
      }
  }
}
