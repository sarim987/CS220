import hw.tictactoe._



class Game(val player : Player, dim : Int, board: Map[(Int, Int), Player] ) extends GameLike[Game] {
      
   

  // should produce true if X's and O's are in a row or the game is in a draw
  def isFinished(): Boolean = {

     if (board.size == dim*dim) true 
     else if (getWinner() != None) true 
     else false
    
  }

  

  //GET Keys returns (0,1) 
  //get values returns O or X 

  def verticalCases(trackX: Int, trackY: Int, person: Player, siz : Int): Boolean  = {  
     if (trackX > siz || trackY > siz) false 
     if (board.contains(trackX, trackY))  {

     if (board.get(trackX, trackY) == Some(person)  && trackX == siz && trackY == siz) true                    //reaches the end of the last column and person matches
     else if (trackX == siz && trackY == siz) false                                                           //reaches the end of the of the last colummn and person doesnt match 
     else if (board.get(trackX, trackY) == Some(person) && trackY == siz) true                              //reaches the end of a column and person matches
     else if (board.get(trackX, trackY) == Some(person))  verticalCases(trackX, trackY + 1, person, siz)     //matches goes down the column
     else  verticalCases(trackX +1, 0, person, siz)                                                          //doesn't match person, moves to right top
          
     }
    // else if (trackX == siz) false
    // else  verticalCases(trackX +1, 0, person, siz)                                                            
    else false 
  }






 def horizontalCases(trackX: Int, trackY: Int, person: Player, siz: Int) : Boolean =  {

   if (trackX > siz || trackY > siz) false 

   if (board.contains((trackX, trackY)))  {

     if (board.get(trackX, trackY) == Some(person)  && trackX == siz && trackY == siz) true  //reaches the end of the last row  and person matches
     else if (trackX == siz && trackY == siz) false //reaches the end of the of the last row  and person doesnt match 
     else if (board.get(trackX, trackY) == Some(person) && trackX == siz) true   //reaches the end of a row and person matches
     else if (board.get(trackX, trackY) == Some(person))  horizontalCases(trackX +1, trackY , person, siz) //matches goes down the row
     else  horizontalCases(0, trackY + 1, person, siz) //doesn't match person, moves to left bottom 
   
   }
  
  // else if (trackY == siz) false 
   else  false

}
  
//FOR HORIZONTAL AND VERTICAL CASES IF IT EXCEEDS EITHER DIMENSION IN THE BEGINNING JUST RETURN FALSE 

     def diagonalCasesRight(trackX : Int, trackY: Int, person: Player, siz: Int) : Boolean = {

        if (board.contains((trackX, trackY))) {
           if (trackX == siz  && trackY == siz && board.get(trackX, trackY) == Some(person)) true 
           else if (board.get(trackX, trackY) == Some(person))  diagonalCasesRight(trackX+1, trackY+1, person, siz)
           else false 
        }
        else false 
    }

    def diagonalCasesLeft(trackX : Int, trackY: Int, person: Player, siz: Int) : Boolean = {
        if (board.contains((trackX, trackY))) {
             if (trackX == 0  && trackY == siz && board.get(trackX, trackY) == Some(person)) true 
             else if (board.get(trackX, trackY) == Some(person))  diagonalCasesLeft(trackX-1, trackY+1, person, siz)
             else false 
        }
        else false 
      
    }



    
   

     
  

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] =  {

    if (diagonalCasesLeft(2,0,X,dim-1)  || diagonalCasesRight(0,0,X,dim-1) ||  horizontalCases(0,0,X,dim-1)    || verticalCases(0,0,X,dim-1))  Some(X) 
    else if (diagonalCasesLeft(2,0,O,dim-1) || diagonalCasesRight(0,0,O,dim-1) ||  horizontalCases(0,0,O,dim-1)  || verticalCases(0,0,O,dim-1))  Some(O) 
    else None 


  }



  override def toString(): String = {
    "Player: " + player + " dim " + dim + " board " + board
  }
  



  //return the rest of the possible moves for player 1 
  def nextBoards(): List[Game] =  {
    
    traversal(0,0, Nil)
    

     
  }

   def traversal(trackX: Int , trackY: Int , list : List[Game]): List[Game] = { 

       if (trackX < dim && trackY < dim){
        if (!board.contains(trackX, trackY)) {
          if (player == O){
             val newMapO = board + ((trackX, trackY) -> O)
             traversal(trackX + 1, trackY, new Game(X, dim, newMapO) :: list)
          }
          else{
             val newMapX = board + ((trackX, trackY) -> X)
             traversal(trackX + 1, trackY, new Game(O, dim, newMapX) :: list)
          } 
        }else traversal(trackX +1, trackY, list) 
      
      }
      else  { if (trackY < dim) traversal(0,trackY+1, list) else list }
   
  }

  

}

  

object Solution extends MinimaxLike {
  type T = Game // T is an "abstract type member" of MinimaxLike
 
  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game =  new Game(turn, dim, board)

  
 
  //tweak this detailed psuedo code 
  def minimax(board: Game): Option[Player] =  
  {
     if (board.isFinished()) board.getWinner()
     else {
        val next = board.nextBoards.toStream.map(b => minimax(b))
           if (next.contains(Some(board.player))) Some(board.player) 
           else if (next.contains(None)) None 
           else {
            if (board.player == X) Some(O)
            else Some(X)
          }
      }
   }


}
     





//       }
//     }
//   }
//  //start with 2,0
//   def diagonalCasesLeft(trackX : Int, trackY: Int, person: Player, siz: Int) : Boolean = {
//        if (board.contains((trackX, trackY))) false
//      board match {
//       case Map => {
//         case((x,y),z) => if (x == 0 && y == siz && z == person) true 
//         case ((x,y),z) => if (x == trackX && y == trackY && z == person)  diagonalCasesRight(trackX-1, trackY+1, person, siz)
//         case _ => false 


//       }
//     }
//   }


//     def horizontalCases(trackX: Int, trackY: Int, person: Player, siz: Int) : Boolean =  {
//        if (board.contains((trackX, trackY))) horizontalCases(0, trackY +1, person, siz) 
//  board match {
//        case Map => {
//           case ((x,y),z) => if (x == siz && y == siz && z == person) true
//           case ((x,y), z) => if (x == siz &&  y == siz) false 
//           case ((x, y),z) => if (x == siz && y == trackY && z == person) true
//           case ((x, y),z) => if (x == trackX && y == trackY && z == person) horizontalCases(trackX+1, trackY, person,siz) 
//           case((x, y), z) => if (x == trackX && y == trackY)  horizontalCases(0, trackY +1, person, siz)         

//       }
//     }

//     def diagonalCasesRight(trackX : Int, trackY: Int, person: Player, siz: Int) : Boolean = {
//        if (board.contains((trackX, trackY))) false
//      board match {
//       case Map => {
//         case((x,y),z) => if (x == siz && y == siz && z == person) true 
//         case ((x,y),z) => if (x == trackX && y == trackY && z == person)  diagonalCasesRight(trackX+1, trackY+1, person, siz)
//         case _ => false 
    
//  def verticalCases(trackX: Int, trackY: Int, person: Player, siz : Int): Boolean  = {  
//      if (board.contains((trackX, trackY))) verticalCases(trackX +1, 0, person, siz) 
//   board match{
//     Map match  => {
//       case ((x,y),z) => if (x == siz && y == siz && z == person) true
//       case ((x,y), z) => if (x == siz &&  y == siz) false 
//       case ((x, y),z) => if (x == trackX && y == siz && z == person) true
//       case ((x, y),z) => if (x == trackX && y == trackY && z == person) verticalCases(trackX, trackY+1, person,siz) 
//       case((x, y), z) => if (x == trackX && y == trackY)  verticalCases(trackX+1, 0, person, siz)
//     }

//    }
//   }