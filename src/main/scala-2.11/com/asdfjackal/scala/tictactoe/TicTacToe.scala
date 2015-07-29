package com.asdfjackal.scala.tictactoe

import scala.util.Random

object TicTacToe extends App{
  val ui = new Gui
  ui.visible = true

  val test = List(-1, -1, -1, 1, 1, 1, -1, -1, 1)
  println(Game.winCheck(test,1))
}

object Game{
  // List of masks to test against the current game state to try and detect a win
  val flags = List(
    List(1,1,1,
      0,0,0,
      0,0,0),
    List(0,0,0,
      1,1,1,
      0,0,0),
    List(0,0,0,
      0,0,0,
      1,1,1),
    List(1,0,0,
      1,0,0,
      1,0,0),
    List(0,1,0,
      0,1,0,
      0,1,0),
    List(0,0,1,
      0,0,1,
      0,0,1),
    List(1,0,0,
      0,1,0,
      0,0,1),
    List(0,0,1,
      0,1,0,
      1,0,0)

  )

  /*
   * Checks a grid against a list of possible win combinations
   * Grid is the grid to be checked and mask is the integer value to maskCheck against each win condition
   */
  def winCheck(grid: List[Int], mask: Int): Boolean ={
    def parseFlags(list: List[List[Int]], index: Int): Boolean ={
      if(index == list.size){
        false
      }else if(maskCheck(grid,list(index),mask)){
        true
      }else{
        parseFlags(list,index+1)
      }
    }
    parseFlags(flags,0)
  }

/*
 * Checks the status of each grid item recursively.
 * On finding zero a false is return and on reaching the list's end a true is returned
 */
  def tieCheck(grid: List[Int]): Boolean ={
    def parseBoard(list:List[Int], index: Int): Boolean ={
      if(index == list.size){
        true
      }else if(list(index) == 0){
        false
      }else{
        parseBoard(list,index+1)
      }
    }
    parseBoard(grid,0)
  }

  /*
   * Checks if for every value of test that isn't 0, the corresponding value in grid is equal to mask
   * Grid is a list of any integers, test is a list of 0s and 1s, and mask is any nonzero integer
   */
  def maskCheck(grid: List[Int], test: List[Int], mask: Int): Boolean ={
    val masked = test zip grid

    def analyze(list: List[(Int,Int)], index: Int = 0): Boolean ={
      if(index == list.size){
        true
      }else if(list(index)._1 != 0){
        if(list(index)._2 == mask){
          analyze(list,index+1)
        }else{
          false
        }
      }else{
        analyze(list,index+1)
      }
    }

    analyze(masked)
  }
}

object AI {
  // Lists representing flags that lead to non-random moves
  val flags = List(
    List(1,1,0,
      0,0,0,
      0,0,0),
    List(1,0,1,
      0,0,0,
      0,0,0),
    List(0,1,1,
      0,0,0,
      0,0,0),
    List(0,0,0,
      1,1,0,
      0,0,0),
    List(0,0,0,
      1,0,1,
      0,0,0),
    List(0,0,0,
      0,1,1,
      0,0,0),
    List(0,0,0,
      0,0,0,
      1,1,0),
    List(0,0,0,
      0,0,0,
      1,0,1),
    List(0,0,0,
      0,0,0,
      0,1,1),
    List(1,0,0,
      1,0,0,
      0,0,0),
    List(1,0,0,
      0,0,0,
      1,0,0),
    List(0,0,0,
      1,0,0,
      1,0,0),
    List(0,1,0,
      0,1,0,
      0,0,0),
    List(0,1,0,
      0,0,0,
      0,1,0),
    List(0,0,0,
      0,1,0,
      0,1,0),
    List(0,0,1,
      0,0,1,
      0,0,0),
    List(0,0,1,
      0,0,0,
      0,0,1),
    List(0,0,0,
      0,0,1,
      0,0,1),
    List(1,0,0,
      0,1,0,
      0,0,0),
    List(1,0,0,
      0,0,0,
      0,0,1),
    List(0,0,0,
      0,1,0,
      0,0,1),
    List(0,0,1,
      0,1,0,
      0,0,0),
    List(0,0,1,
      0,0,0,
      1,0,0),
    List(0,0,0,
      0,1,0,
      1,0,0)
  )

  val moves = List(
    2,1,0,5,4,3,8,7,6,6,3,0,7,4,1,8,5,2,8,4,0,6,4,2
  )

  val r = new Random(System.currentTimeMillis())

  /*
   * Takes a list of Ints where 0 represents an enemy space, 1 represents the AI's space, and -1 represents the opponents space
   * Returns the index of the list that the AI wishes to move to
   */
  def move(grid: List[Int]): Int ={

    def winCheck(grid: List[Int], mask: Int): Int ={
      def parseFlags(list: List[List[Int]], index: Int): Int ={
        if(index == list.size){
          -1
        }else if(Game.maskCheck(grid,list(index),mask)){
          if(grid(moves(index)) == 0){
            moves(index)
          }else{
            parseFlags(list,index+1)
          }
        }else{
          parseFlags(list,index+1)
        }
      }
      parseFlags(flags,0)
    }
    /*
     * Zips all grid cells status to their ids and filters out all tuples with a nonzero status
     * Finally, picks a random tuple in the list and returns its id
     */
    def randomRemainingMove(grid: List[Int]): Int ={
      val available = grid.zipWithIndex.filter(x => x._1 == 0)
      available(r.nextInt(available.length))._2
    }

    winCheck(grid,1) match{
      case -1 =>
        {
          winCheck(grid,-1) match{
            case -1 =>
              randomRemainingMove(grid)
            case i => i
          }
        }
      case i => i
    }
  }

}
