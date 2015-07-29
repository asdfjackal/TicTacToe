package com.asdfjackal.scala.tictactoe

import javax.swing.border.EmptyBorder

import scala.swing._
import scala.swing.event._

class Gui extends MainFrame {

  val prefDef = new Dimension(160, 160)

  def setButtonDimensions(s: Button): Unit = {
    s.maximumSize = prefDef
    s.preferredSize = prefDef
    s.minimumSize = prefDef
  }

  title = "Tic Tac Toe"

  val buttons = List[Button](
    new Button(""),
    new Button(""),
    new Button(""),
    new Button(""),
    new Button(""),
    new Button(""),
    new Button(""),
    new Button(""),
    new Button("")
  )

  val grid = new GridPanel(3, 3) {
    buttons.foreach(contents += _)

    xLayoutAlignment = .5
  }

  grid.preferredSize = prefDef
  grid.maximumSize = prefDef
  grid.minimumSize = prefDef
  grid.hGap = 10
  grid.vGap = 10

  val reset = new Button("Reset")
  reset.xLayoutAlignment = .5
  val status = new Label("Take your turn.")
  status.xLayoutAlignment = .5

  buttons.foreach(setButtonDimensions(_))

  contents = new BoxPanel(Orientation.Vertical) {
    contents += grid
    contents += Swing.VStrut(5)
    contents += status
    contents += Swing.VStrut(5)
    contents += reset
    border = new EmptyBorder(10, 10, 10, 10)
  }
  resizable = false

  listenTo(reset)
  buttons.foreach(listenTo(_))

  reactions += {
    case ButtonClicked(`reset`) => {
      buttons.foreach(
        b => {
          b.text = ""
          b.enabled = true
          status.text = "Take your turn."
        }
      )
    }
    case ButtonClicked(b) => {
      def createStatus(state: String): Int = {
        state match{
          case "X" =>
            -1
          case "O" =>
            1
          case _ =>
            0
        }
      }
      b.enabled = false
      b.text = "X"
      var grid = buttons.map(a => createStatus(a.text)) // I'm sorry
      if(Game.winCheck(grid,-1)) {
        disableAll()
        status.text = "You have won"
      }else if(Game.tieCheck(grid)){
        disableAll()
        status.text = "The game is a tie"
      }else{
        val aiMove = AI.move(grid)
        buttons(aiMove).text = "O"
        buttons(aiMove).enabled = false
        grid = buttons.map(a => createStatus(a.text))
        if(Game.winCheck(grid,1)){
          disableAll()
          status.text = "The AI has won"
        }
      }

    }

  }

  def disableAll()={
    buttons.foreach(_.enabled = false)
  }
}
