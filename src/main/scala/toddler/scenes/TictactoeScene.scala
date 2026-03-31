package toddler.scenes

import indigo.*
import indigo.scenes.{Scene, SceneContext, SceneEvent, SceneName}
import indigo.shared.Outcome
import indigo.shared.events.{EventFilters, GlobalEvent, PointerEvent}
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.subsystems.SubSystem
import indigo.shared.utils.Lens
import toddler.models.Player.{Empty, O, X}
import toddler.models.*
import toddler.models.GameStatus.{Draw, Playing, Waiting, Won}
import toddler.scenes.TictactoeLayout.*

import scala.util.Random
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

// ── Audio ─────────────────────────────────────────────────────────────────────

object TictactoeAudio:
  private def play(file: String): Unit =
    val a = js.Dynamic.newInstance(g.Audio)(s"assets/sounds/$file")
    a.play()
  def win(): Unit = { Music.duck(0.4); play("fanfare.mp3") }

object TictactoeScene extends Scene[Unit, Model, Unit] {

  override type SceneModel = Model
  override type SceneViewModel = Unit

  override def name: SceneName = SceneName("tictactoe")

  override def modelLens: Lens[Model, Model] = Lens.keepLatest[Model]

  override def viewModelLens: Lens[Unit, Unit] = Lens.unit

  override def eventFilters: EventFilters = EventFilters.Permissive

  override def subSystems: Set[SubSystem] = Set.empty

  def updatedStatus(currentPlayer: Player, updatedBoard: Board): GameStatus = {
    if (TicTacToeRules.wins(currentPlayer, updatedBoard)) GameStatus.Won(currentPlayer)
    else if (TicTacToeRules.isDraw(updatedBoard)) GameStatus.Draw
    else GameStatus.Waiting(0.0)
  }

  override def updateModel(context: SceneContext[Unit], model: Model): GlobalEvent => Outcome[Model] = {
    case e: PointerEvent.PointerDown =>
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else
        val vp   = model.viewport
        val tapX = e.position.x
        val tapY = e.position.y
        model.tictactoe.gameStatus match
          case Won(_) | Draw =>
            // "play again?" hit area: full grid width, 20px below grid
            val paY = gridY(vp) + gridSize(vp) + 20
            if tapX >= gridX(vp) && tapX <= gridX(vp) + gridSize(vp) &&
               tapY >= paY && tapY < paY + 40 then
              Outcome(model.copy(tictactoe = TictactoeState.initial))
            else Outcome(model)
          case Waiting(_) => Outcome(model)
          case Playing =>
            val isInGrid = tapX >= gridX(vp) && tapX <= gridX(vp) + gridSize(vp) &&
                           tapY >= gridY(vp) && tapY <= gridY(vp) + gridSize(vp)
            if isInGrid then
              val col   = (tapX - gridX(vp)) / cellSize(vp)
              val row   = (tapY - gridY(vp)) / cellSize(vp)
              val index = row * 3 + col
              if model.tictactoe.board(index) != Empty then Outcome(model)
              else
                def alternatePlayer(player: Player): Player = if player == X then O else X
                val updatedBoard = model.tictactoe.board.map {
                  case (i, _) if i == index => (i, model.tictactoe.currentPlayer)
                  case cell                 => cell
                }
                val newStatus = updatedStatus(model.tictactoe.currentPlayer, updatedBoard)
                newStatus match
                  case Won(_) => TictactoeAudio.win()
                  case _      => ()
                Outcome(model.copy(tictactoe = model.tictactoe.copy(
                  currentPlayer = alternatePlayer(model.tictactoe.currentPlayer),
                  board         = updatedBoard,
                  gameStatus    = newStatus
                )))
            else Outcome(model)

    case FrameTick =>
      model.tictactoe.gameStatus match
        case Waiting(timer) =>
          val newTimer = timer + context.delta.toDouble
          if newTimer >= 0.8 then
            val newBoard = TicTacToeRules.easyModePlayer(model.tictactoe.board)
            val newStatus =
              if TicTacToeRules.wins(O, newBoard) then
                TictactoeAudio.win()
                GameStatus.Won(O)
              else if TicTacToeRules.isDraw(newBoard) then GameStatus.Draw
              else GameStatus.Playing
            Outcome(model.copy(tictactoe = model.tictactoe.copy(
              board = newBoard,
              currentPlayer = X,
              gameStatus = newStatus
            )))
          else
            Outcome(model.copy(tictactoe = model.tictactoe.copy(
              gameStatus = Waiting(newTimer)
            )))
        case _ => Outcome(model)

    case _ => Outcome(model)
  }

  override def updateViewModel(context: SceneContext[Unit], model: Model, viewModel: Unit): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  override def present(context: SceneContext[Unit], model: Model, viewModel: Unit): Outcome[SceneUpdateFragment] = {
    val vp = model.viewport
    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw(vp), bh(vp)), Fill.Color(RGBA(0.851, 0.922, 0.957, 1.0)))
    val gridColor = RGBA(0.545, 0.353, 0.235, 1.0)  // #8B5A3C warm brown
    val xColor    = RGBA(0.624, 0.882, 0.796, 1.0)  // #9FE1CB mint
    val oColor    = RGBA(0.808, 0.796, 0.965, 1.0)  // #CECBF6 lavender

    val gridLinesNode: List[SceneNode] =
      List(
        Shape.Line(Point(gridX(vp)+cellSize(vp),   gridY(vp)),            Point(gridX(vp)+cellSize(vp),   gridY(vp)+gridSize(vp)), Stroke(10, gridColor)),
        Shape.Line(Point(gridX(vp)+cellSize(vp)*2, gridY(vp)),            Point(gridX(vp)+cellSize(vp)*2, gridY(vp)+gridSize(vp)), Stroke(10, gridColor)),
        Shape.Line(Point(gridX(vp),                gridY(vp)+cellSize(vp)), Point(gridX(vp)+gridSize(vp), gridY(vp)+cellSize(vp)), Stroke(10, gridColor)),
        Shape.Line(Point(gridX(vp),              gridY(vp)+cellSize(vp)*2), Point(gridX(vp)+gridSize(vp), gridY(vp)+cellSize(vp)*2), Stroke(10, gridColor))
      )
    val cellNodes: List[SceneNode] = model.tictactoe.board.flatMap { (cell, player) =>
      val col         = cell % 3
      val row         = cell / 3
      val cellCentreX = gridX(vp) + col * cellSize(vp) + cellSize(vp) / 2
      val cellCentreY = gridY(vp) + row * cellSize(vp) + cellSize(vp) / 2
      val margin      = cellSize(vp) / 4
      player match
        case X => List(
          Shape.Line(Point(cellCentreX - margin, cellCentreY - margin), Point(cellCentreX + margin, cellCentreY + margin), Stroke(6, xColor)),
          Shape.Line(Point(cellCentreX + margin, cellCentreY - margin), Point(cellCentreX - margin, cellCentreY + margin), Stroke(6, xColor))
        )
        case O => List(Shape.Circle(Point(cellCentreX, cellCentreY), cellSize(vp) / 4, Fill.None, Stroke(6, oColor)))
        case Empty => List.empty
    }.toList
    val backNode: SceneNode =
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.65))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))
    val messageNode: SceneNode =
      val message = model.tictactoe.gameStatus match
        case Playing    => "Your turn"
        case Waiting(_) => "Computer's turn"
        case Won(p)     => s"Player $p wins!"
        case Draw       => "It's a draw"
      TextBox(message)
        .withFontSize(Pixels(20))
        .withColor(RGBA(0.35, 0.25, 0.15, 0.80))
        .alignCenter
        .withSize(Size(gridSize(vp), 36))
        .moveTo(Point(gridX(vp), gridY(vp) - 46))
    val playAgainNodes: List[SceneNode] =
      model.tictactoe.gameStatus match
        case Won(_) | Draw =>
          List(
            TextBox("play again?")
              .withFontSize(Pixels(20))
              .withColor(RGBA(0.35, 0.25, 0.15, 0.65))
              .alignCenter
              .withSize(Size(gridSize(vp), 36))
              .moveTo(Point(gridX(vp), gridY(vp) + gridSize(vp) + 20))
          )
        case _ => List.empty
    val nodes = List(bgNode, backNode, messageNode) ::: gridLinesNode ::: cellNodes ::: playAgainNodes
    Outcome(SceneUpdateFragment(Layer(nodes*)))
  }
}

object TicTacToeRules {
  val rows = List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8))
  val columns = List(List(0, 3, 6), List(1, 4, 7), List(2, 5, 8))
  val diagonals = List(List(0, 4, 8), List(2, 4, 6))
  val winningMoves = rows ++ columns ++ diagonals

  def wins(player: Player, board: Board): Boolean = {
    winningMoves.exists(_.forall(c => board(c) == player))
  }

  def isDraw(board: Board): Boolean =
    board.forall((_, fill) => fill != Player.Empty) && !wins(X, board) && !wins(O, board)

  //  val randomCell =
  //    Random().shuffle(List(0,1,2,3,5,6,7,8)).head

  def easyModePlayer(board: Board) =
    val emptyCells = board.filter(_._2 == Empty).keys.toList
    if emptyCells.isEmpty then board
    else
      val index = emptyCells(Random.between(0, emptyCells.length))
      board.updated(index, O)
}

//  def findWinningMove(player: Player, board: Board) =
//    winningMoves.map { _.map { index => (index, board(index)) } }
//      if line has exactly 2 of player's marks
//    and exactly 1 empty cell:
//    return that empty cell
//
//  def autoMove(board: Board) = {
//    val move = findWinningMove(O, board) // can I win?
//      .orElse(findWinningMove(X, board)) // should I block?
//      .orElse(findBuildingMove(board)) // can I make progress?
//      .orElse(findAnyMove(board)) // just go somewhere
//
//    move.map(index => placeMarker(O, index, board))
//      .getOrElse(board)
//  }

object TictactoeLayout {
  def bh(viewport: Size): Int = viewport.height
  def bw(viewport: Size): Int = viewport.width
  private def centreX(viewport: Size): Int = bw(viewport) / 2
  private def centreY(viewport: Size): Int = bh(viewport) / 2
  def cellSize(viewport: Size): Int = bw(viewport) / 5 // just a reasonable fraction of the screen
  def gridSize(viewport: Size): Int = cellSize(viewport) * 3 // total grid is 3 cells wide
  def gridX(viewport: Size): Int = centreX(viewport) - gridSize(viewport) / 2 // top-left corner of grid, centred
  def gridY(viewport: Size): Int = centreY(viewport) - gridSize(viewport) / 2
}
