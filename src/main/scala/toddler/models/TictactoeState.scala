package toddler.models

import toddler.models.GameStatus.Playing
import toddler.models.Player.{O, X}

import scala.util.Random

enum Player(val name: String):
  case X extends Player("X")
  case O extends Player("O")
  case Empty extends Player("")

enum GameStatus:
  case Playing
  case Won(player: Player)
  case Draw
  case Waiting(timer: Double)
  
type Board = Map[Int, Player] 

final case class TictactoeState(
                                 currentPlayer: Player,
                                 board: Board,
                                 gameStatus: GameStatus
                               )

object TictactoeState:
//  val currentPlayer: Player = Random.between(0, 2) match {
//    case 0 => X
//    case 1 => O
//  }

  val initial: TictactoeState =
    TictactoeState(
      currentPlayer = X,
      board = Map(
        (0, Player.Empty),
        (1, Player.Empty),
        (2, Player.Empty),
        (3, Player.Empty),
        (4, Player.Empty),
        (5, Player.Empty),
        (6, Player.Empty),
        (7, Player.Empty),
        (8, Player.Empty)
      ),
      gameStatus = Playing
    )

