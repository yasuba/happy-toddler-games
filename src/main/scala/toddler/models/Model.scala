package toddler.models

import indigo.Size
import toddler.scenes.{BunnyState, Bubble, Celebration, ChickenState, Egg, HomeScreenState, LettersState, PanelState, PileSweet, Sweet}

// ── Model ────────────────────────────────────────────────────────────────────

final case class Model(
  panel:       PanelState,
  viewport:    Size,
  bubbles:     List[Bubble],
  nextId:      Int,
  counter:     Int,
  celebration: Option[Celebration],
  letters:     LettersState,
  homeScreen:  HomeScreenState,
  chickens:         List[ChickenState],
  chickenEggs:      List[Egg],
  chickenEggsTotal: Int,
  nextChickenId:    Int,
  bunny:       BunnyState,
  sweets:      List[Sweet],
  pileSweets:  List[PileSweet],
  nextSweetId: Int,
  tictactoe:   TictactoeState
)

object Model:
  val initial: Model = Model(
    panel             = PanelState.initial,
    viewport          = Size(800, 600),
    bubbles           = List.empty,
    nextId            = 0,
    counter           = 0,
    celebration       = None,
    letters           = LettersState.initial,
    homeScreen        = HomeScreenState.initial,
    chickens          = List(ChickenState.initial),
    chickenEggs       = List.empty,
    chickenEggsTotal  = 0,
    nextChickenId     = 1,
    bunny             = BunnyState.initial,
    sweets            = List.empty,
    pileSweets        = List.empty,
    nextSweetId       = 0,
    tictactoe         = TictactoeState.initial
  )
