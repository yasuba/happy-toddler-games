package toddler.scenes

import indigo.*
import indigo.scenes.*
import toddler.models.Model

// ── Letter data ───────────────────────────────────────────────────────────────

final case class CardInfo(word: String, emoji: String)
final case class LetterEntry(letter: Char, correct: CardInfo, d1: CardInfo, d2: CardInfo)

val LetterEntries: List[LetterEntry] = List(
  LetterEntry('A', CardInfo("Apple",     "🍎"), CardInfo("Banana",    "🍌"), CardInfo("Duck",      "🦆")),
  LetterEntry('B', CardInfo("Ball",      "🏀"), CardInfo("Cat",       "🐱"), CardInfo("Apple",     "🍎")),
  LetterEntry('C', CardInfo("Cat",       "🐱"), CardInfo("Ball",      "🏀"), CardInfo("Sun",       "☀️")),
  LetterEntry('D', CardInfo("Dog",       "🐶"), CardInfo("Fish",      "🐟"), CardInfo("Hat",       "🎩")),
  LetterEntry('E', CardInfo("Egg",       "🥚"), CardInfo("Dog",       "🐶"), CardInfo("Flower",    "🌸")),
  LetterEntry('F', CardInfo("Fish",      "🐟"), CardInfo("Egg",       "🥚"), CardInfo("Train",     "🚂")),
  LetterEntry('G', CardInfo("Grapes",    "🍇"), CardInfo("Fish",      "🐟"), CardInfo("Bear",      "🐻")),
  LetterEntry('H', CardInfo("Hat",       "🎩"), CardInfo("Grapes",    "🍇"), CardInfo("Frog",      "🐸")),
  LetterEntry('I', CardInfo("Ice cream", "🍦"), CardInfo("Hat",       "🎩"), CardInfo("Rabbit",    "🐰")),
  LetterEntry('J', CardInfo("Jellyfish", "🪼"), CardInfo("Ice cream", "🍦"), CardInfo("Car",       "🚗")),
  LetterEntry('K', CardInfo("Kite",      "🪁"), CardInfo("Jellyfish", "🪼"), CardInfo("Apple",     "🍎")),
  LetterEntry('L', CardInfo("Lion",      "🦁"), CardInfo("Kite",      "🪁"), CardInfo("Egg",       "🥚")),
  LetterEntry('M', CardInfo("Moon",      "🌙"), CardInfo("Lion",      "🦁"), CardInfo("Fish",      "🐟")),
  LetterEntry('N', CardInfo("Nest",      "🪹"), CardInfo("Moon",      "🌙"), CardInfo("Ball",      "🏀")),
  LetterEntry('O', CardInfo("Orange",    "🍊"), CardInfo("Nest",      "🪹"), CardInfo("Dog",       "🐶")),
  LetterEntry('P', CardInfo("Pig",       "🐷"), CardInfo("Orange",    "🍊"), CardInfo("Moon",      "🌙")),
  LetterEntry('Q', CardInfo("Queen",     "👸"), CardInfo("Pig",       "🐷"), CardInfo("Kite",      "🪁")),
  LetterEntry('R', CardInfo("Rabbit",    "🐰"), CardInfo("Queen",     "👸"), CardInfo("Sun",       "☀️")),
  LetterEntry('S', CardInfo("Sun",       "☀️"), CardInfo("Rabbit",    "🐰"), CardInfo("Hat",       "🎩")),
  LetterEntry('T', CardInfo("Train",     "🚂"), CardInfo("Sun",       "☀️"), CardInfo("Grapes",    "🍇")),
  LetterEntry('U', CardInfo("Umbrella",  "☂️"), CardInfo("Train",     "🚂"), CardInfo("Cat",       "🐱")),
  LetterEntry('V', CardInfo("Van",       "🚐"), CardInfo("Umbrella",  "☂️"), CardInfo("Lion",      "🦁")),
  LetterEntry('W', CardInfo("Whale",     "🐳"), CardInfo("Van",       "🚐"), CardInfo("Orange",    "🍊")),
  LetterEntry('X', CardInfo("Xylophone", "🎵"), CardInfo("Whale",     "🐳"), CardInfo("Pig",       "🐷")),
  LetterEntry('Y', CardInfo("Yo-yo",     "🪀"), CardInfo("Xylophone", "🎵"), CardInfo("Egg",       "🥚")),
  LetterEntry('Z', CardInfo("Zebra",     "🦓"), CardInfo("Yo-yo",     "🪀"), CardInfo("Moon",      "🌙"))
)

// ── Colours ───────────────────────────────────────────────────────────────────

// Letter card — warm amber/peach
val LcBg     = RGBA(0.980, 0.933, 0.855, 1.0) // #FAEEDA
val LcBorder = RGBA(0.980, 0.780, 0.459, 1.0) // #FAC775
val LcText   = RGBA(0.388, 0.220, 0.024, 1.0) // #633806

// Letter card — correct state (mint green)
val LcBgOk     = RGBA(0.882, 0.961, 0.933, 1.0) // #E1F5EE
val LcBorderOk = RGBA(0.365, 0.792, 0.647, 1.0) // #5DCAA5
val LcTextOk   = RGBA(0.031, 0.314, 0.255, 1.0) // #085041

// Picture card — neutral
val CardBg     = RGBA(1.0,   1.0,   1.0,   1.0)
val CardBorder = RGBA(0.780, 0.780, 0.780, 1.0)

// Picture card — correct state
val CardBgOk     = RGBA(0.882, 0.961, 0.933, 1.0) // #E1F5EE
val CardBorderOk = RGBA(0.624, 0.882, 0.796, 1.0) // #9FE1CB (mint, matches palette)

// ── Letters state ─────────────────────────────────────────────────────────────

enum LettersPhase:
  case Idle
  case Correct(timer: Double) // counts up to CorrectPauseDuration then advances letter
  case Wrong(cardPos: Int, timer: Double) // briefly greys out the tapped card, then back to Idle

val CorrectPauseDuration = 1.5
val WrongGreyDuration    = 0.6

final case class LettersState(
  letterIdx: Int,
  cardOrder: List[Int],  // permutation of [0=correct, 1=d1, 2=d2]
  phase:     LettersPhase
)

object LettersState:
  val initial: LettersState = LettersState(
    letterIdx = 0,
    cardOrder = List(0, 1, 2), // shuffled on first ViewportResize
    phase     = LettersPhase.Idle
  )

// ── Scene ────────────────────────────────────────────────────────────────────

object LettersScene extends Scene[Unit, Model, Unit]:
  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName               = SceneName("letters")
  val modelLens: Lens[Model, Model] = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters    = EventFilters.Permissive
  val subSystems: Set[SubSystem]    = Set.empty

  // ── Layout helper ─────────────────────────────────────────────────────────
  // Single source of truth for all bounding boxes, used by both updateModel
  // (hit-testing) and present (rendering).

  private final case class Layout(
    lcRect:  Rectangle,       // letter card
    cards:   List[Rectangle]  // picture cards, indices 0–2 (left → right)
  )

  private def layout(vp: Size): Layout =
    val bw    = vp.width
    val bh    = vp.height
    val lcPadX = (bw * 0.10).toInt
    val lcW    = bw - 2 * lcPadX
    val lcH    = (bh * 0.22).toInt
    val lcX    = lcPadX
    val lcY    = (bh * 0.07).toInt
    val cPad   = 12
    val cGap   = 8
    val cardW  = (bw - 2 * cPad - 2 * cGap) / 3
    val cardH  = (bh * 0.40).toInt
    val cardY  = lcY + lcH + (bh * 0.05).toInt
    Layout(
      lcRect = Rectangle(lcX, lcY, lcW, lcH),
      cards  = (0 until 3).toList.map: pos =>
        Rectangle(cPad + pos * (cardW + cGap), cardY, cardW, cardH)
    )

  // ── Helpers ───────────────────────────────────────────────────────────────

  // Returns a random permutation of [0, 1, 2] by picking one of the 6 possibilities.
  private def shuffleCards(dice: Dice): List[Int] =
    List(
      List(0,1,2), List(0,2,1), List(1,0,2),
      List(1,2,0), List(2,0,1), List(2,1,0)
    )(dice.roll(6) - 1)

  // ── Update ────────────────────────────────────────────────────────────────

  def updateViewModel(
      context: SceneContext[Unit],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  def updateModel(
      context: SceneContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] =

    case ViewportResize(vp) =>
      // Also shuffle on first load so the correct card isn't always on the left
      Outcome(model.copy(
        viewport = vp.toSize,
        letters  = model.letters.copy(cardOrder = shuffleCards(context.dice))
      ))

    case FrameTick =>
      val ls = model.letters
      val newLetters = ls.phase match
        case LettersPhase.Correct(t) =>
          val next = t + context.delta.toDouble
          if next >= CorrectPauseDuration then
            // Advance to next letter, wrapping A→Z→A
            ls.copy(
              letterIdx = (ls.letterIdx + 1) % LetterEntries.length,
              cardOrder = shuffleCards(context.dice),
              phase     = LettersPhase.Idle
            )
          else
            ls.copy(phase = LettersPhase.Correct(next))
        case LettersPhase.Wrong(pos, t) =>
          val next = t + context.delta.toDouble
          if next >= WrongGreyDuration then ls.copy(phase = LettersPhase.Idle)
          else ls.copy(phase = LettersPhase.Wrong(pos, next))
        case _ =>
          ls
      Outcome(model.copy(letters = newLetters))

    case e: PointerEvent.PointerDown =>
      // Back button — top-left 52×52 px
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else {
        val ls = model.letters
        ls.phase match
          case LettersPhase.Idle =>
            val pt  = e.position
            val lay = layout(model.viewport)
            val picHit = lay.cards.zipWithIndex.find: (rect, _) =>
              pt.x >= rect.x && pt.x < rect.x + rect.width &&
              pt.y >= rect.y && pt.y < rect.y + rect.height
            picHit match
              case Some((_, pos)) =>
                if ls.cardOrder(pos) == 0 then
                  // Correct card tapped — play the full phrase then start the pause timer
                  val entry = LetterEntries(ls.letterIdx)
                  Audio.playPhonicsAndWord(entry.letter, entry.correct.word)
                  Outcome(model.copy(letters = ls.copy(phase = LettersPhase.Correct(0.0))))
                else
                  // Wrong card — briefly grey it out
                  Outcome(model.copy(letters = ls.copy(phase = LettersPhase.Wrong(pos, 0.0))))
              case None =>
                // No picture card hit — check if the letter card was tapped to replay the letter
                val lc = lay.lcRect
                if pt.x >= lc.x && pt.x < lc.x + lc.width &&
                   pt.y >= lc.y && pt.y < lc.y + lc.height then
                  Audio.playPhonics(LetterEntries(ls.letterIdx).letter)
                Outcome(model)
          case _ =>
            // Ignore taps while showing the correct state
            Outcome(model)
      }

    case _ => Outcome(model)

  // ── Present ───────────────────────────────────────────────────────────────

  def present(
      context: SceneContext[Unit],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val ls    = model.letters
    val entry = LetterEntries(ls.letterIdx)
    val lay   = layout(model.viewport)
    val bw    = model.viewport.width
    val bh    = model.viewport.height
    val isOk   = ls.phase.isInstanceOf[LettersPhase.Correct]
    val wrongPos: Option[Int] = ls.phase match
      case LettersPhase.Wrong(p, _) => Some(p)
      case _                        => None

    // ── Cycling palette ───────────────────────────────────────────────────────
    // Both letter card and picture cards share the same pastel background,
    // cycling through the 4 colours with each new letter.
    val base   = PastelColors(ls.letterIdx % PastelColors.length).rgba
    val border = RGBA(base.r * 0.72, base.g * 0.72, base.b * 0.72, 1.0)

    // ── Letter card ──────────────────────────────────────────────────────────
    val lcR        = lay.lcRect
    val lcFontSize = (lcR.height * 0.72).toInt.min(110)
    val lcTextOffY = ((lcR.height - lcFontSize) / 2).max(0)
    val (lcBgC, lcBorderC, lcTextC) =
      if isOk then (LcBgOk, LcBorderOk, LcTextOk) else (base, border, LcText)

    val letterCardNodes: List[SceneNode] = List(
      Shape.Box(lcR, Fill.Color(lcBgC), Stroke(3, lcBorderC)),
      TextBox(entry.letter.toString)
        .bold
        .withFontSize(Pixels(lcFontSize))
        .withColor(lcTextC)
        .alignCenter
        .withSize(Size(lcR.width, lcR.height))
        .moveTo(Point(lcR.x, lcR.y + lcTextOffY))
    )

    // ── Picture cards ────────────────────────────────────────────────────────
    // Fixed internal layout: emoji anchored near top, word directly below.
    // Using fixed pixel values avoids cut-off from percentage rounding.
    val emojiSize = 48
    val emojiBoxH = 72   // generous height so tall emoji glyphs aren't clipped
    val emojiPadT = 18   // gap from top of card to top of emoji box
    val wordBoxH  = 28
    val wordPadT  = 8    // gap between bottom of emoji box and top of word

    // Base card content (background, emoji, word)
    val cardBases: List[SceneNode] = (0 until 3).toList.flatMap: pos =>
      val info = ls.cardOrder(pos) match
        case 0 => entry.correct
        case 1 => entry.d1
        case _ => entry.d2
      val r             = lay.cards(pos)
      val isCorrectCard = ls.cardOrder(pos) == 0
      val (bgC, bdC)    = if isOk && isCorrectCard then (CardBgOk, CardBorderOk)
                          else (base, border)
      val emojiY = r.y + emojiPadT
      val wordY  = emojiY + emojiBoxH + wordPadT
      List(
        Shape.Box(r, Fill.Color(bgC), Stroke(2, bdC)),
        TextBox(info.emoji)
          .withFontSize(Pixels(emojiSize))
          .alignCenter
          .withSize(Size(r.width, emojiBoxH))
          .moveTo(Point(r.x, emojiY)),
        TextBox(info.word)
          .withFontSize(Pixels(14))
          .withColor(RGBA(0.35, 0.35, 0.35, 1.0))
          .alignCenter
          .withSize(Size(r.width, wordBoxH))
          .moveTo(Point(r.x, wordY))
      )

    // Wrong-tap grey overlay — briefly dims the incorrectly tapped card
    val wrongOverlay: List[SceneNode] = wrongPos match
      case Some(pos) =>
        val r = lay.cards(pos)
        List(Shape.Box(r, Fill.Color(RGBA(0.5, 0.5, 0.5, 0.35))))
      case None => List.empty

    // Overlays drawn on top of card content:
    // — wrong cards get a white wash to push them to ~40% apparent opacity
    // — correct card gets a tick in the top-right corner
    val cardOverlays: List[SceneNode] = if !isOk then List.empty else
      (0 until 3).toList.flatMap: pos =>
        val r             = lay.cards(pos)
        val isCorrectCard = ls.cardOrder(pos) == 0
        if isCorrectCard then
          List(TextBox("✓")
            .bold
            .withFontSize(Pixels(20))
            .withColor(LcBorderOk)
            .withSize(Size(28, 26))
            .moveTo(Point(r.x + r.width - 30, r.y + 6)))
        else
          List(Shape.Box(r, Fill.Color(RGBA(1.0, 1.0, 1.0, 0.62))))

    // ── Chrome ────────────────────────────────────────────────────────────────
    val backNode: SceneNode =
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.65))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))

    val progressNode: SceneNode =
      TextBox(s"${entry.letter} / 26")
        .withFontSize(Pixels(13))
        .withColor(RGBA(0.55, 0.55, 0.55, 0.6))
        .withSize(Size(56, 20))
        .moveTo(Point(bw - 64, 16))

    val allNodes = letterCardNodes ::: cardBases ::: wrongOverlay ::: cardOverlays ::: List(backNode, progressNode)
    Outcome(SceneUpdateFragment(Layer(allNodes*)))