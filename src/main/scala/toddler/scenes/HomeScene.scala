package toddler.scenes

import indigo.*
import indigo.scenes.*
import toddler.models.*
import toddler.scenes.BunnyScene

// ── Home screen state ─────────────────────────────────────────────────────────

final case class HomeScreenState(
  activeCard: Int,
  stripX:     Double,          // pixels scrolled; 0 = card 0, W = card 1, 2W = card 2
  targetX:    Double,          // snap target, animated toward in FrameTick
  dragStartX: Option[Double],  // finger-down X while dragging
  fadeIn:     Double           // 1.0 = opaque warm-white overlay; drains to 0 on entry
)

object HomeScreenState:
  val initial: HomeScreenState =
    HomeScreenState(activeCard = 0, stripX = 0.0, targetX = 0.0, dragStartX = None, fadeIn = 0.0)

// ── Scene ─────────────────────────────────────────────────────────────────────

object HomeScene extends Scene[Unit, Model, Unit]:
  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName               = SceneName("home")
  val modelLens: Lens[Model, Model] = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters    = EventFilters.Permissive
  val subSystems: Set[SubSystem]    = Set.empty

  // ── Card definitions (in order: 0=Bubbles, 1=Letters, 2=ComingSoon) ─────────

  private val cardBg: List[RGBA] = List(
    RGBA(0.882, 0.961, 0.933, 1.0),  // mint-tinted   — bubbles
    RGBA(0.980, 0.933, 0.855, 1.0),  // amber/peach   — letters
    RGBA(0.980, 0.918, 0.886, 1.0),  // warm peach    — chicken
    RGBA(0.980, 0.918, 0.886, 1.0),  // warm peach    — tictactoe
    RGBA(0.965, 0.933, 0.973, 1.0)   // soft lavender — bunny
  )

  private val cardLabel: List[String] = List("bubbles", "letters", "mrs chicken", "tictactoe", "bunny")

  private val cardTarget: List[Option[SceneName]] = List(
    Some(FloatingCircleScene.name),
    Some(LettersScene.name),
    Some(ChickenScene.name),
    Some(TictactoeScene.name),
    Some(BunnyScene.name)
  )

  // ── Update ────────────────────────────────────────────────────────────────

  def updateViewModel(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  def updateModel(
      context: SceneContext[Unit], model: Model
  ): GlobalEvent => Outcome[Model] =

    case ViewportResize(vp) =>
      val newVp = vp.toSize
      // Keep strip snapped to current card after resize
      val snapped = model.homeScreen.activeCard * newVp.width.toDouble
      Outcome(model.copy(
        viewport   = newVp,
        homeScreen = model.homeScreen.copy(stripX = snapped, targetX = snapped)
      ))

    case FrameTick =>
      val dt = context.delta.toDouble
      val hs = model.homeScreen
      // Lerp stripX toward targetX when not in a live drag
      val newStripX =
        if hs.dragStartX.isDefined then hs.stripX
        else
          val diff = hs.targetX - hs.stripX
          if Math.abs(diff) < 0.5 then hs.targetX
          else hs.stripX + diff * Math.min(1.0, 14.0 * dt)
      val newFadeIn = (hs.fadeIn - dt * 3.0).max(0.0)
      Outcome(model.copy(homeScreen = hs.copy(stripX = newStripX, fadeIn = newFadeIn)))

    case e: PointerEvent.PointerDown =>
      Outcome(model.copy(homeScreen =
        model.homeScreen.copy(dragStartX = Some(e.position.x.toDouble))
      ))

    case e: PointerEvent.PointerMove =>
      model.homeScreen.dragStartX match
        case None => Outcome(model)
        case Some(startX) =>
          val W      = model.viewport.width.toDouble
          val delta  = startX - e.position.x.toDouble   // +ve = swipe left = go to next card
          val clampX = (model.homeScreen.activeCard * W + delta).max(0.0).min(4.0 * W)
          Outcome(model.copy(homeScreen = model.homeScreen.copy(stripX = clampX)))

    case e: PointerEvent.PointerUp =>
      val hs  = model.homeScreen
      val W   = model.viewport.width.toDouble
      val bw  = model.viewport.width
      hs.dragStartX match
        case None => Outcome(model)
        case Some(startX) =>
          val delta = startX - e.position.x.toDouble
          // Clear drag state; reset target so aborted swipes snap back
          val hs1 = hs.copy(dragStartX = None, targetX = hs.activeCard * W)

          if Math.abs(delta) < 30.0 then
            // ── Tap ─────────────────────────────────────────────────────────
            // Settings button occupies the top-right 58×58 px area
            if e.position.x >= bw - 58 && e.position.y < 58 then
              Music.onSceneChange()
              Outcome(model.copy(homeScreen = hs1))
                .addGlobalEvents(SceneEvent.JumpTo(SettingsScene.name))
            else
              cardTarget(hs.activeCard) match
                case Some(sceneName) =>
                  // Set fadeIn=1 so the screen fades back in when we return
                  Music.onSceneChange()
                  Outcome(model.copy(homeScreen = hs1.copy(fadeIn = 1.0)))
                    .addGlobalEvents(SceneEvent.JumpTo(sceneName))
                case None =>
                  // Coming soon — tap does nothing
                  Outcome(model.copy(homeScreen = hs1))
          else
            // ── Swipe ───────────────────────────────────────────────────────
            val newCard = if delta > 0.0 then (hs.activeCard + 1).min(4)
                          else (hs.activeCard - 1).max(0)
            Outcome(model.copy(homeScreen = hs.copy(
              dragStartX = None,
              activeCard = newCard,
              targetX    = newCard * W
            )))

    case _ => Outcome(model)

  // ── Present ───────────────────────────────────────────────────────────────

  def present(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val bw       = model.viewport.width
    val bh       = model.viewport.height
    val hs       = model.homeScreen
    val scrollPx = hs.stripX.toInt

    // Card geometry
    val cardPadH  = 28                        // horizontal padding inside each W-wide slot
    val cardY     = 64                        // top of cards (below title)
    val cardW     = bw - 2 * cardPadH
    val cardH     = (bh - cardY - 80).max(50) // bottom 80px reserved for dots

    val iconCyRel = (cardH * 0.36).toInt   // icon centre, relative to cardY
    val nameCyRel = (cardH * 0.70).toInt   // name text top, relative to cardY

    // ── Cards ─────────────────────────────────────────────────────────────
    val cardNodes: List[SceneNode] = (0 until 5).toList.flatMap: i =>
      val slotLeft = i * bw - scrollPx   // left edge of this card's W-wide slot
      val cx       = slotLeft + cardPadH  // card left edge
      val icx      = slotLeft + bw / 2   // horizontal centre of slot
      val iconCy   = cardY + iconCyRel
      val nameCy   = cardY + nameCyRel
      val hintCy   = nameCy + 46

      // Background with very subtle border
      val bgNode: SceneNode =
        Shape.Box(
          Rectangle(cx, cardY, cardW, cardH),
          Fill.Color(cardBg(i)),
          Stroke(1, RGBA(0.0, 0.0, 0.0, 0.07))
        )

      // Icon — card-specific
      val iconNodes: List[SceneNode] = i match
        case 0 =>
          // Bubbles: cluster of 4 soft pastel circles
          List(
            Shape.Circle(Point(icx - 18, iconCy + 10), 26,
                         Fill.Color(PastelColors(0).rgba.withAlpha(0.85))),
            Shape.Circle(Point(icx + 20, iconCy + 14), 20,
                         Fill.Color(PastelColors(1).rgba.withAlpha(0.85))),
            Shape.Circle(Point(icx + 4,  iconCy - 24), 24,
                         Fill.Color(PastelColors(2).rgba.withAlpha(0.85))),
            Shape.Circle(Point(icx - 32, iconCy - 6),  14,
                         Fill.Color(PastelColors(3).rgba.withAlpha(0.85)))
          )
        case 1 =>
          // Letters: large "A" in amber/brown
          val fontSize = (cardH * 0.40).toInt.min(150)
          List(
            TextBox("A")
              .bold
              .withFontSize(Pixels(fontSize))
              .withColor(RGBA(0.388, 0.220, 0.024, 0.80))
              .alignCenter
              .withSize(Size(cardW, fontSize + 12))
              .moveTo(Point(cx, iconCy - fontSize / 2))
          )
        case 2 =>
          // Mrs Chicken: scaled-down thumbnail using the shared draw helper
          ChickenScene.drawChicken(icx, iconCy + 10, facingRight = true, scale = 0.5)

        case 3 =>
          val fontSize = (cardH * 0.40).toInt.min(150)
          List(
            TextBox("TicTacToe")
              .bold
              .withFontSize(Pixels(fontSize))
              .withColor(RGBA(0.388, 0.220, 0.024, 0.80))
              .alignCenter
              .withSize(Size(cardW, fontSize + 12))
              .moveTo(Point(cx, iconCy - fontSize / 2))
          )

        case _ =>
          // Bunny card: scaled-down bunny thumbnail
          BunnyScene.drawBunny(icx, iconCy + 10, facingRight = true, scale = 0.5)

      // Game name
      val nameNode: SceneNode =
        TextBox(cardLabel(i))
          .bold
          .withFontSize(Pixels(34))
          .withColor(RGBA(0.22, 0.22, 0.22, 0.88))
          .alignCenter
          .withSize(Size(cardW, 48))
          .moveTo(Point(cx, nameCy))

      // "tap to play" hint — playable cards only
      val hintNodes: List[SceneNode] =
        if cardTarget(i).isDefined then
          List(
            TextBox("tap to play")
              .withFontSize(Pixels(13))
              .withColor(RGBA(0.38, 0.38, 0.38, 0.48))
              .alignCenter
              .withSize(Size(cardW, 22))
              .moveTo(Point(cx, hintCy))
          )
        else List.empty

      bgNode :: iconNodes ::: nameNode :: hintNodes

    // ── App title placeholder ──────────────────────────────────────────────
    val titleNode: SceneNode =
      TextBox("...")
        .withFontSize(Pixels(16))
        .withColor(RGBA(0.55, 0.55, 0.55, 0.38))
        .alignCenter
        .withSize(Size(bw, 28))
        .moveTo(Point(0, 20))

    // ── Settings icon (top-right) ──────────────────────────────────────────
    val settingsNode: SceneNode =
      TextBox("⚙")
        .withFontSize(Pixels(24))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.50))
        .withSize(Size(38, 34))
        .moveTo(Point(bw - 46, 14))

    // ── Dot indicators (bottom-centre) ────────────────────────────────────
    // 4 dots, gap=26px, centred on bw/2
    val dotY      = bh - 46
    val dotGap    = 26
    val dotStartX = bw / 2 - dotGap * 2   // 5 dots centred
    val dotColor  = RGBA(0.55, 0.50, 0.48, 1.0)
    val dotNodes: List[SceneNode] = (0 until 5).toList.map: i =>
      val isActive = hs.activeCard == i
      Shape.Circle(
        Point(dotStartX + i * dotGap, dotY),
        if isActive then 8 else 5,
        Fill.Color(dotColor.withAlpha(if isActive then 0.88 else 0.35))
      )

    // ── Fade-in overlay ────────────────────────────────────────────────────
    // Warm white overlay that fades away when returning from a game scene
    val fadeOverlay: List[SceneNode] =
      if hs.fadeIn > 0.01 then
        List(Shape.Box(
          Rectangle(0, 0, bw, bh),
          Fill.Color(RGBA(0.980, 0.957, 0.925, Math.min(hs.fadeIn, 1.0)))
        ))
      else List.empty

    val allNodes: List[SceneNode] =
      cardNodes ::: List(titleNode, settingsNode) ::: dotNodes ::: fadeOverlay

    Outcome(SceneUpdateFragment(Layer(allNodes*)))
