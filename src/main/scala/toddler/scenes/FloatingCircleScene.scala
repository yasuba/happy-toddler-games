package toddler.scenes

import indigo.*
import indigo.scenes.*
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

// ── Constants ────────────────────────────────────────────────────────────────

val PanelWidth      = 46   // right-edge side panel, pixels
val GaugeWidth      = 28   // gauge bar width, centred inside the panel
val TotalBubbles    = 8
val BubbleMinR      = 38
val BubbleMaxR      = 68
val PopDuration     = 0.35
val PulseDuration   = 0.55 // seconds for one gentle scale pulse
val CelebrationDuration = 2.2

val ChangeInterval  = 6.0  // seconds for one full gauge drain
val DrainRate       = 1.0 / ChangeInterval

// ── Audio ────────────────────────────────────────────────────────────────────

object Audio:
  private lazy val ctx: js.Dynamic = js.Dynamic.newInstance(g.AudioContext)()

  def playPop(): Unit =
    Music.duck(0.35)
    val c    = ctx
    val osc  = c.createOscillator()
    val gain = c.createGain()
    osc.connect(gain)
    gain.connect(c.destination)
    osc.updateDynamic("type")("sine")
    val now = c.currentTime.asInstanceOf[Double]
    osc.frequency.setValueAtTime(520.0, now)
    osc.frequency.exponentialRampToValueAtTime(140.0, now + 0.13)
    gain.gain.setValueAtTime(0.22, now)
    gain.gain.exponentialRampToValueAtTime(0.001, now + 0.18)
    osc.start(now)
    osc.stop(now + 0.18)

  def playChime(): Unit =
    Music.duck(2.0)
    val c     = ctx
    val start = c.currentTime.asInstanceOf[Double] + 0.2
    val freqs = List(440.0, 523.0, 659.0, 784.0) // A4 C5 E5 G5
    freqs.zipWithIndex.foreach { case (freq, i) =>
      val t    = start + i * 0.32
      val osc  = c.createOscillator()
      val gain = c.createGain()
      osc.connect(gain)
      gain.connect(c.destination)
      osc.updateDynamic("type")("sine")
      osc.frequency.setValueAtTime(freq, t)
      gain.gain.setValueAtTime(0.0, t)
      gain.gain.linearRampToValueAtTime(0.15, t + 0.05)
      gain.gain.exponentialRampToValueAtTime(0.001, t + 0.4)
      osc.start(t)
      osc.stop(t + 0.4)
    }

  def speakNumber(n: Int): Unit =
    Music.duck(2.0)
    val utterance = js.Dynamic.newInstance(g.SpeechSynthesisUtterance)(n.toString)
    utterance.rate   = 0.78
    utterance.pitch  = 1.15
    utterance.volume = 1.0
    g.speechSynthesis.speak(utterance)

  // Play the pre-recorded phonics MP3 for a letter
  def playPhonics(letter: Char): Unit =
    Music.duck(1.5)
    g.speechSynthesis.cancel()
    val audio = js.Dynamic.newInstance(g.Audio)(s"assets/sounds/phonics/$letter.mp3")
    audio.play()

  // Play the phonics MP3, then speak the word via TTS once the clip ends
  def playPhonicsAndWord(letter: Char, word: String): Unit =
    Music.duck(4.0)
    g.speechSynthesis.cancel()
    val audio = js.Dynamic.newInstance(g.Audio)(s"assets/sounds/phonics/$letter.mp3")
    val cb: js.Function1[js.Any, Unit] = (_: js.Any) =>
      val utterance = js.Dynamic.newInstance(g.SpeechSynthesisUtterance)(word)
      utterance.lang   = "en-GB"
      utterance.rate   = 0.75
      utterance.pitch  = 1.1
      utterance.volume = 1.0
      g.speechSynthesis.speak(utterance)
    audio.addEventListener("ended", cb)
    audio.play()

// ── Palette ──────────────────────────────────────────────────────────────────

enum PastelColor(val rgba: RGBA):
  case Mint     extends PastelColor(RGBA(0.624, 0.882, 0.796, 1.0)) // #9FE1CB
  case Lavender extends PastelColor(RGBA(0.808, 0.796, 0.965, 1.0)) // #CECBF6
  case Pink     extends PastelColor(RGBA(0.988, 0.863, 0.953, 1.0)) // #FCDCF3
  case Yellow   extends PastelColor(RGBA(0.988, 0.988, 0.733, 1.0)) // #FCFCBB

val PastelColors: List[PastelColor] =
  List(PastelColor.Mint, PastelColor.Lavender, PastelColor.Pink, PastelColor.Yellow)

// ── Confetti ──────────────────────────────────────────────────────────────────

final case class ConfettiParticle(
  pos:      Vector2,
  vel:      Vector2,
  radius:   Int,
  colorIdx: Int
)

final case class Celebration(
  timer:     Double,
  particles: List[ConfettiParticle]
)

def spawnConfetti(dice: Dice, vp: Size): List[ConfettiParticle] =
  val playW = (vp.width - PanelWidth).max(1)
  (0 until 28).map(_ =>
    val x  = dice.roll(playW).toDouble
    val y  = -(10.0 + dice.roll(50))
    val vx = (dice.roll(41) - 21).toDouble
    val vy = 80.0 + dice.roll(60)
    val r  = 5 + dice.roll(9)
    val c  = (dice.roll(4) - 1) % PastelColors.length
    ConfettiParticle(Vector2(x, y), Vector2(vx, vy), r, c)
  ).toList

// ── Bubble ───────────────────────────────────────────────────────────────────

final case class Bubble(
  id:         Int,
  pos:        Vector2,
  vel:        Vector2,
  radius:     Int,
  colorIdx:   Int,
  popTimer:   Option[Double],
  pulseTimer: Option[Double]
)

object Bubble:
  def rotateVel(vel: Vector2, angleDeg: Double): Vector2 =
    val a   = angleDeg * Math.PI / 180.0
    val cos = Math.cos(a)
    val sin = Math.sin(a)
    Vector2(vel.x * cos - vel.y * sin, vel.x * sin + vel.y * cos)

  def initialSpawn(id: Int, total: Int, vp: Size): Bubble =
    val cols  = 4
    val rows  = (total + cols - 1) / cols
    val pad   = BubbleMaxR + 4
    val stepX = (vp.width - PanelWidth - 2 * pad).max(0).toDouble / (cols - 1).max(1)
    val stepY = (vp.height - 2 * pad).max(0).toDouble / (rows - 1).max(1)
    val col   = id % cols
    val row   = id / cols
    val x     = pad + (col * stepX).toInt
    val y     = pad + (row * stepY).toInt
    val angle = (id.toDouble / total) * 2 * Math.PI + 0.3
    val speed = 32.0 + id * 7.0
    val r     = BubbleMinR + (id * 5) % (BubbleMaxR - BubbleMinR)
    Bubble(id, Vector2(x.toDouble, y.toDouble),
           Vector2(Math.cos(angle) * speed, Math.sin(angle) * speed),
           r, id % PastelColors.length, None, None)

  def spawn(id: Int, dice: Dice, vp: Size): Bubble =
    val r      = BubbleMinR + dice.roll(BubbleMaxR - BubbleMinR)
    val margin = r + 4
    val playW  = (vp.width - PanelWidth - 2 * margin).max(1)
    val playH  = (vp.height - 2 * margin).max(1)
    val angle  = dice.roll(360).toDouble * Math.PI / 180.0
    val speed  = 32.0 + dice.roll(50)
    Bubble(id,
           Vector2((margin + dice.roll(playW) - 1).toDouble, (margin + dice.roll(playH) - 1).toDouble),
           Vector2(Math.cos(angle) * speed, Math.sin(angle) * speed),
           r, id % PastelColors.length, None, None)

// ── Panel state ───────────────────────────────────────────────────────────────
// Replaces BorderState. The gauge drains from 1.0 → 0.0 over ChangeInterval
// seconds, then the colour advances and the gauge refills to 1.0.

final case class PanelState(colorIdx: Int, gauge: Double)

object PanelState:
  val initial: PanelState = PanelState(colorIdx = 0, gauge = 1.0)

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
  chickenEggsTotal: Int,              // running total, never resets
  nextChickenId:    Int               // monotonic id counter for new chickens
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
    nextChickenId     = 1
  )

// ── Scene ────────────────────────────────────────────────────────────────────

object FloatingCircleScene extends Scene[Unit, Model, Unit]:

  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName               = SceneName("floating circle")
  val modelLens: Lens[Model, Model] = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters    = EventFilters.Permissive
  val subSystems: Set[SubSystem]    = Set.empty

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
      val newVp = vp.toSize
      val (newBubbles, newNextId) =
        if model.bubbles.isEmpty then
          (0 until TotalBubbles).foldLeft((List.empty[Bubble], 0)):
            case ((bs, id), _) => (bs :+ Bubble.initialSpawn(id, TotalBubbles, newVp), id + 1)
        else (model.bubbles, model.nextId)
      Outcome(model.copy(viewport = newVp, bubbles = newBubbles, nextId = newNextId))

    case FrameTick =>
      val dt = context.delta.toDouble
      val vp = model.viewport

      model.celebration match

        // ── Celebration in progress ──────────────────────────────────────────
        case Some(cel) =>
          val newTimer = cel.timer + dt

          val updatedBubbles = model.bubbles.flatMap: bubble =>
            bubble.popTimer match
              case Some(t) =>
                val next = t + dt
                if next >= PopDuration then None else Some(bubble.copy(popTimer = Some(next)))
              case None => Some(bubble)

          val deficit = TotalBubbles - updatedBubbles.size
          val (finalBubbles, newNextId) = (0 until deficit.max(0)).foldLeft((updatedBubbles, model.nextId)):
            case ((bs, id), _) => (bs :+ Bubble.spawn(id, context.dice, vp), id + 1)

          val newParticles = cel.particles.map(p => p.copy(pos = p.pos + p.vel * dt))

          if newTimer >= CelebrationDuration then
            Outcome(model.copy(counter = 0, celebration = None, bubbles = finalBubbles, nextId = newNextId))
          else
            Outcome(model.copy(
              celebration = Some(cel.copy(timer = newTimer, particles = newParticles)),
              bubbles     = finalBubbles,
              nextId      = newNextId
            ))

        // ── Normal gameplay ──────────────────────────────────────────────────
        case None =>
          val bw = vp.width
          val bh = vp.height

          // ── Panel gauge ──
          val p         = model.panel
          val newGauge  = p.gauge - DrainRate * dt
          val colourJustChanged = newGauge <= 0.0
          val newPanel  =
            if colourJustChanged then
              PanelState(colorIdx = (p.colorIdx + 1) % PastelColors.length, gauge = 1.0)
            else
              p.copy(gauge = newGauge)

          // ── Bubbles ──
          val updatedBubbles = model.bubbles.flatMap: bubble =>
            bubble.popTimer match
              case Some(t) =>
                val next = t + dt
                if next >= PopDuration then None else Some(bubble.copy(popTimer = Some(next)))

              case None =>
                val newPos = bubble.pos + bubble.vel * dt
                val minX   = bubble.radius.toDouble
                val maxX   = (bw - PanelWidth - bubble.radius).toDouble
                val minY   = bubble.radius.toDouble
                val maxY   = (bh - bubble.radius).toDouble
                val hitX   = newPos.x < minX || newPos.x > maxX
                val hitY   = newPos.y < minY || newPos.y > maxY
                val rawVel = Vector2(
                  if hitX then -bubble.vel.x else bubble.vel.x,
                  if hitY then -bubble.vel.y else bubble.vel.y
                )
                val vel  = if hitX || hitY then Bubble.rotateVel(rawVel, context.dice.rollRange(-8, 8).toDouble) else rawVel
                val pos  = Vector2(newPos.x.max(minX).min(maxX), newPos.y.max(minY).min(maxY))
                val newPulse   = bubble.pulseTimer.map(_ + dt).filter(_ < PulseDuration)
                // Pulse matching bubbles when the colour changes
                val pulseTimer = if colourJustChanged && bubble.colorIdx == newPanel.colorIdx
                                 then Some(0.0) else newPulse
                Some(bubble.copy(pos = pos, vel = vel, pulseTimer = pulseTimer))

          val deficit = TotalBubbles - updatedBubbles.size
          val (finalBubbles, newNextId) = (0 until deficit.max(0)).foldLeft((updatedBubbles, model.nextId)):
            case ((bs, id), _) => (bs :+ Bubble.spawn(id, context.dice, vp), id + 1)

          Outcome(model.copy(panel = newPanel, bubbles = finalBubbles, nextId = newNextId))

    case e: PointerEvent.PointerDown =>
      // Back button — top-left 52×52 px
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else if model.celebration.isDefined then Outcome(model)
      else
        val tap = e.position.toVector
        val hit = model.bubbles
          .filter(_.popTimer.isEmpty)
          .find(b => (b.pos - tap).length <= b.radius.toDouble)
        hit.fold(Outcome(model)): b =>
          val isMatch = b.colorIdx == model.panel.colorIdx
          if isMatch then
            val newCounter = model.counter + 1
            val updated = model.bubbles.map: bubble =>
              if bubble.id == b.id then bubble.copy(popTimer = Some(0.0)) else bubble
            if newCounter >= 10 then
              Audio.playPop()
              Audio.speakNumber(newCounter)
              Audio.playChime()
              val particles = spawnConfetti(context.dice, model.viewport)
              Outcome(model.copy(
                bubbles     = updated,
                counter     = newCounter,
                celebration = Some(Celebration(0.0, particles))
              ))
            else
              Audio.playPop()
              Audio.speakNumber(newCounter)
              Outcome(model.copy(bubbles = updated, counter = newCounter))
          else
            Outcome(model.copy(bubbles = model.bubbles.filterNot(_.id == b.id)))

    case _ => Outcome(model)

  def present(
      context: SceneContext[Unit],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val bw = model.viewport.width
    val bh = model.viewport.height

    // ── Bubbles ──────────────────────────────────────────────────────────────
    val bubbleNodes: List[SceneNode] = model.bubbles.map: bubble =>
      val pulseScale = bubble.pulseTimer match
        case None    => 1.0
        case Some(t) => 1.0 + 0.15 * Math.sin(Math.PI * t / PulseDuration)
      bubble.popTimer match
        case None =>
          Shape.Circle(bubble.pos.toPoint, (bubble.radius * pulseScale).toInt,
                       Fill.Color(PastelColors(bubble.colorIdx).rgba))
        case Some(elapsed) =>
          val p = elapsed / PopDuration
          Shape.Circle(bubble.pos.toPoint, (bubble.radius * (1.0 + p * 0.6)).toInt,
                       Fill.Color(PastelColors(bubble.colorIdx).rgba.withAlpha(1.0 - p)))

    // ── Side panel ───────────────────────────────────────────────────────────
    val panelX     = bw - PanelWidth
    val gaugeX     = panelX + (PanelWidth - GaugeWidth) / 2
    val gaugePadT  = 24
    val pipRadius  = 8
    val pipSpacing = 22
    val pipsH      = 3 * pipSpacing + pipRadius * 2  // span of 4 pips
    val gaugePadB  = pipsH + 24                      // gap + pips + bottom margin
    val gaugeMaxH  = (bh - gaugePadT - gaugePadB).max(1)
    val gaugeTop   = gaugePadT

    // Panel background — very subtle tint to separate from game area
    val panelBg = Shape.Box(Rectangle(panelX, 0, PanelWidth, bh),
                            Fill.Color(RGBA(0.0, 0.0, 0.0, 0.11)))

    // Gauge track (capsule: rect body + circles at each end)
    val capR       = GaugeWidth / 2
    val trackColor = RGBA(0.0, 0.0, 0.0, 0.12)
    val trackNodes: List[SceneNode] = List(
      Shape.Box(Rectangle(gaugeX, gaugeTop + capR, GaugeWidth, (gaugeMaxH - GaugeWidth).max(0)), Fill.Color(trackColor)),
      Shape.Circle(Point(gaugeX + capR, gaugeTop + capR),          capR, Fill.Color(trackColor)), // top cap
      Shape.Circle(Point(gaugeX + capR, gaugeTop + gaugeMaxH - capR), capR, Fill.Color(trackColor))  // bottom cap
    )

    // Gauge fill — anchored at top, height proportional to gauge level;
    // as it drains toward 0 the bottom edge rises, emptying from the bottom
    val fillH     = (model.panel.gauge * gaugeMaxH).max(0.0).toInt
    val fillColor = PastelColors(model.panel.colorIdx).rgba
    val fillNodes: List[SceneNode] =
      if fillH <= GaugeWidth then
        // Too short for a rect body — just draw the top cap circle
        if fillH > 0 then List(Shape.Circle(Point(gaugeX + capR, gaugeTop + capR), capR, Fill.Color(fillColor)))
        else List.empty
      else
        List(
          Shape.Box(Rectangle(gaugeX, gaugeTop + capR, GaugeWidth, fillH - GaugeWidth), Fill.Color(fillColor)),
          Shape.Circle(Point(gaugeX + capR, gaugeTop + capR), capR, Fill.Color(fillColor)) // top cap
        )

    // Colour pips — current colour full opacity/size, upcoming at 35%
    val pipCenterX = panelX + PanelWidth / 2
    val pipsStartY = gaugeTop + gaugeMaxH + 14 + pipRadius
    val pipNodes: List[SceneNode] = (0 until 4).toList.map: i =>
      val pipColorIdx = (model.panel.colorIdx + i) % PastelColors.length
      val alpha  = if i == 0 then 1.0 else 0.65
      val radius = if i == 0 then pipRadius else pipRadius - 1
      val y      = pipsStartY + i * pipSpacing
      Shape.Circle(Point(pipCenterX, y), radius, Fill.Color(PastelColors(pipColorIdx).rgba.withAlpha(alpha)))

    val panelNodes: List[SceneNode] =
      panelBg :: trackNodes ::: fillNodes ::: pipNodes

    // ── Confetti ─────────────────────────────────────────────────────────────
    val confettiNodes: List[SceneNode] = model.celebration match
      case None => List.empty
      case Some(cel) =>
        val fadeStart = CelebrationDuration - 0.5
        val alpha = if cel.timer < fadeStart then 1.0
                    else ((CelebrationDuration - cel.timer) / 0.5).max(0.0).min(1.0)
        cel.particles.map: p =>
          Shape.Circle(p.pos.toPoint, p.radius, Fill.Color(PastelColors(p.colorIdx).rgba.withAlpha(alpha)))

    // ── Counter ───────────────────────────────────────────────────────────────
    val counterAlpha: Double = model.celebration match
      case None      => 0.72
      case Some(cel) =>
        val fadeStart = CelebrationDuration - 0.4
        if cel.timer < fadeStart then 0.72
        else (0.72 * (CelebrationDuration - cel.timer) / 0.4).max(0.0)

    val counterNode: SceneNode =
      TextBox(model.counter.toString)
        .bold
        .withFontSize(Pixels(96))
        .withColor(RGBA(0.35, 0.35, 0.35, counterAlpha))
        .alignCenter
        .withSize(Size(bw - PanelWidth, 130))
        .moveTo(Point(0, 28))

    val backNode: SceneNode =
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.55))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))

    val allNodes: List[SceneNode] = counterNode :: bubbleNodes ::: panelNodes ::: confettiNodes ::: List(backNode)
    Outcome(SceneUpdateFragment(Layer(allNodes*)))