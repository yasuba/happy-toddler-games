package toddler.scenes

import indigo.*
import indigo.scenes.*
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import toddler.models.Model

// ── Audio ─────────────────────────────────────────────────────────────────────

object BunnyAudio:
  private def play(file: String): Unit =
    val a = js.Dynamic.newInstance(g.Audio)(s"assets/sounds/$file")
    a.play()

  def boing(): Unit = { Music.duck(0.4); play("boing.mp3") }
  def pop():   Unit = { Music.duck(0.3); play("plop.mp3") }

// ── Sweet colour ──────────────────────────────────────────────────────────────

enum SweetColor:
  case Mint, Lavender, Peach, Lemon, Cherry

object SweetColor:
  def rgba(c: SweetColor): RGBA = c match
    case SweetColor.Mint     => RGBA(0.624, 0.882, 0.796, 1.0)  // #9FE1CB
    case SweetColor.Lavender => RGBA(0.808, 0.796, 0.965, 1.0)  // #CECBF6
    case SweetColor.Peach    => RGBA(0.961, 0.769, 0.702, 1.0)  // #F5C4B3
    case SweetColor.Lemon    => RGBA(0.980, 0.957, 0.627, 1.0)  // #FAF4A0
    case SweetColor.Cherry   => RGBA(0.957, 0.753, 0.820, 1.0)  // #F4C0D1

  val all: List[SweetColor] = List(Mint, Lavender, Peach, Lemon, Cherry)
  def byIndex(i: Int): SweetColor = all(((i % 5) + 5) % 5)

// ── Sweet (floating in game area) ─────────────────────────────────────────────

final case class Sweet(id: Int, pos: Vector2, color: SweetColor)

// ── Pile sweet (collected; rests at bottom) ───────────────────────────────────

final case class PileSweet(x: Int, y: Int, rotation: Double, color: SweetColor)

// ── Bunny state ───────────────────────────────────────────────────────────────

final case class BunnyState(
  pos:           Vector2,
  vel:           Vector2,
  facingRight:   Boolean,
  boostCooldown: Double,  // seconds until next jump allowed
  squashAmount:  Double,  // 0 = no squash; SquashAmt = full squash at timer 0
  squashTimer:   Double,  // seconds elapsed since squash; SquashDur = fully recovered
  groundedAt:    Double   // 0 = airborne; else Y of the surface the bunny rests on
)

object BunnyState:
  val Gravity       = 900.0  // px/s²
  val JumpStr       = 540.0  // px/s upward velocity on tap (fixed, always straight up)
  val MaxSpeed      = 950.0  // px/s cap
  val JumpCooldown  = 0.20   // seconds between jumps
  val SquashDur     = 0.22   // seconds to recover from squash
  val SquashAmt     = 0.28   // peak squash (squashX = 1+amt, squashY = 1-amt)
  val HardBounce    = 380.0  // min |vy| at impact to trigger squash
  val StopThreshold = 90.0   // |vy| below which the bunny settles instead of bouncing

  val initial: BunnyState = BunnyState(
    pos           = Vector2(220, 180),
    vel           = Vector2(280, -180),
    facingRight   = true,
    boostCooldown = 0.0,
    squashAmount  = 0.0,
    squashTimer   = SquashDur,
    groundedAt    = 0.0
  )

// ── Scene ─────────────────────────────────────────────────────────────────────

object BunnyScene extends Scene[Unit, Model, Unit]:
  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName                 = SceneName("bunny")
  val modelLens: Lens[Model, Model]   = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters      = EventFilters.Permissive
  val subSystems: Set[SubSystem]      = Set.empty

  private val BunnyBodyR        = 36     // body radius px
  private val BunnyHeadR        = 28     // head radius px
  private val SweetR            = 18.0   // in-game sweet radius
  private val BodyCollectDistSq = (BunnyBodyR + SweetR.toInt) * (BunnyBodyR + SweetR.toInt)
  private val HeadCollectDistSq = (BunnyHeadR + SweetR.toInt) * (BunnyHeadR + SweetR.toInt)
  private val PileAreaFrac      = 0.15   // bottom fraction reserved for pile
  private val PileSweetR        = 13     // pile sweet visual radius px
  private val PileSweetStep     = 30     // px between pile sweet centres
  private val InitSweetCount    = 4      // sweets on screen at once
  private val PlatH             = 14     // platform visual height px

  // A suspended platform the bunny can land on
  private case class Platform(x: Double, y: Double, w: Double)

  // Two platforms at fixed relative positions within the game area
  private def makePlatforms(bw: Double, pileTop: Double): List[Platform] =
    val pw = (bw * 0.24).max(60.0)
    List(
      Platform(bw * 0.08,  pileTop * 0.32, pw),
      Platform(bw * 0.62,  pileTop * 0.58, pw)
    )

  def updateViewModel(
    context: SceneContext[Unit], model: Model, viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  def updateModel(
    context: SceneContext[Unit], model: Model
  ): GlobalEvent => Outcome[Model] =

    case ViewportResize(vp) =>
      val newVp   = vp.toSize
      val bw      = newVp.width.toDouble
      val bh      = newVp.height.toDouble
      val pileTop = bh - bh * PileAreaFrac
      val dice    = context.dice

      val (newBunny, newSweets, newNextId) =
        if model.sweets.isEmpty then
          val b  = model.bunny.copy(pos = Vector2(bw * 0.30, pileTop * 0.35))
          val sw = spawnInitialSweets(newVp, b.pos, dice, model.nextSweetId)
          (b, sw, model.nextSweetId + sw.length)
        else
          val maxY = pileTop - BunnyBodyR.toDouble - 10.0
          val bPos = Vector2(
            model.bunny.pos.x.max(BunnyBodyR.toDouble).min(bw - BunnyBodyR),
            model.bunny.pos.y.max(40.0).min(maxY)
          )
          (model.bunny.copy(pos = bPos), model.sweets, model.nextSweetId)

      Outcome(model.copy(
        viewport    = newVp,
        bunny       = newBunny,
        sweets      = newSweets,
        nextSweetId = newNextId
      ))

    case e: PointerEvent.PointerDown =>
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else
        val b = model.bunny
        if b.boostCooldown <= 0.0 then
          // Straight-up jump — keep horizontal momentum; if near-still, add a random nudge
          val nudgeVx =
            if b.vel.x.abs < 30.0 then
              val mag = 90.0 + context.dice.roll(120).toDouble
              if context.dice.roll(2) == 1 then mag else -mag
            else b.vel.x
          val newVy = -BunnyState.JumpStr
          val spd   = Math.sqrt(nudgeVx * nudgeVx + newVy * newVy)
          val (cvx, cvy) =
            if spd > BunnyState.MaxSpeed
            then (nudgeVx / spd * BunnyState.MaxSpeed, newVy / spd * BunnyState.MaxSpeed)
            else (nudgeVx, newVy)
          BunnyAudio.boing()
          Outcome(model.copy(bunny = b.copy(
            vel           = Vector2(cvx, cvy),
            boostCooldown = BunnyState.JumpCooldown,
            groundedAt    = 0.0   // leave any surface immediately
          )))
        else Outcome(model)

    case FrameTick =>
      val dt   = context.delta.toDouble
      val vp   = model.viewport
      val bw   = vp.width.toDouble
      val bh   = vp.height.toDouble
      val dice = context.dice
      val b    = model.bunny

      // Lazy init: seed sweets if we reach FrameTick before a ViewportResize
      val (activeSweets, seedOffset) =
        if model.sweets.isEmpty && bw > 100 then
          val sw = spawnInitialSweets(vp, b.pos, dice, model.nextSweetId)
          (sw, sw.length)
        else (model.sweets, 0)
      val baseId = model.nextSweetId + seedOffset

      // ── Physics ─────────────────────────────────────────────────────────────
      val pileTop = bh - bh * PileAreaFrac
      val minX    = BunnyBodyR.toDouble
      val maxX    = bw - BunnyBodyR.toDouble
      val minY    = (BunnyBodyR + 20).toDouble
      val maxY    = pileTop - BunnyBodyR.toDouble - 10.0

      val platforms = makePlatforms(bw, pileTop)

      var vx          = b.vel.x
      var vy          = b.vel.y
      var px          = b.pos.x
      var py          = b.pos.y
      var groundedAt  = b.groundedAt
      var hardBounce  = false

      // Helper: land on a surface at surfaceY with the current impact speed
      def landOn(surfaceY: Double): Unit =
        val impact = vy.abs
        py = surfaceY
        if impact < BunnyState.StopThreshold then
          vy = 0.0; groundedAt = surfaceY
        else
          vy = -impact * 0.75
          if impact > BunnyState.HardBounce then hardBounce = true

      if groundedAt > 0 then
        // ── Grounded: slide with friction; fall if walked off surface edge ──────
        px += vx * dt
        if px < minX then { px = minX; vx =  vx.abs * 0.92 }
        if px > maxX then { px = maxX; vx = -vx.abs * 0.92 }
        vx *= Math.pow(0.04, dt)          // ~4% speed remains after 1 s
        if vx.abs < 5.0 then vx = 0.0
        py = groundedAt
        vy = 0.0
        // Check whether the bunny is still over its surface
        val stillOnFloor    = groundedAt >= maxY - 1.0
        val stillOnPlatform = platforms.exists { p =>
          px - BunnyBodyR < p.x + p.w && px + BunnyBodyR > p.x &&
          (groundedAt - (p.y - BunnyBodyR)).abs < 2.0
        }
        if !stillOnFloor && !stillOnPlatform then
          groundedAt = 0.0   // walked off edge — become airborne

      else if vx.abs > 0.5 || vy.abs > 0.5 then
        // ── Airborne: gravity, movement, floor + platform collisions ─────────────
        vy += BunnyState.Gravity * dt
        px += vx * dt
        py += vy * dt

        if px < minX then { px = minX; vx =  vx.abs * 0.92 }
        if px > maxX then { px = maxX; vx = -vx.abs * 0.92 }
        if py < minY then { py = minY; vy =  vy.abs * 0.88 }

        // Floor
        if py > maxY then landOn(maxY)
        else if vy > 0 then
          // Platforms — only check while moving downward
          for p <- platforms do
            val overPlatform = px - BunnyBodyR < p.x + p.w && px + BunnyBodyR > p.x
            // crossed the platform top this frame (bunny centre was above it, now at/below)
            val crossedTop   = py >= p.y - BunnyBodyR && py - vy * dt < p.y - BunnyBodyR
            if overPlatform && crossedTop && groundedAt == 0.0 then
              landOn(p.y - BunnyBodyR)

      val newSquashAmount = if hardBounce then BunnyState.SquashAmt else b.squashAmount
      val newSquashTimer  = if hardBounce then 0.0
                            else (b.squashTimer + dt).min(BunnyState.SquashDur)

      val newFacing    = if Math.abs(vx) > 10.0 then vx > 0.0 else b.facingRight
      val newBoostCool = (b.boostCooldown - dt).max(0.0)

      val newBunny = b.copy(
        pos           = Vector2(px, py),
        vel           = Vector2(vx, vy),
        facingRight   = newFacing,
        boostCooldown = newBoostCool,
        squashAmount  = newSquashAmount,
        squashTimer   = newSquashTimer,
        groundedAt    = groundedAt
      )

      // ── Sweet collection — body or head contact ───────────────────────────────
      val d      = if newBunny.facingRight then 1.0 else -1.0
      val headCx = px + 8.0 * d
      val headCy = py - 48.0

      val (collected, remaining) = activeSweets.partition { sw =>
        val bdx = px - sw.pos.x;     val bdy = py - sw.pos.y
        val hdx = headCx - sw.pos.x; val hdy = headCy - sw.pos.y
        bdx*bdx + bdy*bdy < BodyCollectDistSq ||
        hdx*hdx + hdy*hdy < HeadCollectDistSq
      }

      if collected.nonEmpty then BunnyAudio.pop()

      val newPileSweets: List[PileSweet] = collected.zipWithIndex.map { case (sw, i) =>
        val idx = model.pileSweets.length + i
        val (plx, ply) = pilePosition(idx, bw.toInt, bh.toInt)
        val rot = (dice.roll(31) - 16).toDouble * Math.PI / 180.0
        PileSweet(plx, ply, rot, sw.color)
      }

      val replacements: List[Sweet] = collected.zipWithIndex.map { case (_, i) =>
        spawnOneSweet(baseId + i, vp, Vector2(px, py), remaining, dice)
      }

      Outcome(model.copy(
        bunny       = newBunny,
        sweets      = remaining ++ replacements,
        pileSweets  = model.pileSweets ++ newPileSweets,
        nextSweetId = baseId + collected.length
      ))

    case _ => Outcome(model)

  // ── Helpers ──────────────────────────────────────────────────────────────────

  private def spawnInitialSweets(vp: Size, bunnyPos: Vector2, dice: Dice, startId: Int): List[Sweet] =
    SweetColor.all.take(InitSweetCount).zipWithIndex.map { case (color, i) =>
      spawnOneSweet(startId + i, vp, bunnyPos, Nil, dice, Some(color))
    }

  private def spawnOneSweet(
    id: Int, vp: Size, bunnyPos: Vector2,
    existing: List[Sweet], dice: Dice,
    forceColor: Option[SweetColor] = None
  ): Sweet =
    val bw      = vp.width.toDouble
    val bh      = vp.height.toDouble
    val pileTop = bh - bh * PileAreaFrac
    val color   = forceColor.getOrElse(SweetColor.byIndex(dice.roll(5) - 1))
    def ru(): Double = (dice.roll(1000) - 1).toDouble / 999.0
    val pos = (0 until 12).iterator.map { _ =>
      Vector2(
        60.0 + ru() * (bw - 120.0).max(1.0),
        80.0 + ru() * (pileTop - 160.0).max(1.0)
      )
    }.find { p =>
      val dbx = p.x - bunnyPos.x; val dby = p.y - bunnyPos.y
      val bunnyFar = dbx * dbx + dby * dby > 100.0 * 100.0
      val notOverlap = existing.forall { sw =>
        val dx = p.x - sw.pos.x; val dy = p.y - sw.pos.y
        dx * dx + dy * dy > 60.0 * 60.0
      }
      bunnyFar && notOverlap
    }.getOrElse(Vector2(bw * 0.5, pileTop * 0.4))
    Sweet(id, pos, color)

  private def pilePosition(idx: Int, bw: Int, bh: Int): (Int, Int) =
    val margin       = 18
    val sweetsPerRow = ((bw - 2 * margin) / PileSweetStep).max(1)
    val col = idx % sweetsPerRow
    val row = idx / sweetsPerRow
    val x   = margin + PileSweetR + col * PileSweetStep
    val y   = bh - PileSweetR - 6 - row * PileSweetStep
    (x, y)

  private def computeTilt(vel: Vector2): Double =
    val maxTilt     = Math.PI / 12.0  // ±15°
    val speedFactor = (Math.sqrt(vel.x * vel.x + vel.y * vel.y) / 600.0).min(1.0)
    (vel.x / 600.0).max(-1.0).min(1.0) * maxTilt * speedFactor

  // ── Present ──────────────────────────────────────────────────────────────────

  def present(
    context: SceneContext[Unit], model: Model, viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val bw      = model.viewport.width
    val bh      = model.viewport.height
    val pileTop = bh - (bh.toDouble * PileAreaFrac).toInt
    val b       = model.bunny

    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw, bh), Fill.Color(RGBA(0.992, 0.965, 0.933, 1.0)))

    val pileAreaNode: SceneNode =
      Shape.Box(
        Rectangle(0, pileTop, bw, bh - pileTop),
        Fill.Color(RGBA(0.94, 0.88, 0.80, 0.20))
      )

    val groundNode: SceneNode =
      Shape.Line(
        Point(0, pileTop), Point(bw, pileTop),
        Stroke(2, RGBA(0.72, 0.64, 0.56, 0.30))
      )

    val platforms = makePlatforms(bw.toDouble, pileTop.toDouble)

    val platformNodes: List[SceneNode] = platforms.flatMap(drawPlatform)

    val pileNodes: List[SceneNode] = model.pileSweets.flatMap { ps =>
      drawSweet(ps.x, ps.y, PileSweetR, ps.color)
    }

    val sweetNodes: List[SceneNode] = model.sweets.flatMap { sw =>
      drawSweet(sw.pos.x.toInt, sw.pos.y.toInt, SweetR.toInt, sw.color)
    }

    val tilt = computeTilt(b.vel)
    val t    = (b.squashTimer / BunnyState.SquashDur).min(1.0)
    val sq   = b.squashAmount * (1.0 - t)
    val bunnyNodes: List[SceneNode] =
      drawBunny(b.pos.x.toInt, b.pos.y.toInt, b.facingRight,
                tilt = tilt, squashX = 1.0 + sq, squashY = 1.0 - sq)

    val backNode: SceneNode =
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.65))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))

    val allNodes =
      bgNode :: pileAreaNode :: groundNode ::
      platformNodes ::: pileNodes ::: sweetNodes ::: bunnyNodes ::: List(backNode)
    Outcome(SceneUpdateFragment(Layer(allNodes*)))

  // ── Platform drawing ──────────────────────────────────────────────────────────

  private def drawPlatform(p: Platform): List[SceneNode] =
    val x = p.x.toInt; val y = p.y.toInt; val w = p.w.toInt
    List(
      // Drop shadow
      Shape.Box(Rectangle(x + 3, y + 5, w, PlatH), Fill.Color(RGBA(0.0, 0.0, 0.0, 0.10))),
      // Body — warm wooden tone
      Shape.Box(Rectangle(x, y, w, PlatH), Fill.Color(RGBA(0.76, 0.58, 0.38, 1.0))),
      // Top shine strip
      Shape.Box(Rectangle(x, y, w, 3), Fill.Color(RGBA(1.0, 1.0, 1.0, 0.20)))
    )

  // ── Sweet drawing ─────────────────────────────────────────────────────────────

  private def drawSweet(cx: Int, cy: Int, r: Int, color: SweetColor): List[SceneNode] =
    val base  = SweetColor.rgba(color)
    val inner = (r * 62 / 100).max(3)
    val shine = (r * 28 / 100).max(2)
    List(
      Shape.Circle(Point(cx + 2, cy + 3), r,     Fill.Color(base.withAlpha(0.28))),
      Shape.Circle(Point(cx,     cy    ), r,     Fill.Color(base)),
      Shape.Circle(Point(cx - 2, cy - 2), inner, Fill.Color(RGBA(1.0, 1.0, 1.0, 0.22))),
      Shape.Circle(Point(cx - r/3, cy - r/3), shine, Fill.Color(RGBA(1.0, 1.0, 1.0, 0.58)))
    )

  // ── Bunny drawing ─────────────────────────────────────────────────────────────
  //
  // Translated from assets/images/bunny.svg (viewBox 0 0 220 180).
  // All offsets are relative to the SVG body-ellipse centre (100, 112).
  // sp() converts SVG offsets → screen Points, applying direction (d),
  // base scale (bs = 0.75 → ~140px wide at scale=1.0), squash, and tilt.
  // makeEllipse() approximates a rotated SVG ellipse as a 16-point polygon;
  // rotation angle is multiplied by d internally to auto-mirror.

  private val BodyColor    = RGBA(0.361, 0.200, 0.090, 1.0)  // #5C3317
  private val ShadingColor = RGBA(0.482, 0.290, 0.173, 1.0)  // #7B4A2D
  private val TailColor    = RGBA(0.627, 0.439, 0.310, 1.0)  // #A0704F
  private val OutlineColor = RGBA(0.239, 0.122, 0.051, 1.0)  // #3D1F0D
  private val SpoonHandle  = RGBA(0.784, 0.784, 0.784, 1.0)  // #C8C8C8
  private val SpoonBowl    = RGBA(0.878, 0.878, 0.878, 1.0)  // #E0E0E0

  private def xf(cx: Int, cy: Int, dx: Double, dy: Double,
                  sx: Double, sy: Double, tilt: Double): Point =
    val sdx  = dx * sx
    val sdy  = dy * sy
    val cosA = Math.cos(tilt); val sinA = Math.sin(tilt)
    Point(
      cx + (sdx * cosA - sdy * sinA).toInt,
      cy + (sdx * sinA + sdy * cosA).toInt
    )

  def drawBunny(
    cx: Int, cy: Int,
    facingRight:  Boolean,
    tilt:         Double = 0.0,
    squashX:      Double = 1.0,
    squashY:      Double = 1.0,
    scale:        Double = 1.0,
    bounceOffset: Int    = 0,
    earWiggle:    Double = 0.0
  ): List[SceneNode] =
    val d   = if facingRight then 1.0 else -1.0
    val bcy = cy + bounceOffset
    val bs  = 0.75  // base scale: SVG px → screen px at scale=1.0

    // Convert a raw SVG body-relative offset to a screen Point.
    // d is applied to x here; callers pass unsigned SVG offsets.
    def sp(svgDx: Double, svgDy: Double): Point =
      xf(cx, bcy, svgDx * d * bs, svgDy * bs, squashX * scale, squashY * scale, tilt)

    def si(n: Double): Int = (n * scale * bs).toInt.max(1)

    val sw = Stroke(si(2), OutlineColor)

    // Approximate a rotated SVG ellipse as a 16-point polygon.
    // svgOx/svgOy: centre offset from body centre in raw SVG units (no d pre-applied).
    // angleDeg: SVG rotation in degrees — flipped by d internally to auto-mirror.
    def makeEllipse(svgOx: Double, svgOy: Double,
                    svgRx: Double, svgRy: Double,
                    angleDeg: Double,
                    fill: Fill, stroke: Stroke = Stroke.None): SceneNode =
      val ar   = angleDeg * Math.PI / 180.0 * d
      val cosA = Math.cos(ar); val sinA = Math.sin(ar)
      val pts  = Batch.fromList(List.tabulate(16) { i =>
        val t  = 2 * Math.PI * i / 16
        val ex = svgRx * Math.cos(t)
        val ey = svgRy * Math.sin(t)
        sp(svgOx + ex * cosA - ey * sinA,
           svgOy + ex * sinA + ey * cosA)
      })
      Shape.Polygon(pts, fill, stroke)

    // 1. Tail: y tracks the body's rotated back end so it sits correctly in both directions.
    //    Body back-end y ≈ 60·sin(22°)·d SVG units from centre; tail stays 26.5 SVG units above that.
    val tailSvgY = 60.0 * Math.sin(22.0 * Math.PI / 180.0) * d - 15
    val tail = Shape.Circle(sp(-58, tailSvgY), si(14), Fill.Color(TailColor))

    // 2. Back legs: haunch (65,133)→(-35,21) rotate=-12; shin (30,150)→(-70,38) rotate=8
    val backHaunch = makeEllipse(-35, 21, 30, 19, -12.0, Fill.Color(BodyColor), sw)
    val backShin   = makeEllipse(-70, 38, 22, 11,   8.0, Fill.Color(BodyColor), sw)

    // 3. Body (100,112)→(0,0) rotate=-22; belly (104,120)→(4,8) rotate=-22
    val body  = makeEllipse(  0,  0, 60, 25, -22.0, Fill.Color(BodyColor),    sw)
    val belly = makeEllipse(  4,  8, 36, 13, -22.0, Fill.Color(ShadingColor), Stroke.None)

    // 4. Far ear (ear-left in SVG — sits behind near ear)
    //    Outer (152,32)→(52,-80) rx=8 ry=23 rotate=32; inner (152,33)→(52,-79) rx=4 ry=15
    val farEarOuter = makeEllipse(52, -80,  8, 23, 32.0 + earWiggle, Fill.Color(BodyColor),    sw)
    val farEarInner = makeEllipse(52, -79,  4, 15, 32.0 + earWiggle, Fill.Color(ShadingColor), Stroke.None)

    // 5. Near ear (ear-right in SVG)
    //    Outer (163,34)→(63,-78) rx=9 ry=25 rotate=18; inner (163,35)→(63,-77) rx=5 ry=17
    val nearEarOuter = makeEllipse(63, -78,  9, 25, 18.0 + earWiggle, Fill.Color(BodyColor),    sw)
    val nearEarInner = makeEllipse(63, -77,  5, 17, 18.0 + earWiggle, Fill.Color(ShadingColor), Stroke.None)

    // 6. Head: circle (155,82)→(55,-30) r=30; nose ellipse (176,88)→(76,-24); tip dot at (93,-24)
    val head    = Shape.Circle(sp(55, -30), si(30), Fill.Color(BodyColor))
    val nose    = makeEllipse(76, -24, 17, 15, 0.0, Fill.Color(ShadingColor), sw)
    val noseTip = Shape.Circle(sp(93, -24), si(3), Fill.Color(OutlineColor))

    // 7. Front legs: upper (152,118)→(52,6) rx=25 ry=12 rotate=28;
    //               paw   (183,136)→(83,24) rx=21 ry=10 rotate=-8
    val frontUpper = makeEllipse(52,  6, 25, 12,  28.0, Fill.Color(BodyColor), sw)
    val frontPaw   = makeEllipse(83, 24, 21, 10,  -8.0, Fill.Color(BodyColor), sw)

    // 8. Eye: sclera (169,76)→(69,-36) r=8; pupil (171,76)→(71,-36) r=5;
    //         shine (173,74)→(73,-38) r=2
    val eyeSclera = Shape.Circle(sp( 69, -36), si(8), Fill.Color(RGBA(1.0, 1.0, 1.0, 1.0)))
    val eyePupil  = Shape.Circle(sp( 71, -36), si(5), Fill.Color(OutlineColor))
    val eyeShine  = Shape.Circle(sp( 73, -38), si(2), Fill.Color(RGBA(1.0, 1.0, 1.0, 0.75)))

    // 9. Spoon: handle (193,130)→(93,18) to (205,98)→(105,-14);
    //           bowl (205,94)→(105,-18) rx=10 ry=7 rotate=-20;
    //           shade (204,93)→(104,-19) rx=5 ry=3.5 rotate=-20
    val spoonStick = Shape.Line(sp(93, 18), sp(105, -14), Stroke(si(5).max(2), SpoonHandle))
    val spoonBowl  = makeEllipse(105, -18, 10, 7.0, -20.0,
                       Fill.Color(SpoonBowl), Stroke(si(2), SpoonHandle))
    val spoonShade = makeEllipse(104, -19,  5, 3.5, -20.0,
                       Fill.Color(SpoonHandle.withAlpha(0.6)), Stroke.None)

    List(tail, backHaunch, backShin,
         body, belly,
         farEarOuter, farEarInner,
         nearEarOuter, nearEarInner,
         head, nose, noseTip,
         frontUpper, frontPaw,
         eyeSclera, eyePupil, eyeShine,
         spoonStick, spoonBowl, spoonShade)