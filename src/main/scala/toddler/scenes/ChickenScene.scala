package toddler.scenes

import indigo.*
import indigo.scenes.*
import toddler.models.Model

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global as g

// ── Audio ─────────────────────────────────────────────────────────────────────

object ChickenAudio:
  private def play(file: String): Unit =
    val audio = js.Dynamic.newInstance(g.Audio)(s"assets/sounds/$file")
    audio.play()

  def cluck():   Unit = { Music.duck(0.6); play("cluck.mp3") }
  def plop():    Unit = { Music.duck(0.4); play("plop.mp3") }
  def fanfare(): Unit = { Music.duck(3.0); play("fanfare.mp3") }

// ── Egg ───────────────────────────────────────────────────────────────────────

final case class Egg(
  pos:         Vector2,
  rotation:    Double,
  bounceTimer: Double,
  chickenId:   Int
)

object Egg:
  val BounceLen = 0.45

// ── Phase ─────────────────────────────────────────────────────────────────────

enum ChickenPhase:
  case Waddling
  case Pausing(timer: Double)
  case Laying(timer: Double, eggPlaced: Boolean)
  case Celebrating(timer: Double)

// ── State ─────────────────────────────────────────────────────────────────────

final case class ChickenState(
  id:              Int,
  pos:             Vector2,
  vel:             Vector2,
  facingRight:     Boolean,
  waddle:          Double,
  dirTimer:        Double,
  phase:           ChickenPhase,
  speed:           Double,      // px/s; increases after each celebration
  eggsAtLastCeleb: Int          // egg count when last celebration was triggered
)

object ChickenState:
  val BaseSpeed     = 90.0
  val PauseDur      = 0.45
  val WaddleFreq    = 6.0
  val BodyRadius    = 44
  val LayDur        = 0.6
  val FullEggCount  = 10
  val CelebDur      = 3.2
  val SpeedBoost    = 10.0    // added to every chicken after each celebration
  val MaxSpeed      = 160.0
  val MaxChickens   = 10

  val initial: ChickenState = ChickenState(
    id              = 0,
    pos             = Vector2(200, 300),
    vel             = Vector2(BaseSpeed, 1.0),
    facingRight     = true,
    waddle          = 0.0,
    dirTimer        = 2.5,
    phase           = ChickenPhase.Waddling,
    speed           = BaseSpeed,
    eggsAtLastCeleb = 0
  )

// ── Scene ─────────────────────────────────────────────────────────────────────

object ChickenScene extends Scene[Unit, Model, Unit]:
  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName                 = SceneName("chicken")
  val modelLens: Lens[Model, Model]   = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters      = EventFilters.Permissive
  val subSystems: Set[SubSystem]      = Set.empty

  def updateViewModel(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  def updateModel(
      context: SceneContext[Unit], model: Model
  ): GlobalEvent => Outcome[Model] =

    case ViewportResize(vp) =>
      val newVp  = vp.toSize
      val grassY = newVp.height - (newVp.height * 0.12).toInt
      val maxY   = grassY.toDouble - ChickenState.BodyRadius - 14.0
      // Reposition all chickens within the new bounds
      val clamped = model.chickens.zipWithIndex.map { (ck, i) =>
        val x = ck.pos.x.max(ChickenState.BodyRadius.toDouble).min(newVp.width.toDouble - ChickenState.BodyRadius)
        val y = if i == 0 then maxY else ck.pos.y.max(80.0).min(maxY)
        ck.copy(pos = Vector2(x, y))
      }
      Outcome(model.copy(viewport = newVp, chickens = clamped))

    case e: PointerEvent.PointerDown =>
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else
        // Find first tappable chicken that was hit
        val idx = model.chickens.indexWhere { ck =>
          (ck.phase match
            case ChickenPhase.Waddling | ChickenPhase.Pausing(_) => true
            case _ => false) && hitChicken(e.position, ck)
        }
        if idx >= 0 then
          ChickenAudio.cluck()
          val updated = model.chickens.updated(idx,
            model.chickens(idx).copy(phase = ChickenPhase.Laying(0.0, false)))
          Outcome(model.copy(chickens = updated))
        else Outcome(model)

    case FrameTick =>
      val dt   = context.delta.toDouble
      val dice = context.dice

      // Step each chicken; collect (newState, optionalNewEgg, celebrationEnded)
      val results: List[(ChickenState, Option[Egg], Boolean)] =
        model.chickens.map(ck => stepChicken(ck, dt, model.chickenEggs, model.viewport, dice))

      val steppedChickens = results.map(_._1)
      val newlyLaid       = results.flatMap(_._2)

      // Tick existing egg bounce timers and append newly laid eggs
      val allEggs = model.chickenEggs.map(e =>
        e.copy(bounceTimer = (e.bounceTimer + dt).min(Egg.BounceLen))) ++ newlyLaid

      // IDs of chickens whose celebration just ended this frame
      val celebEndedIds: Set[Int] = results.collect { case (ck, _, true) => ck.id }.toSet

      // Boost speed for chickens that just finished celebrating
      val afterCelebEnd = steppedChickens.map { ck =>
        if celebEndedIds(ck.id) then
          ck.copy(speed = (ck.speed + ChickenState.SpeedBoost).min(ChickenState.MaxSpeed))
        else ck
      }

      // Spawn one new chicken per ended celebration, up to the cap
      val maxSpeed = if afterCelebEnd.nonEmpty then afterCelebEnd.map(_.speed).max
                     else ChickenState.BaseSpeed
      val slotsAvailable = (ChickenState.MaxChickens - afterCelebEnd.length).max(0)
      val newChickens: List[ChickenState] = celebEndedIds.toList.take(slotsAvailable).zipWithIndex.map { (_, i) =>
        spawnChicken(model.nextChickenId + i, maxSpeed, model.viewport, dice)
      }

      // Per-chicken egg counts (eggs are never cleared)
      val eggCounts: Map[Int, Int] =
        allEggs.groupBy(_.chickenId).view.mapValues(_.length).toMap

      // Trigger celebration for any chicken that just hit its egg quota
      // (skip chickens already celebrating or laying, and those that just ended)
      val finalChickens = (afterCelebEnd ++ newChickens).map { ck =>
        val count = eggCounts.getOrElse(ck.id, 0)
        val canCelebrate = ck.phase match
          case ChickenPhase.Waddling | ChickenPhase.Pausing(_) => true
          case _ => false
        if count - ck.eggsAtLastCeleb >= ChickenState.FullEggCount && canCelebrate && !celebEndedIds(ck.id) then
          ChickenAudio.fanfare()
          ck.copy(phase = ChickenPhase.Celebrating(0.0), eggsAtLastCeleb = count)
        else ck
      }

      Outcome(model.copy(
        chickens         = finalChickens,
        chickenEggs      = allEggs,
        chickenEggsTotal = model.chickenEggsTotal + newlyLaid.length,
        nextChickenId    = model.nextChickenId + newChickens.length
      ))

    case _ => Outcome(model)

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def ru(dice: Dice): Double = (dice.roll(1000) - 1).toDouble / 999.0

  private def hitChicken(pt: Point, ck: ChickenState): Boolean =
    val d   = if ck.facingRight then 1.0 else -1.0
    val bdx = pt.x - ck.pos.x;          val bdy = pt.y - ck.pos.y
    val hdx = pt.x - (ck.pos.x + 10*d); val hdy = pt.y - (ck.pos.y - 35.0)
    (bdx*bdx + bdy*bdy) < 52.0*52.0 || (hdx*hdx + hdy*hdy) < 28.0*28.0

  private def pickVel(speed: Double, dice: Dice): Vector2 =
    val angle = (dice.roll(8) - 1).toDouble * (Math.PI / 4.0) +
                (ru(dice) - 0.5) * (Math.PI / 8.0)
    Vector2(Math.cos(angle) * speed, Math.sin(angle) * speed)

  private def spawnChicken(id: Int, speed: Double, vp: Size, dice: Dice): ChickenState =
    val bw = vp.width.toDouble; val bh = vp.height.toDouble
    val grassY = bh - bh * 0.12
    val x = 80.0 + ru(dice) * (bw - 160.0).max(1.0)
    val y = 120.0 + ru(dice) * (grassY - 220.0).max(1.0)
    val vel = pickVel(speed, dice)
    ChickenState.initial.copy(
      id          = id,
      pos         = Vector2(x, y),
      vel         = vel,
      facingRight = vel.x >= 0,
      speed       = speed,
      dirTimer    = 1.5 + ru(dice) * 2.0
    )

  // ── Movement ──────────────────────────────────────────────────────────────
  // Returns (newState, optionalNewEgg, celebrationJustEnded)

  private def stepChicken(
    ck: ChickenState, dt: Double, eggs: List[Egg], vp: Size, dice: Dice
  ): (ChickenState, Option[Egg], Boolean) =
    val bw = vp.width.toDouble; val bh = vp.height.toDouble
    val grassY = bh - bh * 0.12
    val minX = ChickenState.BodyRadius.toDouble
    val maxX = bw - ChickenState.BodyRadius
    val minY = 80.0
    val maxY = grassY - ChickenState.BodyRadius.toDouble - 14.0

    ck.phase match

      case ChickenPhase.Celebrating(t) =>
        val next = t + dt
        if next >= ChickenState.CelebDur then
          val vel = pickVel(ck.speed, dice)
          (ck.copy(vel = vel, facingRight = vel.x >= 0,
                   dirTimer = 2.0 + ru(dice) * 2.0,
                   phase = ChickenPhase.Waddling), None, true)
        else
          (ck.copy(phase = ChickenPhase.Celebrating(next)), None, false)

      case ChickenPhase.Laying(t, eggPlaced) =>
        val next = t + dt
        val (newEgg, placed) =
          if !eggPlaced && next >= 0.3 then
            ChickenAudio.plop()
            val egg = Egg(
              pos         = Vector2(ck.pos.x, ck.pos.y + ChickenState.BodyRadius + 10.0),
              rotation    = (dice.roll(21) - 11).toDouble * Math.PI / 180.0,
              bounceTimer = 0.0,
              chickenId   = ck.id
            )
            (Some(egg), true)
          else (None, eggPlaced)
        if next >= ChickenState.LayDur then
          val vel = pickVel(ck.speed, dice)
          (ck.copy(vel = vel, facingRight = vel.x >= 0,
                   dirTimer = 2.0 + ru(dice) * 2.0,
                   phase = ChickenPhase.Waddling), newEgg, false)
        else
          (ck.copy(phase = ChickenPhase.Laying(next, placed)), newEgg, false)

      case ChickenPhase.Pausing(t) =>
        val next = t + dt
        if next >= ChickenState.PauseDur then
          val vel = pickVel(ck.speed, dice)
          (ck.copy(vel = vel, facingRight = vel.x >= 0,
                   dirTimer = 2.0 + ru(dice) * 2.0,
                   phase = ChickenPhase.Waddling), None, false)
        else
          (ck.copy(phase = ChickenPhase.Pausing(next)), None, false)

      case ChickenPhase.Waddling =>
        val sv = steerAroundEggs(ck.pos, ck.vel, eggs)
        var x = ck.pos.x + sv.x * dt; var y = ck.pos.y + sv.y * dt
        var vx = sv.x;                var vy = sv.y

        if x < minX then { x = minX; vx =  vx.abs }
        if x > maxX then { x = maxX; vx = -vx.abs }
        if y < minY then { y = minY; vy =  vy.abs }
        if y > maxY then { y = maxY; vy = -vy.abs }

        val newFacing   = if Math.abs(vx) > 6.0 then vx > 0.0 else ck.facingRight
        val newDirTimer = ck.dirTimer - dt

        if newDirTimer <= 0.0 then
          (ck.copy(pos = Vector2(x, y), vel = Vector2(vx, vy),
                   facingRight = newFacing, waddle = ck.waddle + dt,
                   phase = ChickenPhase.Pausing(0.0)), None, false)
        else
          (ck.copy(pos = Vector2(x, y), vel = Vector2(vx, vy),
                   facingRight = newFacing, waddle = ck.waddle + dt,
                   dirTimer = newDirTimer), None, false)

  // ── Egg avoidance ─────────────────────────────────────────────────────────

  private val EggCollisionDist = (ChickenState.BodyRadius + 18).toDouble
  private val LookaheadDist    = 68.0

  private def steerAroundEggs(pos: Vector2, vel: Vector2, eggs: List[Egg]): Vector2 =
    if eggs.isEmpty then vel
    else
      def clearAt(angle: Double): Boolean =
        val nx = pos.x + Math.cos(angle) * LookaheadDist
        val ny = pos.y + Math.sin(angle) * LookaheadDist
        !eggs.exists { egg =>
          val dx = nx - egg.pos.x; val dy = ny - egg.pos.y
          dx*dx + dy*dy < EggCollisionDist * EggCollisionDist
        }
      val cur = Math.atan2(vel.y, vel.x)
      if clearAt(cur) then vel
      else
        val chosen = Seq(0.35, -0.35, 0.7, -0.7, 1.1, -1.1, 1.6, -1.6, Math.PI)
          .map(cur + _).find(clearAt).getOrElse(cur + Math.PI)
        val spd = Math.sqrt(vel.x * vel.x + vel.y * vel.y)
        Vector2(Math.cos(chosen) * spd, Math.sin(chosen) * spd)

  // ── Present ───────────────────────────────────────────────────────────────

  def present(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val bw     = model.viewport.width
    val bh     = model.viewport.height
    val grassH = (bh * 0.12).toInt
    val grassY = bh - grassH

    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw, bh), Fill.Color(RGBA(0.851, 0.922, 0.957, 1.0)))
    val grassNode: SceneNode =
      Shape.Box(Rectangle(0, grassY, bw, grassH), Fill.Color(RGBA(0.741, 0.871, 0.663, 1.0)))

    val visibleEggs = model.chickenEggs

    val eggNodes: List[SceneNode] = visibleEggs.flatMap { egg =>
      val yOff = eggBounceOffset(egg.bounceTimer)
      val ex = egg.pos.x.toInt; val ey = (egg.pos.y - yOff).toInt
      List(
        Shape.Circle(Point(ex, ey + 16), 11, Fill.Color(RGBA(0.0, 0.0, 0.0, 0.07))),
        Shape.Circle(Point(ex, ey + 5),  13, Fill.Color(RGBA(0.992, 0.976, 0.953, 1.0))),
        Shape.Circle(Point(ex, ey - 5),  10, Fill.Color(RGBA(0.992, 0.976, 0.953, 1.0))),
        Shape.Circle(Point(ex - 4, ey - 8), 4, Fill.Color(RGBA(1.0, 1.0, 1.0, 0.5)))
      )
    }

    // Draw each chicken with its own animation state
    val chickenNodes: List[SceneNode] = model.chickens.flatMap { ck =>
      val (squatY, beakOpen, wingFlare) = ck.phase match
        case ChickenPhase.Laying(t, _) =>
          val sq = if t < 0.3 then (t / 0.3 * 14).toInt
                   else if t < 0.8 then 14
                   else ((1.0 - (t - 0.8) / 0.2) * 14).toInt.max(0)
          val bk = if t >= 0.3 && t < 0.8
                   then (Math.abs(Math.sin((t - 0.3) * 14.0)) * 7.0).toInt else 0
          (sq, bk, (sq * 0.6).toInt)
        case ChickenPhase.Celebrating(t) =>
          val sq = if t >= 0.7 then (Math.abs(Math.sin(t * 8.0)) * 6.0).toInt else 0
          val wf = if t >= 0.7 then (Math.abs(Math.sin(t * 8.0)) * 30.0).toInt else 0
          (sq, 0, wf)
        case _ => (0, 0, 0)

      val waddleAmt = ck.phase match
        case ChickenPhase.Waddling => Math.sin(ck.waddle * ChickenState.WaddleFreq)
        case _                     => 0.0

      val headBobY = ck.phase match
        case ChickenPhase.Pausing(t)       => (Math.sin(t * 18.0) * 4.0).toInt
        case ChickenPhase.Laying(t, _)
          if t >= 1.0                      => (Math.sin((t - 1.0) * 12.0) * 5.0).toInt
        case ChickenPhase.Celebrating(t)   =>
          if t < 0.7 then (Math.sin(t * 5.0) * 7.0).toInt
          else             (Math.sin(t * 10.0) * 5.0).toInt
        case _ => 0

      drawChicken(ck.pos.x.toInt, ck.pos.y.toInt, ck.facingRight,
                  waddleAmt = waddleAmt, headBobY = headBobY,
                  squatY = squatY, beakOpen = beakOpen, wingFlare = wingFlare)
    }

    val backNode: SceneNode =
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.65))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))

    val totalNode: SceneNode =
      TextBox(s"🥚 ${model.chickenEggsTotal}")
        .withFontSize(Pixels(22))
        .withColor(RGBA(0.35, 0.25, 0.15, 0.70))
        .withSize(Size(120, 34))
        .moveTo(Point(bw - 124, 10))

    val allNodes = bgNode :: grassNode :: eggNodes ::: chickenNodes ::: List(backNode, totalNode)
    Outcome(SceneUpdateFragment(Layer(allNodes*)))

  private def eggBounceOffset(t: Double): Int =
    val p = (t / Egg.BounceLen).min(1.0)
    if p < 0.65 then (12.0 * (1.0 - p / 0.65)).toInt
    else              (3.0 * Math.sin((p - 0.65) / 0.35 * Math.PI)).toInt

  // ── Drawing ───────────────────────────────────────────────────────────────

  private val BodyColor  = RGBA(0.961, 0.769, 0.702, 1.0)
  private val BodyShade  = RGBA(0.878, 0.659, 0.580, 1.0)
  private val CombColor  = RGBA(0.878, 0.333, 0.333, 1.0)
  private val BeakColor  = RGBA(0.980, 0.780, 0.459, 1.0)
  private val EyeColor   = RGBA(0.239, 0.122, 0.051, 1.0)
  private val LegColor   = RGBA(0.780, 0.502, 0.251, 1.0)
  private val MouthColor = RGBA(0.180, 0.080, 0.020, 1.0)

  def drawChicken(
    cx: Int, cy: Int, facingRight: Boolean,
    scale:     Double = 1.0,
    waddleAmt: Double = 0.0,
    headBobY:  Int    = 0,
    squatY:    Int    = 0,
    beakOpen:  Int    = 0,
    wingFlare: Int    = 0
  ): List[SceneNode] =
    val d = if facingRight then 1 else -1
    def s(n: Int): Int   = (n * scale).toInt
    def px(dx: Int): Int = cx + s(dx) * d

    val bodyCx = cx + (waddleAmt * 3.0 * scale).toInt * d
    val bodyCy = cy - (Math.abs(waddleAmt) * 2.0 * scale).toInt + squatY

    val headCx = bodyCx + s(10) * d
    val headCy = bodyCy - s(35) + headBobY

    val swing    = (waddleAmt * s(5)).toInt
    val fwdEndX  = bodyCx + s(6)  * d - swing * d
    val backEndX = bodyCx - s(10) * d + swing * d

    val beakW     = s(13).max(2)
    val beakBaseY = headCy - s(5)
    val beakX     = if facingRight then headCx + s(18) else headCx - s(18) - beakW

    val sw        = (2.0 * scale).max(1.0).toInt
    val nearWingY = bodyCy + s(14) - (Math.abs(waddleAmt) * 2.0 * scale).toInt
    val farWingX  = bodyCx - (s(22) + wingFlare) * d
    val nearWingX = bodyCx + (s(24) + wingFlare) * d

    val beakNodes: List[SceneNode] =
      if beakOpen <= 0 then
        List(Shape.Box(Rectangle(beakX, beakBaseY, beakW, s(8).max(2)), Fill.Color(BeakColor)))
      else
        val halfH = s(4).max(1)
        List(
          Shape.Box(Rectangle(beakX, beakBaseY,                    beakW, halfH),     Fill.Color(BeakColor)),
          Shape.Box(Rectangle(beakX, beakBaseY + halfH,            beakW, beakOpen),  Fill.Color(MouthColor)),
          Shape.Box(Rectangle(beakX, beakBaseY + halfH + beakOpen, beakW, halfH),     Fill.Color(BeakColor))
        )

    List(
      Shape.Circle(Point(farWingX,  bodyCy + s(10)), s(24).max(1), Fill.Color(BodyShade)),
      Shape.Circle(Point(nearWingX, nearWingY),       s(22).max(1), Fill.Color(BodyShade)),
      Shape.Circle(Point(bodyCx, bodyCy), s(44).max(1), Fill.Color(BodyColor), Stroke(sw, BodyShade)),
      Shape.Circle(Point(bodyCx + s(6) * d, bodyCy + s(10)), s(26).max(1),
                   Fill.Color(RGBA(0.996, 0.965, 0.933, 1.0))),
      Shape.Circle(Point(bodyCx + s(4)  * d, bodyCy - s(2)),  s(9).max(1),
                   Fill.Color(RGBA(0.988, 0.941, 0.898, 1.0))),
      Shape.Circle(Point(bodyCx + s(14) * d, bodyCy + s(10)), s(8).max(1),
                   Fill.Color(RGBA(0.988, 0.941, 0.898, 1.0))),
      Shape.Circle(Point(bodyCx + s(3)  * d, bodyCy + s(20)), s(8).max(1),
                   Fill.Color(RGBA(0.988, 0.941, 0.898, 1.0))),
      Shape.Circle(Point(headCx, headCy), s(22).max(1), Fill.Color(BodyColor), Stroke(sw, BodyShade)),
      Shape.Circle(Point(headCx + s(2)  * d, headCy - s(20)), s(6).max(1), Fill.Color(CombColor)),
      Shape.Circle(Point(headCx + s(8)  * d, headCy - s(24)), s(8).max(1), Fill.Color(CombColor)),
      Shape.Circle(Point(headCx + s(14) * d, headCy - s(19)), s(6).max(1), Fill.Color(CombColor)),
      Shape.Circle(Point(headCx + s(20) * d, headCy + s(8)),  s(6).max(1), Fill.Color(CombColor)),
      Shape.Circle(Point(headCx + s(13) * d, headCy - s(4)),  s(4).max(1), Fill.Color(EyeColor)),
      Shape.Circle(Point(headCx + s(14) * d, headCy - s(6)),  s(1).max(1), Fill.Color(RGBA(1,1,1,0.75))),
      Shape.Line(Point(bodyCx + s(6)  * d, bodyCy + s(43)), Point(fwdEndX,  bodyCy + s(58)), Stroke(s(4).max(1), LegColor)),
      Shape.Line(Point(bodyCx - s(10) * d, bodyCy + s(43)), Point(backEndX, bodyCy + s(58)), Stroke(s(4).max(1), LegColor)),
      Shape.Line(Point(fwdEndX  - s(5), bodyCy + s(58)), Point(fwdEndX  + s(9), bodyCy + s(58)), Stroke(s(3).max(1), LegColor)),
      Shape.Line(Point(backEndX - s(9), bodyCy + s(58)), Point(backEndX + s(5), bodyCy + s(58)), Stroke(s(3).max(1), LegColor)),
    ) ::: beakNodes