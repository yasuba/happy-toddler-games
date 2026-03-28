package toddler.scenes

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

// ── Background music ──────────────────────────────────────────────────────────
//
// Two HTMLAudioElements alternating for crossfaded track cycling.
// Volume is controlled directly on the elements — no Web Audio graph needed,
// which avoids AudioContext suspended-state issues with streaming audio.
//
// Public API:
//   Music.start()             — call once from Main.setup()
//   Music.duck(seconds)       — duck during sfx / voice, then fade back up
//   Music.setEnabled(boolean) — toggle from settings
//   Music.setVolume(0.0–1.0)  — master volume from settings

object Music:

  // ── Constants ─────────────────────────────────────────────────────────────

  private val Tracks: Array[String] = Array(
    "assets/audio/music/avanti-time.mp3",
    "assets/audio/music/aventure-a-beautiful-garden.mp3",
    "assets/audio/music/hazelwood-coming-of-age.mp3",
    "assets/audio/music/massobeats-daydream.mp3",
    "assets/audio/music/massobeats-gingersweet.mp3",
    "assets/audio/music/milkywayvers-love-in-japan.mp3"
  )
  private val N            = Tracks.length
  private val BaseGain     = 0.35
  private val DuckedMult   = 0.30
  private val CrossfadeSec = 2.0   // crossfade overlap between tracks
  private val FadeStepMs   = 40    // setInterval tick for volume fades

  // ── State ─────────────────────────────────────────────────────────────────

  private var enabled:     Boolean    = true
  private var masterVol:   Double     = 1.0
  private var isStarted:   Boolean    = false
  private var currentIdx:  Int        = 0
  private var useElA:      Boolean    = true   // which element is "current"
  private var unduckTimer: js.Dynamic = null.asInstanceOf[js.Dynamic]
  private var xfadeTimer:  js.Dynamic = null.asInstanceOf[js.Dynamic]
  private var fadeHandle:  js.Dynamic = null.asInstanceOf[js.Dynamic]

  private def targetVol: Double = if enabled then BaseGain * masterVol else 0.0

  // ── Two audio elements (lazy, created on first tap) ────────────────────────

  private lazy val elA: js.Dynamic = makeEl()
  private lazy val elB: js.Dynamic = makeEl()

  private def currentEl: js.Dynamic = if useElA then elA else elB
  private def nextEl:    js.Dynamic = if useElA then elB else elA

  private def makeEl(): js.Dynamic =
    val el = js.Dynamic.newInstance(g.Audio)()
    el.updateDynamic("preload")("auto")
    el

  // ── Public API ─────────────────────────────────────────────────────────────

  /** Call once from Main.setup(). Playback starts on the first user tap. */
  def start(): Unit =
    if !isStarted then
      isStarted = true
      loadPrefs()
      currentIdx = (g.Math.random().asInstanceOf[Double] * N).toInt
      val onPtr: js.Function1[js.Any, Unit] = (_: js.Any) =>
        if !isGoing then
          isGoing = true
          playTrack(currentIdx, currentEl, fadeIn = true, fadeMs = 600)
      g.document.addEventListener("pointerdown", onPtr, true)

  /** Call whenever the user navigates to a new scene. Skips to the next track. */
  def onSceneChange(): Unit =
    if isGoing then
      g.clearTimeout(xfadeTimer)
      crossfadeTo((currentIdx + 1) % N)

  /** Duck the music for `seconds`, then fade back up. */
  def duck(seconds: Double): Unit =
    if !enabled then return
    g.clearTimeout(unduckTimer)
    setElVolume(currentEl, targetVol * DuckedMult)
    val cb: js.Function0[Unit] = () => fadeTo(currentEl, targetVol, (UnduckSec * 1000).toInt)
    unduckTimer = g.setTimeout(cb, (seconds * 1000).toInt)

  /** Toggle music on/off. Persists to localStorage. */
  def setEnabled(on: Boolean): Unit =
    enabled = on
    savePrefs()
    fadeTo(currentEl, targetVol, 800)

  /** Adjust master volume (0.0–1.0). */
  def setVolume(v: Double): Unit =
    masterVol = v.max(0.0).min(1.0)
    setElVolume(currentEl, targetVol)

  // ── Private ────────────────────────────────────────────────────────────────

  private var isGoing: Boolean = false
  private val UnduckSec = 1.0

  private def playTrack(idx: Int, el: js.Dynamic, fadeIn: Boolean, fadeMs: Int = (CrossfadeSec * 1000).toInt): Unit =
    el.updateDynamic("src")(Tracks(idx))
    el.updateDynamic("currentTime")(0.0)
    setElVolume(el, if fadeIn then 0.0 else targetVol)
    el.play()
    if fadeIn then fadeTo(el, targetVol, fadeMs)

    // Schedule crossfade to next track
    val onMeta: js.Function1[js.Any, Unit] = (_: js.Any) =>
      val dur = el.duration.asInstanceOf[Double]
      if !dur.isNaN && dur > CrossfadeSec then
        val ms = ((dur - CrossfadeSec) * 1000).toInt.max(500)
        xfadeTimer = g.setTimeout(
          (() => crossfadeTo((idx + 1) % N)): js.Function0[Unit],
          ms
        )
    el.updateDynamic("onloadedmetadata")(onMeta)

    // Fallback: if track ends naturally before crossfade timer fires
    val onEnded: js.Function1[js.Any, Unit] = (_: js.Any) =>
      g.clearTimeout(xfadeTimer)
      crossfadeTo((idx + 1) % N)
    el.updateDynamic("onended")(onEnded)

  private def crossfadeTo(nextIdx: Int): Unit =
    currentIdx = nextIdx
    val from = currentEl
    val to   = nextEl
    useElA   = !useElA  // swap current/next

    // Fade out the departing track
    fadeTo(from, 0.0, (CrossfadeSec * 1000).toInt)

    // Start and fade in the arriving track
    playTrack(nextIdx, to, fadeIn = true)

    // Silence and reset the departing element once its fade completes
    g.setTimeout(
      (() => {
        from.pause()
        setElVolume(from, 0.0)
      }): js.Function0[Unit],
      (CrossfadeSec * 1000 + 200).toInt
    )

  // Immediate volume set (clamped to valid range)
  private def setElVolume(el: js.Dynamic, v: Double): Unit =
    el.volume = v.max(0.0).min(1.0)

  // Smooth fade using setInterval
  private def fadeTo(el: js.Dynamic, target: Double, durationMs: Int): Unit =
    g.clearInterval(fadeHandle)
    val start  = el.volume.asInstanceOf[Double]
    val steps  = (durationMs / FadeStepMs).max(1)
    var step   = 0
    fadeHandle = g.setInterval(
      (() => {
        step += 1
        val t = step.toDouble / steps
        setElVolume(el, start + (target - start) * t)
        if step >= steps then
          setElVolume(el, target)
          g.clearInterval(fadeHandle)
      }): js.Function0[Unit],
      FadeStepMs
    )

  private def loadPrefs(): Unit =
    try
      val v = g.localStorage.getItem("bgMusicEnabled")
      if v != null then enabled = v.asInstanceOf[String] != "false"
    catch case _: Throwable => ()

  private def savePrefs(): Unit =
    try g.localStorage.setItem("bgMusicEnabled", enabled.toString)
    catch case _: Throwable => ()