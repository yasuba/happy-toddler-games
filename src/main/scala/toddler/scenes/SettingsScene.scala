package toddler.scenes

import indigo.*
import indigo.scenes.*
import toddler.models.Model

// Stub — wired for navigation only. Settings content to be built later.

object SettingsScene extends Scene[Unit, Model, Unit]:
  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName               = SceneName("settings")
  val modelLens: Lens[Model, Model] = Lens.keepLatest[Model]
  val viewModelLens: Lens[Unit, Unit] = Lens.unit
  val eventFilters: EventFilters    = EventFilters.Permissive
  val subSystems: Set[SubSystem]    = Set.empty

  def updateViewModel(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  def updateModel(
      context: SceneContext[Unit], model: Model
  ): GlobalEvent => Outcome[Model] =

    case ViewportResize(vp) =>
      Outcome(model.copy(viewport = vp.toSize))

    case e: PointerEvent.PointerDown =>
      // Back button (top-left 52×52 px)
      if e.position.x < 52 && e.position.y < 52 then
        Music.onSceneChange()
        Outcome(model).addGlobalEvents(SceneEvent.JumpTo(HomeScene.name))
      else Outcome(model)

    case _ => Outcome(model)

  def present(
      context: SceneContext[Unit], model: Model, viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val bw = model.viewport.width
    val bh = model.viewport.height
    val nodes: List[SceneNode] = List(
      TextBox("settings")
        .bold
        .withFontSize(Pixels(32))
        .withColor(RGBA(0.35, 0.35, 0.35, 0.88))
        .alignCenter
        .withSize(Size(bw, 50))
        .moveTo(Point(0, bh / 2 - 25)),
      TextBox("coming soon")
        .withFontSize(Pixels(15))
        .withColor(RGBA(0.55, 0.55, 0.55, 0.55))
        .alignCenter
        .withSize(Size(bw, 26))
        .moveTo(Point(0, bh / 2 + 32)),
      TextBox("←")
        .withFontSize(Pixels(26))
        .withColor(RGBA(0.5, 0.5, 0.5, 0.65))
        .withSize(Size(40, 36))
        .moveTo(Point(8, 10))
    )
    Outcome(SceneUpdateFragment(Layer(nodes*)))


