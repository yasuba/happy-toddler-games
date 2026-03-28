package toddler

import indigo.*
import indigo.scenes.SceneName
import indigo.scenes.Scene
import toddler.scenes.HomeScene
import toddler.scenes.SettingsScene
import toddler.scenes.FloatingCircleScene
import toddler.scenes.LettersScene
import toddler.scenes.ChickenScene
import toddler.scenes.Model
import toddler.scenes.Music

object Main extends IndigoGame[Unit, Unit, Model, Unit] {

  def main(args: Array[String]): Unit =
    launch("indigo-container")

  override def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, Unit]] =
    NonEmptyList(HomeScene, LettersScene, FloatingCircleScene, SettingsScene, ChickenScene)

  override def initialScene(bootData: Unit): Option[SceneName] = Some(HomeScene.name)

  override def eventFilters: EventFilters = EventFilters.Permissive


  override def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Music.start()
    Outcome(Startup.Success(()))

  val assets: Set[AssetType] =
    Set()

  val fonts: Set[FontInfo] =
    Set()

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(BootResult.configOnly(
      GameConfig.default
        .withClearColor(RGBA(0.980, 0.957, 0.925, 1.0))
    ))

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(Model.initial)

  def initialViewModel(
      startupData: Unit,
      model: Model
  ): Outcome[Unit] =
    Outcome(())

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] = _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] = _ => Outcome(viewModel)

  def present(
      context: FrameContext[Unit],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
}
