package zombies

import zombies.guitutils.parameters._
import zombies.world.World

import scala.util.Random
import zombies.guitutils.controls.{Controller, Mecanism}
import zombies.worldgen.GridGeneratorLauncher
import zombies.worldgen._
import scala.scalajs.js.annotation.JSExportTopLevel

object spatialsens {

  def world(controlList: Seq[Controller], rng: Random): World = {
    val controlValues = controlList.map { p => p.name -> p.value }.toMap

    def value[T](p: Range[T]) = controlValues.getOrElse(p.name, defaultOrOff(p)).asInstanceOf[T]
    def optionValue(o: Options) = controlValues.getOrElse(o.name, defaultOrOff(o)).toString
    def booleanValue[T](o: OnOff[T]) = controlValues.getOrElse(o.name, false).asInstanceOf[Boolean]

    val worldsize = 40

    closeWorld(World(
      GridGeneratorLauncher(
        optionValue(generationMethod),
        worldsize,
        value(randomDensity),
        value(expMixtureCenters),
        value(expMixtureRadius),
        value(expMixtureThreshold),
        value(blocksNumber),
        value(blocksMinSize),
        value(blocksMaxSize),
        value(percolationProba),
        value(percolationBordPoints),
        value(percolationLinkWidth)
      ).getGrid(rng),
      worldsize
    ))
  }


  @JSExportTopLevel("zombies")
  def zombies(): Unit = {
    simulate.buildGUI(
      world(_, _),
      generationMethod,
      randomDensity,
      expMixtureCenters,
      expMixtureRadius,
      expMixtureThreshold,
      blocksNumber,
      blocksMinSize,
      blocksMaxSize,
      percolationProba,
      percolationBordPoints,
      percolationLinkWidth,
      humanPerception isDefault,
      zombiePerception isDefault,
      humanRunSpeed asDefaultFrom (walkSpeed),
      zombieRunSpeed asDefaultFrom (walkSpeed),
      infectionRange.isDefault,
      humanExhaustionProbability isDefault,
      humanFightBackProbability isDefault,
      humanFollowProbability isDefault,
      humanInformProbability isDefault,
      numberHumans isDefault,
      numberZombies isDefault,
      humanMaxRotation isOff,
      humanFollowProbability isOff,
      humanInformProbability isOff,
      humanInformedRatio isOff,
      zombieMaxRotation isOff
    )
  }
}