import zombies.agent.{Agent, PheromoneMechanism}
import zombies.metrics.defaultGroupSize
import zombies.simulation.{ArmyOption, NoArmy, NoRedCross, RedCrossOption, Simulation, SimulationResult}
import zombies.world.World

package object zombies extends DSL {

  implicit class ResultDecorator(results: SimulationResult) {
    def humansDynamic(by: Int = defaultGroupSize) = metrics.humansDynamic(results, by)
    def walkingHumansDynamic(by: Int = defaultGroupSize) = metrics.walkingHumansDynamic(results, by)
    def runningHumansDynamic(by: Int = defaultGroupSize) = metrics.runningHumansDynamic(results, by)

    def zombiesDynamic(by: Int = defaultGroupSize) = metrics.zombiesDynamic(results, by)
    def walkingZombiesDynamic(by: Int = defaultGroupSize) = metrics.walkingZombiesDynamic(results, by)
    def runningZombiesDynamic(by: Int = defaultGroupSize) = metrics.runningZombiesDynamic(results, by)
    def rescuedDynamic(by: Int = defaultGroupSize) = metrics.rescuedDynamic(results, by)
    def killedDynamic(by: Int = defaultGroupSize) = metrics.killedDynamic(results, by)
    def zombifiedDynamic(by: Int = defaultGroupSize) = metrics.zombifiedDynamic(results, by)
    def fleeDynamic(by: Int = defaultGroupSize) = metrics.fleeDynamic(results, by)
    def pursueDynamic(by: Int = defaultGroupSize) = metrics.pursueDynamic(results, by)
    def humansGoneDynamic(by: Int = defaultGroupSize) = metrics.humansGoneDynamic(results, by)
    def zombiesGoneDynamic(by: Int = defaultGroupSize) = metrics.zombiesGoneDynamic(results, by)

    def totalRescued = metrics.totalRescued(results)
    def halfTimeRescued = metrics.halfTimeRescued(results)
    def peakTimeRescued(window: Int = defaultGroupSize) = metrics.peakTimeRescued(results, window)
    def peakSizeRescued(window: Int = defaultGroupSize) = metrics.peakSizeRescued(results, window)

    def totalZombified = metrics.totalZombified(results)
    def halfZombified= metrics.halfTimeZombified(results)
    def peakTimeZombified(window: Int = defaultGroupSize) = metrics.peakTimeZombified(results, window)
    def peakSizeZombified(window: Int = defaultGroupSize) = metrics.peakSizeZombified(results, window)

    // spatial observables
    def spatialMoranZombified: Double = metrics.spatialMoran(metrics.zombified)(results)
    def spatialDistanceMeanZombified: Double = metrics.spatialDistanceMean(metrics.zombified)(results)
    def spatialEntropyZombified: Double = metrics.spatialEntropy(metrics.zombified)(results)
    def spatialSlopeZombified: Double = metrics.spatialSlope(metrics.zombified)(results)
    def spatialRipleyZombified: Double = metrics.spatialRipley(metrics.zombified)(results)
  }

}
