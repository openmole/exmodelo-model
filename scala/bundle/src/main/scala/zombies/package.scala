import zombies.observable.defaultGroupSize
import zombies.simulation.SimulationResult

package object zombies {
  implicit class ResultDecorator(results: SimulationResult) {
    def humansDynamic(by: Int = defaultGroupSize) = observable.humansDynamic(results)
    def walkingHumansDynamic(by: Int = defaultGroupSize) = observable.walkingHumansDynamic(results, by)
    def runningHumansDynamic(by: Int = defaultGroupSize) = observable.runningHumansDynamic(results, by)
    def zombiesDynamic(by: Int = defaultGroupSize) = observable.zombiesDynamic(results)
    def walkingZombiesDynamic(by: Int = defaultGroupSize) = observable.walkingZombiesDynamic(results, by)
    def runningZombiesDynamic(by: Int = defaultGroupSize) = observable.runningZombiesDynamic(results, by)
    def rescuedDynamic(by: Int = defaultGroupSize) = observable.rescuedDynamic(results)
    def killedDynamic(by: Int = defaultGroupSize) = observable.killedDynamic(results)
    def zombifiedDynamic(by: Int = defaultGroupSize) = observable.zombifiedDynamic(results, by = by)
    def fleeDynamic(by: Int = defaultGroupSize) = observable.fleeDynamic(results, by = by)
    def pursueDynamic(by: Int = defaultGroupSize) = observable.pursueDynamic(results, by = by)
    def humansGoneDynamic(by: Int = defaultGroupSize) = observable.humansGoneDynamic(results, by = by)
    def zombiesGoneDynamic(by: Int = defaultGroupSize) = observable.zombiesGoneDynamic(results, by = by)

    def totalRescued = observable.totalRescued(results)
    def halfRescued = observable.halfRescued(results)
  }
}
