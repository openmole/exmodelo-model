package zombies

import space._

import scala.util.Random

import scala.scalajs.js.annotation._

@JSExportTopLevel("world")
object world:

  sealed trait Cell

  case object Wall extends Cell

  case class Floor(
    wallSlope: Vector[Slope] = Vector(),
    rescueSlope: Vector[Slope] = Vector(),
    rescueZone: Boolean = false,
    trap: Option[Trap] = None,
    information: Double = 0.0,
    pheromone: Double = 0.0,
    entrance: Entrance = NoEntrance) extends Cell

  sealed trait Trap
  case object CaptureTrap extends Trap
  case object DeathTrap extends Trap

  case class Slope(x: Double = 0.0, y: Double = 0.0, intensity: Double = 0)

  object Floor {
    def trapZone(f: Floor) = f.trap.isDefined
  }


  sealed trait Entrance
  case object NoEntrance extends Entrance
  case object HumanEntrance extends Entrance

  object World {
    def floor: PartialFunction[Cell, Floor] =
      case floor: Floor => floor

    def get(world: World, l: Location): Option[Cell] = {
      val (x, y) = l
      get(world, x, y)
    }

    def get(world: World, x: Int, y: Int): Option[Cell] =
      if(outsideOfTheWorld(world, (x, y))) None
      else Some(world.cells(x)(y))

    def outsideOfTheWorld(world: World, l: Location) = l._1 < 0 || l._1 >= world.side || l._2 < 0 || l._2 >= world.side

    def parse(altitudeLambdaDecay: Double = 1.0, slopeIntensity: Double = 0.1)(worldDescription: String) = {

      def parse(s: String) = {
        def toWall(c: Char): Option[Cell] = c match {
          case '0' => Some(Floor())
          case 'r' => Some(Floor(information = 1.0))
          case '+' => Some(Wall)
          case 'R' => Some(Floor(rescueZone = true))
          case 'E' => Some(Floor(rescueZone = true, information = 1.0))
          case 'e' => Some(Floor(entrance = HumanEntrance))
          case 'T' => Some(Floor(trap = Some(CaptureTrap)))
          case 'D' => Some(Floor(trap = Some(DeathTrap)))
          case _ => None
        }

        val cells = s.split("\n").map(l => l.flatMap(toWall).toArray).filter(!_.isEmpty)

        val xMax = cells.size
        val yMax = cells.map(_.size).max

        assert(cells.forall(_.size == yMax), s"All lines should have the same length: ${cells.map(_.size).mkString(" ")}")
        assert(xMax == yMax, s"World should be a square, wrong dimensions: $xMax x $yMax")

        World(cells, xMax)
      }
      val world = parse(worldDescription)
      World.computeRescueSlope(World.computeWallSlope(world, altitudeLambdaDecay, slopeIntensity))
    }

    def coordinates(world:World):Vector[(Location,Cell)] = {
      val lc = for {
        x <- 0 until world.side
        y <- 0 until world.side
      } yield (x,y) -> world.cells(x)(y)

      lc.toVector
    }

    def floorsCoordinate(world:World,includeRescueZone:Boolean = false):Seq[(Int,Int)] = {
      val floors = coordinates(world).collect{ case (loc, cell:Floor) => loc -> cell }
      val filteredFloors = floors.filter{ _._2.rescueZone == includeRescueZone}
      filteredFloors.map{ case(loc,cell) => loc}
    }

    def cellCenter(world: World, location: Location) = {
      val (x, y) = location
      val cellSize = 1.0 / world.side
      (x.toDouble * cellSize + cellSize / 2, y.toDouble * cellSize + cellSize / 2)
    }

    def locationIsInTheWorld(world: World, x: Int, y: Int) =
      x >= 0 && y >= 0 && x < world.side && y < world.side

    def neighbors(w: World, x: Int, y: Int, neighborhoodSize: Int) =
      space.neighbors(get(w, _, _), x, y, neighborhoodSize)

    def computeDistance(world: World, isOrigin: (Int, Int) => Boolean) = {
      val distances = Array.tabulate(world.side, world.side) { (x, y) => if(isOrigin(x, y)) -1.0 else Double.PositiveInfinity }

      def pass(): Unit = {
        var finished = true

        for {
          x <- 0 until world.side
          y <- 0 until world.side
          previousDistance = distances(x)(y)
          if !isOrigin(x, y)
          if !isWall(world, x, y)
          newDistance = space.neighbors(space.get(distances, _, _), x, y, 1, center = false).min + 1.0
          if previousDistance != newDistance
        } {
          distances(x)(y) = newDistance
          finished = false
        }

        if !finished then pass()
      }

      pass()
      distances
    }


    def slope(world: World, x: Int, y: Int, levels: Array[Array[Double]], intensity: (Double, Double) => Double) = {
      val level = levels(x)(y)
      val slopes =
        for {
          ox <- -1 to 1
          oy <- -1 to 1
          if ox != 0 || oy != 0
          if locationIsInTheWorld(world, x + ox, y + oy)
          fLevel = levels(x + ox)(y + oy)
          if fLevel < level
        } yield Slope(ox, oy, intensity(level, fLevel))
      slopes.toVector
    }


    def computeRescueSlope(world: World) = {
      val levels = computeDistance(world, isRescueCell(world, _, _))
      val cells = copyCells(world.cells)

      for {
        x <- 0 until world.side
        y <- 0 until world.side
        c <- floor.lift(cells(x)(y))
        if !levels(x)(y).isInfinite
      } cells(x)(y) = c.copy(rescueSlope = slope(world, x, y, levels, (_, _) => 1.0))

      world.copy(cells = cells)
    }

    def computeWallSlope(world: World, lambda: Double, intensity: Double) = {
      def computeLevel(world: World, lambda: Double) = {
        val distances = computeDistance(world, isWall(world, _, _))

        def toExponential(cells: Array[Array[Cell]]) =
          cells.zipWithIndex.map { case (l, x) =>
            l.zipWithIndex.map { case(c, y) =>
              c match {
                case f: Floor => math.exp(-lambda * distances(x)(y))
                case x => Double.PositiveInfinity
              }
            }
          }

        toExponential(world.cells)
      }

      val levels = computeLevel(world, lambda)
      val cells = copyCells(world.cells)

      def computeIntensity(levelFrom: Double, levelTo: Double) = (levelFrom - levelTo) * intensity

      for {
        x <- 0 until world.side
        y <- 0 until world.side
        c <- floor.lift(cells(x)(y))
      } cells(x)(y) = c.copy(wallSlope = slope(world, x, y, levels, computeIntensity))

      world.copy(cells = cells)
    }

    def copyCells(cells: Array[Array[Cell]]) = {
      Array.tabulate(cells.size, cells.size)((x, y) => cells(x)(y))
    }

    def isWall(world: World, x: Int, y: Int, outsideWall: Boolean = false) = get(world, x, y) match {
      case Some(Wall) => true
      case None if outsideWall => true
      case _ => false
    }

    def isRescueCell(world: World, x: Int, y: Int) = get(world, x, y) match {
      case Some(f: Floor) => f.rescueZone
      case _ => false
    }

    def pheromone(world: World, location: Location): Double = {
      val (x, y) = location
      World.get(world, x, y) match
        case Some(f: Floor) => f.pheromone
        case _=> 0.0
    }

    def minCellSide(world: World) = 1.0 / world.side
    def cellDiagonal(world: World) = space.cellDiagonal(world.side)



    def randomPosition(world: World, rng: Random): Position =
      val v = randomUnitVector(rng)
      val p = positionToLocation(v, world.side)
      if(World.isWall(world, p._1, p._2)) randomPosition(world, rng) else v

    def jaude = parse() {
      """+++++++00000+++++++++++0000+++++++++++++
        |+++++++00000+++++++++++0000+++++++++++++
        |+++++++00000+++++++++++0000+++++++++++++
        |+++++++00000+++++++++++0000+++++++++++++
        |+++++++00000+++++++++++0000+++++++++++++
        |++++0000000000000++++++0000+++++++++++++
        |++++0000000000000++++++00000000000000000
        |++++0000000000000++++++00000000000000000
        |++++0000000000000++++++00000000000000000
        |++++0000000000000++++++0000+++++++++++++
        |++++0000000000000++++++0000+++++++++++++
        |++++0000000000000++++++0000+++++++++++++
        |++++0000000000000++++++0000+++++++++++++
        |++++++++++++00000++++++0000+++++++++++++
        |++++++++++++000000000000000+++++++++++++
        |++++++++++++00000000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |++++++++++++++++0000000000++++++++++++++
        |0000000000000000000000000000000000000000
        |0000000000000000000++++00000000000000000
        |0000000000000000000++++00000000000000000
        |0000000000000000000000000000000000000000
        |++++++++++++++++00000000000+++++++++++++
        |++++++++++++++++00000000000+++++++++++++
        |++++++++++++++++00000000000+++++++++++++
        |++++++++++++++++++++++00000+++++++++++++
        |++++++++++++++++++++++00000+++++++++++++
        |+++++R000000000000++++00000+++++++++++++
        |+++++0000000000000++++00000+++++++++++++
        |+++++0000000000000++++00000+++++++++++++
        |+++++0000000000000000000000+++++++++++++
        |+++++0000000000000000000000+++++++++++++
        |+++++0000000000000000000000+++++++++++++
        |+++++0000000000000000000000+++++++++++++
        |+++++0000000000000++++00000+++++++++++++
        |++++++++++++++++++++++00000+++++++++++++
        |""".stripMargin
    }

    def square(side: Int) = parse() {
      s"""${"+" * side}\n""" +
        s"""+${"0" * (side - 2)}+\n""" * (side - 2) +
        s"""${"+" * side}\n"""
    }


    def place(side: Int, halfDoorSize: Int) = parse() {
      val doorSize = halfDoorSize * 2
      assert(side > doorSize)

      val wallSize = (side - doorSize) / 2

      s"""${"+" * wallSize}${"0" * doorSize}${"+" * wallSize}\n""" +
        s"""+${"0" * (side - 2)}+\n""" * (wallSize - 1) +
        s"""${"0" * side}\n""" * doorSize +
        s"""+${"0" * (side - 2)}+\n""" * (wallSize - 1) +
        s"""${"+" * wallSize}${"0" * doorSize}${"+" * wallSize}\n"""
    }

    def stadium(wallSize: Int, fieldSide: Int, doorSize: Int) = parse() {

      def totalBleacherSide(wallSide: Int, fieldSide: Int) = (wallSize * 2 + doorSize - 2 - fieldSide)
      val adjustedFieldSize = if(totalBleacherSide(wallSize, fieldSide) % 2 == 0) fieldSide else fieldSide + 1

      val side = wallSize * 2 + doorSize
      val bleacherSize = (side - 2 - fieldSide) / 2

      s"""${"+" * wallSize}${"E" * doorSize}${"+" * wallSize}\n""" +
        s"""+${"0" * (side - 2)}+\n""" * bleacherSize +
        s"""+${"0" * bleacherSize}${"+" * adjustedFieldSize}${"0" * bleacherSize}+\n""" * (wallSize - bleacherSize - 1) +
        s"""E${"0" * bleacherSize}${"+" * adjustedFieldSize}${"0" * bleacherSize}E\n""" * doorSize +
        s"""+${"0" * bleacherSize}${"+" * adjustedFieldSize}${"0" * bleacherSize}+\n""" * (wallSize - bleacherSize - 1) +
        s"""+${"0" * (side - 2)}+\n""" * bleacherSize +
        s"""${"+" * wallSize}${"E" * doorSize}${"+" * wallSize}\n"""
    }



    def quarantineStadium(wallSize: Int, fieldSide: Int) = parse() {

      def totalBleacherSide(wallSide: Int, fieldSide: Int) = (wallSize * 2 - 2 - fieldSide)

      val adjustedFieldSize = if (totalBleacherSide(wallSize, fieldSide) % 2 == 0) fieldSide else fieldSide + 1

      val side = wallSize * 2
      val bleacherSize = (side - 2 - fieldSide) / 2

      s"""${"+" * side}\n""" +
        s"""+${"0" * (wallSize - 1)}${"R"}${"0" * (wallSize - 2)}+\n""" +
        s"""+${"0" * (side - 2)}+\n""" * (bleacherSize - 1) +
        s"""+${"0" * bleacherSize}${"+" * adjustedFieldSize}${"0" * bleacherSize}+\n""" * (wallSize - bleacherSize - 1) +
        s"""+${"0" * bleacherSize}${"+" * adjustedFieldSize}${"0" * bleacherSize}+\n""" * (wallSize - bleacherSize - 1) +
        s"""+${"0" * (side - 2)}+\n""" * (bleacherSize - 1) +
        s"""+${"0" * (wallSize - 1)}${"R"}${"0" * (wallSize - 2)}+\n""" +
        s"""${"+" * side}\n"""
    }


    /* ------------- Traps ------------- */
    def setTraps(world: World, trapLocation: Seq[(Location, Trap)]): World =
      val cells = copyCells(world.cells)

      for {
        ((x, y), t) <- trapLocation
      } {
        cells(x)(y) match {
          case f: Floor => cells(x)(y) = Floor(f.wallSlope, f.rescueSlope, f.rescueZone, Some(t), f.information, f.pheromone)
          case _ =>
        }
      }

      world.copy(cells = cells)



    def setTrap(world: World, trapLocation: Location*): World = setTraps(world, trapLocation.map(_ -> CaptureTrap))
    def withTrap(trapLocation: Location*)(world: World) = setTrap(world, trapLocation: _*)
  }

  case class World(cells: Array[Array[Cell]], side: Int)

  object WorldCache:
    def compute(world: World, range: Double): WorldCache =
      val neighborhoodSize = math.ceil(range / space.cellSide(world.side)).toInt

      def visible(location: Location) = shadow.visible(location, World.isWall(world, _, _), (world.side, world.side), neighborhoodSize)

      def entrances =
        for
          x <- 0 until world.side
          y <- 0 until world.side
          c = world.cells(x)(y)
        yield
          c match
            case c: Floor if c.entrance != NoEntrance => Some(c.entrance -> (x, y))
            case _ => None

      WorldCache(
        cache = Array.tabulate(world.side, world.side): (x, y) =>
          if World.isWall(world, x, y)
          then Array.empty
          else visible(x, y).toArray,
        entrances = entrances.flatten.toArray
      )

  case class WorldCache(
    cache: Array[Array[Array[(Int, Int)]]],
    entrances: Array[(Entrance, (Int, Int))])


