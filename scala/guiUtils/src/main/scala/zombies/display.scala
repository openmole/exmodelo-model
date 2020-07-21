package zombies

import org.scalajs.dom.raw.SVGElement
import scalatags.JsDom.all._
import scalatags.JsDom.styles
import scalatags.JsDom.svgAttrs.{height, style, width, x, y}
import scalatags.JsDom.svgTags
import scalatags.JsDom.svgAttrs
import zombies.agent._

import scala.scalajs.js.annotation._
import scala.util.Random
import scaladget.bootstrapnative.bsn._
import zombies.simulation.{Event, RedCross, Simulation}
import zombies.world.{CaptureTrap, DeathTrap, Floor, NeighborhoodCache, NoEntrance, Wall, World}
import rx._
import scaladget.svg.path._
import scaladget.tools._
import zombies.agent.Human.Army
import zombies.guitutils.controls._
import zombies.guitutils.parameters.ParameterName

import scala.scalajs.js.timers

/*
 * Copyright (C) 24/03/16 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 */

object display {

  object Color {

    val coldColor = (255, 238, 170)
    val hotColor = (255, 100, 0)
    val rescue = (55, 170, 200)

    val captureTrap = (150, 0, 0)
    val deathTrap =  (50, 0, 0)

    val entrance = (125, 120, 200)
    val wall = hotColor

    val baseR = (hotColor._1 - coldColor._1)
    val baseG = (hotColor._2 - coldColor._2)
    val baseB = (hotColor._3 - coldColor._3)

    def colors = Seq(rescue, captureTrap, entrance)

    def color(value: Double) = (
        (baseR * value + coldColor._1).toInt,
        (baseG * value + coldColor._2).toInt,
        (baseB * value + coldColor._3).toInt
      )

  }

  type Color = (Int, Int, Int)

  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

  val rng = new Random(42)

  case class People(
                     humans: Int = 0,
                     zombies: Int = 0,
                     rescued: Int = 0,
                     alerted: Int = 0,
                     killed: Int = 0,
                     informed: Int = 0,
                     zombified: Int = 0)

  val controllerSeq = Var(Seq[Controller]())

  def init(initFunction: () => Simulation, controllerList: Seq[Controller]) = {

    val simulation = initFunction()
    val side = simulation.world.side

    val doorSize = 2
    val wallSize = (side - doorSize) / 2

    val stepBuffer: Var[Option[(Simulation, List[Event], Int, NeighborhoodCache)]] = Var(None)
    val stepState: Var[Option[(Simulation, List[Event], Int, NeighborhoodCache)]] = Var(None)

    val timeOut: Var[Option[Int]] = Var(None)
    val people: Var[People] = Var(People())

    val onOffControllerPositions = controllerList.collect(onOffControllers).map { ooc => ooc -> ooc.isOn }.toMap
    val invisibleControllers: Var[Seq[ParameterName]] = Var(Seq())

    onOffControllerPositions.foreach { case (ooc, isOn) =>
      isOn.trigger {
        invisibleControllers.update({
          if (isOn.now) invisibleControllers.now.filterNot(ic => ooc.childs.contains(ic)).distinct
          else invisibleControllers.now ++ ooc.childs
        }
        )
      }
    }

    val optionalControllers = controllerList.collect(optionalParameters).flatten


    val gridSize = 800

    val scene = svgTags.svg(
      width := gridSize,
      height := gridSize
    ).render


    val cellDimension = gridSize.toDouble / side

    val agentSize = cellDimension / 3
    val thirdAgentSize = agentSize / 3
    val offsetY = agentSize / 2
    val offsetX = agentSize / 3

    val agentPath = Path(precisionPattern = "%1.2f").m(0, agentSize).l(thirdAgentSize, 0).l(2 * thirdAgentSize, agentSize).l(thirdAgentSize, agentSize * 5 / 6).z

    def worldToInts(world: World, lineIndex: Int): Seq[Option[Color]] = {

      world.cells(lineIndex).map {
        case Wall => Some(Color.wall)
        case f: Floor =>
          (f.rescueZone, f.entrance == NoEntrance, f.trap) match {
            case (true, _, _) => Some(Color.rescue)
            case (_, false, _ )=> Some(Color.entrance)
            case (_, _, Some(CaptureTrap)) => Some(Color.captureTrap)
            case (_, _, Some(DeathTrap)) => Some(Color.deathTrap)
            case _ => None
          }
      }
    }

    def buildWorld(nbCellsByDimension: Int, world: World) = {
      val values = (1 to nbCellsByDimension).foldLeft(Seq[Seq[Option[Color]]]())((elems, index) => elems :+ worldToInts(world, index - 1)).transpose
      scene.appendChild(
        svgTags.rect(x := 0, y := 0, width := gridSize * cellDimension, height := gridSize * cellDimension,
          style := s"fill:rgb${Color.color(0)};").render
      )
      for {
        col <- (0 to nbCellsByDimension - 1).toArray
        colCoord = (col * cellDimension) + 1
        row <- (0 to nbCellsByDimension - 1).toArray
      } {
        val v = values(row)(col)
        v match {
          case Some(c) =>
            scene.appendChild(
              svgTags.rect(x := ((row * cellDimension) + 1), y := colCoord, width := cellDimension, height := cellDimension,
                style := s"fill:rgb${c};").render
            )
          case None =>
        }
      }
    }

    def buildAgents = {
      val element: SVGElement = scaladget.tools.rxSVGMod(Rx {
        svgTags.g((for {
          a <- stepState().map {
            _._1.agents
          }.getOrElse(Vector())
        } yield {
          val ax = "%1.2f".format((Agent.position(a)._2 * gridSize) + 1 - offsetX)
          val ay = "%1.2f".format((Agent.position(a)._1) * gridSize + 1 - offsetY)
          val rotation = "%1.2f".format(math.atan2(Agent.velocity(a)._2, -Agent.velocity(a)._1).toDegrees)
          val color = a match {
            case h: Human if h.function == Human.Army => "#08eafa"
            case h: Human if h.function == Human.RedCross => "#f50bfa"
            case h: Human => "#666666"
            case _ => "red"
          }
          svgTags.g(agentPath.render(svgAttrs.fill := color), svgAttrs.transform := s"rotate($rotation, ${ax}, ${ay}) translate(${ax},${ay})")
          //agentPath.render(svgAttrs.fill := color, svgAttrs.transform := s"rotate($rotation, ${ax}, ${ay}) translate(${ax - offsetX},${ay - offsetY})")
        }): _*)
      })
      scene.appendChild(element)
    }

    def step: Unit = stepBuffer.now match {
      case state@Some((simulationBuffer, eventBuffer, stateNumberBuffer, neighborhoodCache)) =>
        stepState.update(state)
        stepBuffer.update(None)

        val p = People(
          humans = simulationBuffer.agents.count(Agent.isHuman),
          zombies = simulationBuffer.agents.count(Agent.isZombie),
          rescued = eventBuffer.collect(Event.rescued).size,
          killed = eventBuffer.collect(Event.killed).size,
          informed = simulationBuffer.agents.collect(Agent.human).count(_.rescue.informed),
          alerted = simulationBuffer.agents.collect(Agent.human).count(_.rescue.alerted),
          zombified = eventBuffer.collect(Event.zombified).size,
        )

        people.update(p)
        timeOut.now.foreach(to => timers.setTimeout(to) {
          step
        })

        val (ns, ev) = _root_.zombies.simulation.Simulation.step(stateNumberBuffer + 1, simulationBuffer, neighborhoodCache, rng)
        stepBuffer.update(Some(ns, eventBuffer ++ ev, stateNumberBuffer + 1, neighborhoodCache))
      case None => timeOut.now.foreach(to => timers.setTimeout(to) {
        step
      })
    }

    val setupButton = button("Setup", btn_default, onclick := { () =>
      val simulation = initFunction()
      val stepNumber = 0
      val neighborhoodCache = World.visibleNeighborhoodCache(simulation.world, math.max(simulation.humanPerception, simulation.zombiePerception))

      stepBuffer.update(Some(simulation, List(), stepNumber, neighborhoodCache))
      stepState.update(Some(simulation, List(), stepNumber, neighborhoodCache))

      buildWorld(side, simulation.world)
      buildAgents
      people.update(People())
    })

    val stepButton = button(span(Rx {
      timeOut() match {
        case Some(_) => "Stop"
        case _ => "Start"
      }
    }), btn_danger, onclick := { () =>
      timeOut() = timeOut.now match {
        case None => Some(100)
        case _ => None
      }
      timeOut.now.foreach { _ => step }
    })

    val stats = span(styles.display.flex, flexDirection.row, justifyContent.spaceBetween)(
      span(`class` := "stat")(Rx {
        s"# step: ${stepState().map(_._3).getOrElse(0)}"
      }),
      span(`class` := "stat")(Rx {
        s"# humans: ${people().humans}"
      }),
      span(`class` := "stat")(Rx {
        s"# rescued: ${people().rescued}"
      }),
      span(`class` := "stat")(Rx {
        s"# informed: ${people().informed}"
      }),
      span(`class` := "stat")(Rx {
        s"# alerted: ${people().alerted}"
      }),
      span(`class` := "stat")(Rx {
        s"# zombified:  ${people().zombified}"
      }),
      span(`class` := "stat")(Rx {
        s"# zombies: ${people().zombies}"
      }),
      span(`class` := "stat")(Rx {
        s"# killed zombies: ${people().killed}"
      }),
    )

    val controllers = div(marginTop := 50, marginLeft := 10, marginRight := 10, maxWidth := 500, styles.display.flex, flexDirection.column, styles.justifyContent.center)(
      div(
        Rx {
          controllerList.map { p =>
            if (!optionalControllers.contains(p.name))
              span(styles.display.flex, flexDirection.row, paddingTop := 10, textAlign.right)(span(minWidth := 220, paddingRight := 20)(p.name), span(p.element, paddingLeft := 10), span(p.valueElement, paddingLeft := 20, fontWeight.bold)).render
            else span().render
          }
        }),
      span(styles.display.flex, styles.justifyContent.center)(buttonGroup(paddingTop := 20)(setupButton, stepButton))
    )

    val optional = div(marginTop := 50, marginLeft := 200, marginRight := 10, `class` := "optional", styles.display.flex, flexDirection.column, styles.justifyContent.flexEnd, alignItems.flexStart)(
      div(
        Rx {
          controllerList.map { p =>
            if (optionalControllers.contains(p.name) && !invisibleControllers().contains(p.name))
              span(styles.display.flex, flexDirection.row, paddingTop := 10, textAlign.right)(span(minWidth := 220, paddingRight := 20)(p.name), span(p.element, paddingLeft := 10), span(p.valueElement, paddingLeft := 20, fontWeight.bold)).render
            else span.render
          }
        }
      )
    )


    val sceneAndStats = div(marginTop := 50, marginLeft := 10, marginRight := 10, maxWidth := 500, styles.display.flex, flexDirection.column, styles.justifyContent.center)(
      scene,
      stats
    )

    org.scalajs.dom.document.body.appendChild(div(styles.display.flex, flexDirection.row, alignItems.flexStart, justifyContent.spaceAround)(controllers, sceneAndStats, optional))
  }

}