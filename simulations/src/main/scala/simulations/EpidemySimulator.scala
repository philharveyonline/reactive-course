package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val infectionRate = 0.4
    val deathRate = 0.25

    val maxTimeBetweenMoves = 5

    val incubationPeriod = 6
    val deathPeriod = 14
    val immunePeriod = 16
    val healthyPeriod = 18
  }

  import SimConfig._

  private def generatePersonList(): List[Person] = {
    val people = (1 to population) map { id =>
      val p = new Person(id)
      if (id <= population * prevalenceRate) {
        p.infect()
      }
      afterDelay(0)(p.move())
      p
    }
    people.toList
  }

  val persons: List[Person] = generatePersonList()

  case class Room(row: Int, col: Int) {

    private def wrap(x: Int, numValues: Int) = {
      x match {
        case -1 => numValues - 1 // wrap under
        case `numValues` => 0 // wrap over
        case okValue => okValue
      }
    }

    def containsSickOrDead(): Boolean = {
      persons exists { person =>
        this == person.room && person.sickOrDead
      }
    }

    def containsInfectious(): Boolean = {
      persons exists { person =>
        this == person.room && person.infected
      }
    }

    def adjacentRooms(): List[Room] = {
      val left = wrap(col - 1, roomColumns)
      val right = wrap(col + 1, roomColumns)
      val up = wrap(row - 1, roomRows)
      val down = wrap(row + 1, roomRows)

      List(
        Room(row, left),
        Room(row, right),
        Room(up, col),
        Room(down, col)) filterNot (room => room.containsSickOrDead)
    }
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def room() = Room(row, col)
    def setRoom(room: Room) = {
      row = room.row
      col = room.col
    }

    def sickOrDead(): Boolean = {
      sick || dead
    }

    def infect(): Unit = {
      infected = true
      afterDelay(incubationPeriod) {
        sick = true
      }
      afterDelay(deathPeriod) {
        if (random < deathRate) {
          dead = true
        }
      }
      afterDelay(immunePeriod) {
        if (!dead) {
          sick = false
          immune = true
        }
      }
      afterDelay(healthyPeriod) {
        if (!dead) {
          infected = false
          sick = false
          immune = false
        }
      }
    }

    def maybeInfect(): Unit = {
      if (infected || dead || immune) {
        // we can't become infected
      } else {
        if (room.containsInfectious) {
          if (random < infectionRate) {
            infect
          }
        }
      }
    }

    /**
     * Move now, and schedule another move later
     */
    protected[simulations] def move(): Unit = {
      if (!dead) {
        val adjacentRooms = room.adjacentRooms
        if (!adjacentRooms.isEmpty) {
          val nextRoomIndex = randomBelow(adjacentRooms.size)
          val nextRoom = adjacentRooms(nextRoomIndex)
          setRoom(nextRoom)
          maybeInfect()
        }
        val timeUntilNextMove = randomBelow(maxTimeBetweenMoves + 1)

        afterDelay(timeUntilNextMove) { move() }
      }
    }
  }
}
