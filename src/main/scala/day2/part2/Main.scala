package com.shabab
package day2.part2

import scala.io.Source
import scala.util.Using


object Main extends App {

  final val RequiredRed = 12
  final val RequiredGreen = 13
  final val RequiredBlue = 14

  Using(Source.fromResource("day2_p2.txt")) { source =>
    val games = source.getLines().map { line =>

      val id = line.split(":")(0).replace("Game ", "").toInt
      val gameLine = line.split(":")(1)
      val gameSets = gameLine.split(";").map { gameSetLine =>

        gameSetLine.split(",").map(_.trim).map {
          case greenLine if greenLine.contains("green") =>
            val number = greenLine.split("\\s+")(0).toInt
            GameSet(green = number)
          case redLine if redLine.contains("red") =>
            val number = redLine.split("\\s+")(0).toInt
            GameSet(red = number)
          case blueLine =>
            val number = blueLine.split("\\s+")(0).toInt
            GameSet(blue = number)
        }.foldLeft(GameSet()) {(x, y) =>

          GameSet(red = x.red + y.red, green = x.green + y.green, blue = x.blue + y.blue)
        }
      }

      Game(id, gameSets)
    }

    val result = games.map(_.gameSet).map { gameSet =>
      val maxRed = gameSet.map(_.red).max
      val maxGreen = gameSet.map(_.green).max
      val maxBlue = gameSet.map(_.blue).max

      maxRed * maxGreen * maxBlue
    }.sum

    println(result)
  }
}

case class Game(id: Int, gameSet: Array[GameSet])
case class GameSet(red: Int = 0, green: Int = 0, blue: Int = 0) {
  override def toString: String = s"red: $red, green: $green, blue: ${blue}"
}