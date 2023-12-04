package com.shabab
package day3.part1

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Using}

object Main extends App {

  private def isSpecialSymbol: Char => Boolean = ch => ch != '.' && !ch.isDigit
  private def isDigit: Char => Boolean = ch => ch.isDigit
  private def isPeriod: Char => Boolean = ch => ch == '.'

  private def hasSpecialAdjacent: (Array[Array[Char]], Int, Int) => Boolean
  = (engineMap: Array[Array[Char]], x: Int, y: Int) => {
    val allRegions = Array((x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1),
      (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))

    val lowestX = 0
    val highestX = engineMap.length - 1
    val lowestY = 0
    val highestY = engineMap(0).length - 1

    allRegions.exists {
      case (nx, ny) if (nx >= lowestX && nx <= highestX) && (ny >= lowestY && ny <= highestY) =>
        isSpecialSymbol(engineMap(nx)(ny))
      case _ => false
    }
  }

  val result = Using(Source.fromResource("day3_p1.txt")) { source =>

    val engineMap = source.getLines().map(_.toCharArray).toArray
    val result = engineMap.zipWithIndex.map { case (line, lineIndex) =>
      val eol = line.zipWithIndex.foldLeft((List[String](), "", false)) {

        case ((collectedList, collectedString, flag),
        (currentChar, _)) if isPeriod(currentChar) || isSpecialSymbol(currentChar) => {
          if (flag) (collectedList :+ collectedString, "", false) else (collectedList, "", false)
        }

        case ((collectedList, collectedString, flag),
        (currentChar, charIndex)) if isDigit(currentChar) => {
          val newCurrentChar = s"$collectedString$currentChar"

          newCurrentChar match {
            case _ if hasSpecialAdjacent(engineMap, lineIndex, charIndex) => (collectedList, newCurrentChar, true)
            case _ => (collectedList, newCurrentChar, flag)
          }
        }
      }

      eol match {
        case (accumList, str, flag) if flag && str(str.length - 1).isDigit => (accumList :+ str, str, flag)
        case e => e
      }
    }.map(_._1).filter(_.nonEmpty).flatten.map(_.toInt).sum

    println(result)
  }

  result match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }

}
