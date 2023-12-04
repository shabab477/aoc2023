package com.shabab
package day3.part2

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Using}

object Main extends App {

  private def findTwoNumericParts: (List[(Int, List[NumericPart])] , Int, Int) => Option[(NumericPart, NumericPart)]
  = (numberMap: List[(Int, List[NumericPart])], x: Int, y: Int) => {
    val allRegions = Array((x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1),
      (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))

    val numberParts = allRegions.flatMap { case (nx, ny) =>
      numberMap.flatMap(_._2).filter(np => np.isWithin(nx, ny))
    }.toSet

    if (numberParts.size == 2) {
      Some(numberParts.head, numberParts.last)
    } else {
      None
    }
  }

  val result = Using(Source.fromResource("day3_p2.txt")) { source =>
    val allLines = source.getLines().toList
    val numberMap: List[(Int, List[NumericPart])] = allLines.zipWithIndex.map { case (line, lineIndex) =>
      val lineResult = line.toCharArray.zipWithIndex.foldLeft((List[NumericPart](), "")) {
        case ((currentList, currentStr), (currentCh, charIndex)) if !currentCh.isDigit => {
          if (currentStr.nonEmpty && currentStr.forall(_.isDigit)) {
            val numericPart = NumericPart(currentStr.toInt, (lineIndex, charIndex - currentStr.length), (lineIndex, charIndex - 1))

            (currentList :+ numericPart, "")
          } else {

            (currentList, "")
          }
        }
        case ((currentList, currentStr), (currentCh, _)) => (currentList, s"$currentStr$currentCh")
      }

      lineResult match {
        case (currentList, lastStr) if lastStr.nonEmpty && lastStr.forall(_.isDigit) => {
          val numericPart = NumericPart(lastStr.toInt, (lineIndex, line.length - lastStr.length), (lineIndex, line.length - 1))

          (lineIndex, currentList :+ numericPart)
        }
        case (currentList, _) => (lineIndex, currentList)
      }
    }

    val answers = allLines.zipWithIndex.flatMap { case (lineStr, lineIndex) =>
      lineStr.toCharArray.zipWithIndex.filter { case (ch, _) =>
        ch == '*'
      }.flatMap { case (_, charIndex) =>
        findTwoNumericParts(numberMap, lineIndex, charIndex)
      }
    }

    val total = answers.map { case (p1, p2) =>
      p1.number * p2.number
    }.sum

    println(total)
  }

  result match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }

}

case class NumericPart(number: Int, from: (Int, Int), to: (Int, Int)) {

  def isWithin(point: (Int, Int)): Boolean = {

    point match {
      case (x, y) if x == from._1 && x == to._1 => {

        y >= from._2 && y <= to._2
      }
      case _ => false
    }
  }
  override def toString: String = s"$number (${from._1},${from._2} -- ${to._1},${to._2})"
}
