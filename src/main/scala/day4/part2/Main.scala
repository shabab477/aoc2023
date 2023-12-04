package com.shabab
package day4.part2

import java.security.InvalidParameterException
import scala.io.Source
import scala.util.{Failure, Using}

object Main extends App {

  val process = Using(Source.fromResource("day4_p2.txt")) { source =>
    val CardLineRE = """Card\s+(\d+): (.+?) \| (.+)""".r
    val inputStructs = source.getLines().map {
      case CardLineRE(cardNum, winningPart, myPart) =>
        (cardNum.toInt, """\d+""".r.findAllIn(winningPart).map(_.toInt).toSet, """\d+""".r.findAllIn(myPart).map(_.toInt).toSet)
      case line => throw new InvalidParameterException(line)
    }

    val accumMap = inputStructs.foldLeft(Map[Int, Int]()) {
      case (map, (cardNumber, winningSet, mySet)) => {

        val commonCount = (winningSet & mySet).size
        val wonCards = List.range(cardNumber + 1, cardNumber + commonCount + 1, 1)

        val newKv = (cardNumber -> (map.get(cardNumber) match {
          case Some(value) => value + 1
          case _ => 1
        }))

        wonCards.foldLeft(map + newKv) { case (map, num) =>
          val newNumV = map.get(num) match {
            case Some(value) => value + newKv._2
            case _ => newKv._2
          }

          map + (num -> newNumV)
        }
      }
    }

    println(accumMap.values.sum)

  }

  process match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }
}
