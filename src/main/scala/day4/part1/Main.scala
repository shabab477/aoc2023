package com.shabab
package day4.part1

import java.security.InvalidParameterException
import scala.io.Source
import scala.util.{Failure, Using}

object Main extends App {

  val process = Using(Source.fromResource("day4_p1.txt")) { source =>
    val CardLineRE = """Card.+(\d+): (.+?) \| (.+)""".r
    val inputStructs = source.getLines().map {
      case CardLineRE(cardNum, winningPart, myPart) =>
        (cardNum, """\d+""".r.findAllIn(winningPart).map(_.toInt).toSet, """\d+""".r.findAllIn(myPart).map(_.toInt).toSet)
      case line => throw new InvalidParameterException(line)
    }

    val result = inputStructs.map {
      case (_, winningSet, mySet) => {
        val commonCount = (winningSet & mySet).size
        commonCount match {
          case _ if commonCount == 0 => commonCount
          case _ => Math.pow(2, commonCount - 1)
        }
      }
    }.sum

    println(result.toInt)
  }

  process match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }
}
