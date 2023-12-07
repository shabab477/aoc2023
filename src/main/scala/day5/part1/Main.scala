package com.shabab
package day5.part1

import java.security.InvalidParameterException
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.{Failure, Using}

object Main extends App {
  val process = Using(Source.fromResource("day5_p1.txt")) { source =>
    val listOfSeedToSoil = Seq[AToB]()
    val listOfSoilToFertilizer = List[AToB]()
    val listOfFertilizerToWater = List[AToB]()
    val listOfWaterToLight = List[AToB]()
    val listOfLightToTemp = List[AToB]()
    val listOfTempToHumidity = List[AToB]()
    val listOfHumidityToLocation = List[AToB]()
    val mapIterator = Seq("ss", "sf", "fw", "wl", "lt", "th", "hl").iterator

    val inputAccum = source.getLines().zipWithIndex.foldLeft(
      (
        "",
        Seq[Long](),
        ListMap( // this is critical because I want to maintain order
          "ss" -> listOfSeedToSoil,
          "sf" -> listOfSoilToFertilizer,
          "fw" -> listOfFertilizerToWater,
          "wl" -> listOfWaterToLight,
          "lt" -> listOfLightToTemp,
          "th" -> listOfTempToHumidity,
          "hl" -> listOfHumidityToLocation
        )
      )
    ) {
      case (accum, (line, lineIndex)) if lineIndex == 0 =>
        val seedsStr= line.split(":")(1)
        val allSeeds = """\d+""".r.findAllIn(seedsStr).map(_.toLong).toSeq
        val collectedSeeds: Seq[Long] = accum._2 :++ allSeeds

        accum.copy(_2 = collectedSeeds)
      case (accum, (line, _)) if line.isBlank =>
        accum.copy(_1 = mapIterator.next())

      case ((collType, seeds, infoMap), (line, lineIndex)) if lineIndex > 0 && !line.contains(":") =>
        val info = """\d+""".r.findAllIn(line).map(_.toLong).toList

        val a2b = (info, infoMap.get(collType)) match {
          case (List(d, s, step), Some(value)) =>
            value :+ AToB(s, d, step)
          case _ => throw new InvalidParameterException()
        }

        val updatedMap = infoMap + (collType -> a2b)

        (collType, seeds, updatedMap)
      case (accum, _) => accum
    }

    val seeds = inputAccum._2
    val inputMap = inputAccum._3

    val answer = seeds.toList.map { seed =>
      inputMap.foldLeft(seed) { case (current, (_, v)) =>

        v.find(_.isValueWithinSource(current)).map { a2b =>

          a2b.getDestination(current)
        }.getOrElse(current)
      }
    }.min

    println(answer)
  }

  process match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }
}

case class AToB(a: Long, b: Long, step: Long) {

  def isValueWithinSource(num: Long): Boolean = {
    val start = a
    val end = start + step

    start <= num && num <= end
  }

  def getDestination(num: Long): Long = {
    val start = a
    val moveCount = num - start

    b + moveCount
  }

}

