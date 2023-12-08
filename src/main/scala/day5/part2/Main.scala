package com.shabab
package day5.part2


import java.security.InvalidParameterException
import scala.collection.immutable.ListMap
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.Source
import scala.util.{Failure, Using}

//noinspection Duplicates
// Takes ~8 mins with the following JVM args:
// -Xmx8g -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -XX:G1HeapRegionSize=2 -XX:ParallelGCThreads=8 -XX:ConcGCThreads=8 -XX:+UseCompressedOops
// Profiler says less than 4G memory used max. Can experiment with batchSize value
object Main extends App {

  val process = Using(Source.fromResource("day5_p2.txt")) { source =>
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
        val seedsStr = line.split(":")(1)
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

    val seedRanges = inputAccum._2.grouped(2).toList
    val inputMap = inputAccum._3


    implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

    val answer = seedRanges.foldLeft((Long.MaxValue, 0L)) { case ((currentMin, progress), List(seed, length)) =>

      val iterationsNeeded = length
      val batchSize = 100_00

      val batchStep = iterationsNeeded / batchSize

      val seedRangeMin = Seq.range(0, batchStep + 1).par.map { c =>
        val start = seed + (batchSize * c)
        val end = Math.min(start + batchSize, seed + length)

        val calculatedMin = LazyList.iterate[Long](start)(_ + 1).takeWhile(_ < end).foldLeft(Future.successful(Long.MaxValue, 0)) {
          case (future, seedInRange) =>

            future.map { case (currentRangeMin, rangeCount) =>
              val newMin = inputMap.foldLeft(seedInRange) { case (current, (_, v)) =>

                v.find(_.isValueWithinSource(current)).map { a2b =>

                  a2b.getDestination(current)
                }.getOrElse(current)
              }

              val answerMin = Math.min(newMin, Math.min(currentRangeMin, currentMin))
              val progressedMade = rangeCount + 1

              (answerMin, progressedMade)
            }
        }

        Await.result(calculatedMin, Duration.Inf) match {
          case (min, _) => {

            min
          }
        }
      }

      val out = (seedRangeMin.min, progress + length)

      out
    }

    println(answer._1)

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

    start <= num && num < end
  }

  def getDestination(num: Long): Long = {
    val start = a
    val moveCount = num - start

    b + moveCount
  }

}

