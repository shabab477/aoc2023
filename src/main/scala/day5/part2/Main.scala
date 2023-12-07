package com.shabab
package day5.part2


import java.security.InvalidParameterException
import java.util.concurrent.atomic.AtomicLong
import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

//noinspection Duplicates
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

    val totalLength = seedRanges.map {
      case List(_, length) => length
    }.sum

    val answer = seedRanges.par.foldLeft((Long.MaxValue, 0L)) { case ((currentMin, progress), List(seed, length)) =>
      val calculatedMin = LazyList.iterate[Long](seed)(_ + 1).takeWhile(_ < seed + length).foldLeft((Long.MaxValue, 0)) {
        case ((currentRangeMin, rangeCount), seedInRange) =>

          val newMin = inputMap.foldLeft(seedInRange) { case (current, (_, v)) =>

            v.find(_.isValueWithinSource(current)).map { a2b =>

              a2b.getDestination(current)
            }.getOrElse(current)
          }

          val answerMin = Math.min(newMin, Math.min(currentMin, currentRangeMin))
          val progressedMade = rangeCount + 1
          println(s"Progress: ${((progressedMade) / totalLength.toFloat) * 100}%")

          (answerMin, progressedMade)
      }._1

      val out = (calculatedMin, progress + length)

      println(s"Progress: ${((out._2) / totalLength.toFloat) * 100}%")
      out
    }

    println(answer._1)

//    val answer = lazySeeder.grouped(batchSize)
//      .zipWithIndex
//      .map { case (batchedList, batchCount) =>
//        val futures = batchedList.map { num =>
//          Future {
//            seedRanges.exists { case Seq(seed, length) =>
//
//              val start = seed
//              val endExcluding = seed + length
//
//              start <= num && num < endExcluding
//            }
//          }.map { doesExist =>
//            if (doesExist) {
//              inputMap.foldLeft(num) { case (current, (_, v)) =>
//
//                v.find(_.isValueWithinSource(current)).map { a2b =>
//
//                  a2b.getDestination(current)
//                }.getOrElse(current)
//              }
//            } else {
//              minMaxValue._2
//            }
//          }
//        }.foldLeft(Future.successful(minMaxValue._2)) { case (x, y) =>
//          for {
//            currentNum <- x
//            otherNum <- y
//          } yield {
//            if (currentNum < otherNum) {
//              currentNum
//            } else {
//              otherNum
//            }
//          }
//        }
//
//        val result = Await.ready(futures, Duration.Inf).value.get
//
//        println(s"Finished processing: batch: ${batchCount + 1}, Progress: ${((batchCount + 1) / batchStep.toFloat) * 100}%")
//        result match {
//          case Success(value) => value
//          case Failure(exception) =>
//            exception.printStackTrace()
//            throw exception
//        }
//      }.foldLeft(minMaxValue._2) { case (x, y) =>
//        if (x < y) x else y
//      }
//
//    println(answer)

  }
  //    val test = lazySeeder.par.filter { num =>
  //      seedRanges.exists { case Seq(seed, length) =>
  //
  //        val start = seed
  //        val endExcluding = seed + length
  //
  //        start >= num && num < endExcluding
  //      }
  //    }
  //
  //    println(test.min)

  //    val answer = seeds.grouped(2).toList.par.map {
  //      case Seq(start, length) =>
  //
  //        (start, start + length)
  //      case _ => throw  new InvalidParameterException()
  //    }.map { case (from, toExcluding) =>
  //
  //      LazyList.range(from, toExcluding, 1).par.map { seed =>
  //
  //        inputMap.foldLeft(seed) { case (current, (_, v)) =>
  //
  //          v.find(_.isValueWithinSource(current)).map { a2b =>
  //
  //            a2b.getDestination(current)
  //          }.getOrElse(current)
  //        }
  //      }.min
  //    }.min

  //    println(answer)

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

