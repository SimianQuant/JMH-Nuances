package simianquant.jmhnuances

import collection.mutable
import io.Source
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Count(value: Double, count: Int)

final class ReadHistogram(val name: String, val hist: List[Count])

object ReadHistogram {

  def apply(name: String, rawHist: Seq[Seq[Seq[Seq[Double]]]]): ReadHistogram = {
    val selectedName = name.split('.').last // the package name isn't particularly useful

    val pairMap = mutable.TreeMap.empty[Double, Int] // so that is is pre sorted
    var totalCount = 0
    for {
      a1 <- rawHist // forks, threads
      a2 <- a1 // iterations within a fork/thread
      a3 <- a2 // hist within iteration
    } {
      val k = a3(0)
      val oldCount = pairMap.getOrElse(k, 0)
      val countAddn = a3(1).toInt
      val newCount = oldCount + countAddn
      totalCount += countAddn
      pairMap += (k -> newCount)
    }

    val resHist = mutable.ListBuffer.empty[Count]
    val countCutoff = totalCount * 0.99 // only take 99.5% of the data
    var accCount = 0

    pairMap foreach {
      case (k, v) =>
        if (accCount < countCutoff) {
          resHist += Count(k, v)
          accCount += v
        }
    }

    new ReadHistogram(selectedName, resHist.toList)
  }

  implicit val persistedReads: Reads[ReadHistogram] = (
    (JsPath \ "benchmark").read[String] and
      (JsPath \ "primaryMetric" \ "rawDataHistogram").read[Seq[Seq[Seq[Seq[Double]]]]]
  )(ReadHistogram.apply _)

}

object HistogramBuilder {

  def main(args: Array[String]): Unit = {
    val sourceFile = args(0)
    val src = Source.fromFile(sourceFile).getLines.mkString
    println("Source read")
    val parsed = Json.parse(src).validate[Seq[ReadHistogram]]
    parsed match {
      case JsSuccess(data, _) =>
        println("successfully parsed")
        val outputBuf = mutable.ListBuffer.empty[String]
        outputBuf += "benchmark, value, count"
        data foreach { x =>
          val name = x.name
          x.hist foreach {
            case Count(value, count) =>
              outputBuf += s"$name, $value, $count"
          }
        }
        val outputString = outputBuf.mkString("\n")
        println("output constructed")
        val destPath = args(1)
        Files.write(Paths.get(destPath), outputString.getBytes(StandardCharsets.UTF_8))
        println("output written")
      case _ => println("parse failed")
    }

    println("ho gaya")
  }

}
