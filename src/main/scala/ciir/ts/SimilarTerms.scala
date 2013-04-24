package ciir.ts

import gnu.trove.map.hash.{TIntIntHashMap, TIntLongHashMap}
import gnu.trove.set.hash.{TIntHashSet, TLongHashSet}
import collection.mutable.ArrayBuilder

object Statistics {
  def mean(xs: Array[Int]) = {
    xs.sum.toDouble / xs.size.toDouble
  }
  def mean(xs: Array[Double]) = {
    xs.sum / xs.size
  }
  def median(xs: Array[Int]): Double = {
    val tmp = xs.sorted
    val mid = tmp.size/2
    if(tmp.size % 2 == 1) {
      tmp(mid)
    } else {
      (tmp(mid-1) + tmp(mid)) / 2
    }
  }
  def variance(xs: Array[Int]) = {
    val mu = mean(xs)
    mean(xs.map(x =>{
      val off = x.toDouble - mu
      off*off
    }))
  }
  def maxIndex(xs: Array[Int]) = { 
    xs.zipWithIndex.maxBy(_._1)._2
  }
  def minIndex(xs: Array[Int]) = { 
    xs.zipWithIndex.minBy(_._1)._2
  }
  
  def summary(data: Array[Int], base: Int=0) = {
    println("  total: "+data.sum)
    println("  max: "+data.max+" in "+(base+maxIndex(data)))
    println("  mean: "+mean(data))
    println("  variance: "+variance(data))
    println("  median: "+median(data))
    println("  min: "+data.min+" in "+(base+minIndex(data)))
  }
}


object SimilarTerms {
  def cli(args: Array[String]) {
    if(args.size != 1 || !IO.dirExists(args(0))) {
      Util.quit("expected args: galago-index-dir")
    }
    val retrieval = new DateRetrieval(args(0))

    println("Ready for input!")
    Util.whileCLI("> ") {
      case "" => true
      case "q" | "quit" | "exit" => {
        false
      }
      case "?" | "h" | "help" => {
        println("quit to exit")
        true
      }
      case line => {
        val cmd = line.takeWhile(!_.isWhitespace)
        val rest = line.drop(cmd.size).trim

        if(rest.nonEmpty) {
          cmd match {
            case "search" => {
                retrieval.dateSearch(rest).take(10).foreach {
                  case DocDateScore(doc, date, score) => {
                    printf("%-60s %4d %10d\n", retrieval.getDocName(doc), date, score)
                  }
                }
              true
            }
            case "date-summary" => {
              val raw = retrieval.dateSearch(rest)
              val byDate = raw.groupBy(_.date)
              true
            }
            case _ => true
          }
        } else {
          println(" *** error: empty query")
          true
        }
      }
    }
  }

  def quantized(args: Array[String]) {
    if(args.size != 4) {
      Util.quit("expected args: index-dir query years-per-quantile num-results-per-quantile")
    }

    val dates = new DateRetrieval(args(0))
    val query = args(1)
    val qSize = args(2).toInt
    val nRes = args(3).toInt

    println("# Initial Search:")
    val tfv = dates.search(query)
    val dv = dates.toDateVector(tfv)
    
    println("# Similarity Search:")
    
    dates.findSimilarQuantized(dv, nRes, qSize)
    println("")
  }
}

