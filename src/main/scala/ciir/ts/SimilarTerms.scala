package ciir.ts

import gnu.trove.map.hash.{TIntIntHashMap, TIntLongHashMap}
import gnu.trove.set.hash.{TIntHashSet, TLongHashSet}
import collection.mutable.ArrayBuilder


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

