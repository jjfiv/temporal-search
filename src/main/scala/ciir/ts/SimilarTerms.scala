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

  def outputDocCSV(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("expected args: index-dir queries...")
    }

    val dates = new DateRetrieval(args.head)
    val queries = args.tail


    var header = false

    queries.foreach(query => {
      val dv = dates.toDateVector(dates.search(query))
      if(!header) {
        println("Document,"+dv.indices.map(idx => { dates.StartYear + idx }).mkString(","))
        header = true
      }
      println(query+","+dv.mkString(","))
    })
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

  def comparison(args: Array[String]) {
    if(args.size != 3) {
      Util.quit("expected args: index-dir query num-results")
    }

    val dates = new DateRetrieval(args(0))
    val query = args(1)
    val nRes = args(2).toInt

    println("# Initial Search:")
    val tfv = dates.search(query)
    val dv = dates.toDateVector(tfv)
    
    println("# Similarity Search:")
    
    //Util.timed("similarTF:", {
    //  dates.findSimilarTF(tfv, nRes).foreach {
    //    case SimilarTerm(term, score, _) => println(term+" "+score)
    //  }
    //})
    Util.timed("similarDate:", {
      dates.findSimilarDate(dv, nRes).foreach {
        case SimilarTerm(term, score, _) => println(term+" "+score)
      }
    })
    
  }

  def ofPairs(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("expected args: index-dir pairs.csv")
    }


    val dates = new DateRetrieval(args(0))
    val wordPairs = IO.fileLines(args(1)).foreach(_.split("\\s+") match {
      case Array(w1, w2, mturk) => {
        val humanScore = mturk.toDouble
        
        val tf1 = dates.search(w1)
        val dv1 = dates.toDateVector(tf1)
        val tf2 = dates.search(w2)
        val dv2 = dates.toDateVector(tf2)
    

        printf("%s,%s,%.3f,", w1, w2, humanScore)
        printf("%.3f,%.3f,", Math.squaredSimilarity(tf1, tf2), Math.squaredSimilarity(dv1, dv2))
        printf("%.3f,%.3f,", Math.cosineSimilarity(tf1, tf2), Math.cosineSimilarity(dv1, dv2))
        printf("%.3f,%.3f,", Math.JSSimilarity(tf1, tf2), Math.JSSimilarity(dv1, dv2))
        printf("%d,%d\n", Math.DTWSimilarity(tf1, tf2), Math.DTWSimilarity(dv1, dv2))
      }
    })

  }
}

