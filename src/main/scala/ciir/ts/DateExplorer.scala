package ciir.ts

// rename galago, avoid RSI
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

import GalagoCore.retrieval.LocalRetrieval
import GalagoTupleflow.Parameters

import gnu.trove.map.hash._

class LocalDateInfo(val retrieval: LocalRetrieval) {
  var cache = new TIntIntHashMap
  
  private def retrieveDate(documentName: String): Int = {
    var parms = new Parameters
    parms.set("terms", false)
    parms.set("tags", false)
    parms.set("text", false)
    parms.set("metadata", true)
    
    var dateStr = retrieval.getDocument(documentName, parms).metadata.get("date")
    
    if(dateStr == null) { return -1 }
    
    dateStr = dateStr.filter(_.isDigit)
    try {
      return dateStr.toInt
    } catch {
      case nfe: NumberFormatException => return -1
    }
  }
  
  private def retrieveDate(doc: Int): Int = try {
    retrieveDate(retrieval.getDocumentName(doc))
  } catch {
    case e: java.io.IOException => -1
  }
  
  def getDate(doc: Int) = {
    if(cache.containsKey(doc)) {
      cache.get(doc)
    } else {
      val date = retrieveDate(doc)
      cache.put(doc, date)
      date
    }
  }
}

object DateCurve {
  def ofTroveHash(term: String, data: TIntIntHashMap) = {
    val dates = data.keys
    val minDate = dates.min
    val maxDate = dates.max
    val numDates = (maxDate - minDate) + 1

    var counts = new Array[Int](numDates)

    dates.foreach(date => {
      val count = data.get(date)
      if(count > 0 && date >= minDate && date <= maxDate) {
        val index = date - minDate
        counts(index) += count
      }
    })

    DateCurve(term, minDate, counts)
  }
  def decode(dis: java.io.DataInputStream) = {
    val term = dis.readUTF
    val numDates = dis.readInt
    var hash = new TIntIntHashMap
    
    Util.loopUntil(numDates)(x => {
      val date = dis.readInt
      val count = dis.readInt
      hash.put(date, count)
    })
    
    ofTroveHash(term, hash)
  }
}

case class DateCurve(val term: String, val startDate: Int, val counts: Array[Int]) {
  def numDates = counts.size
  def endDate = startDate + numDates
  def weight = counts.sum
  def encode(dos: java.io.DataOutputStream) {
    val hash = toTroveHash()

    dos.writeUTF(term)
    dos.writeInt(hash.size)
    hash.keys.foreach( date => {
      val count = hash.get(date)
      dos.writeInt(date)
      dos.writeInt(count)
    })
  }
  def toTroveHash() = {
    val data = counts
    val minDate = startDate
    var hash = new TIntIntHashMap

    Util.loopUntil(numDates)(idx => {
      val count = data(idx)
      if(count > 0) {
        hash.put(minDate + idx, count)
      }
    })
    
    hash
  }
}

object DateExplorer {
  var UsefulCountMinimum = 50

  def buildIndex(args: Array[String]) {
    println("DateExplorer::buildIndex ["+args.mkString(",")+"]")

    if(args.size != 2) {
      Console.err.println("Need two arguments (Galago input index) and (output file)")
    }

    val inputFile = args(0)
    val outputFile = args(1)

    if(!Util.fileExists(inputFile)) {
      Console.err.println("Tried to build an index from non-existent file \""+inputFile+"\"")
    }

    var fp = Util.binaryOutputStream(outputFile)
    
    var curvesKept = 0
    var curvesTouched = 0

    // handle each argument as a galago index
    val retrieval = GalagoIndexUtil.retrievalFromPath(inputFile)
    val index = retrieval.getIndex
    
    var dateInfo = new LocalDateInfo(retrieval)

    Util.timed("inspect postings", { 
      GalagoIndexUtil.forKeyInIndex(index, "postings", (term,docIter) => {
        // only touch ascii terms for now, they're more likely OCR issues than valid identifiers
        // TODO
        if(term.forall(_.isLetterOrDigit)) {
          // investigate current term
          var hits = new TIntIntHashMap
          var hitCount = 0
        
          curvesTouched += 1

          if(curvesTouched % 10000 == 0) { println( "# curvesTouched = "+curvesTouched) }

          GalagoIndexUtil.forDocInInvertedList(docIter, (doc, count) => {
            val date = dateInfo.getDate(doc)
            if(date > 0) {
              hits.adjustOrPutValue(date, count, count)
              hitCount += count
            }
          })
          
          if(hitCount >= UsefulCountMinimum) {
            curvesKept += 1
            DateCurve.ofTroveHash(term,hits).encode(fp)
          }
        }
      })
    })

    println("Parsed "+curvesTouched+" and kept " + curvesKept)
    fp.close()
  }

  // outputs a CSV to stdout
  def plotTerms(args: Array[String]) {
    if(args.size < 2) {
      Console.err.println("Needs at least two arguments (Galago input index) and (terms...)")
      sys.exit(-1)
    }

    val galagoIndex = args.head
    val terms = args.tail

    val retrieval = GalagoIndexUtil.retrievalFromPath(galagoIndex)
    val dateInfo = new LocalDateInfo(retrieval)

    val numDocs = retrieval.getCollectionStatistics("#lengths:document:part=lengths()").documentCount.toInt

    val results = terms.map(term => {
      var dateCounts = new TIntIntHashMap
      RawSearch.runQuery(retrieval, numDocs, term).foreach( sdoc => {
        val date = dateInfo.getDate(sdoc.document)
        if(date > 0) {
          val count = sdoc.score.toInt
          dateCounts.adjustOrPutValue(date, count, count)
        }
      })
      
      dateCounts
    })

    // collect all dates
    val dates = results.map(_.keys.toSet).reduce(_ | _).toArray.sorted
    
    println("Date,"+terms.mkString(","))
    dates.foreach(date => {
      println(date+","+results.map(_.get(date)).mkString(","))
    })
  }

  def kmeansTerms(args: Array[String]) {
    val curvesFile = args(0)
    if(!Util.fileExists(curvesFile)) {
      Console.err.println("First argument curvesFile='"+curvesFile+"' should exist.")
      sys.exit(-1)
    }

    var minDate = Int.MaxValue
    var maxDate = Int.MinValue
    val allCurves = Util.timed("Load curves", {
      var fp = Util.binaryInputStream(curvesFile)
      var curveBuilder = Array.newBuilder[DateCurve]

      try {
        while(true) {
          val curve = DateCurve.decode(fp)
          // keep running domain
          if(curve.startDate < minDate) minDate = curve.startDate
          if(curve.endDate > maxDate) maxDate = curve.endDate
          curveBuilder += curve
        }
      } catch {
        case eof: java.io.EOFException => { }
        //case ioe: java.io.IOException => { }
      } finally {
        fp.close()
      }
      
      curveBuilder.result()
    })
    println("loaded "+allCurves.size+" curves")
    println("minDate = "+minDate)
    println("maxDate = "+maxDate)

    assert(maxDate > minDate)

    maxDate = 1920 // chop off poorly specified extras

    // turn corpus into aligned arrays
    val numDates = maxDate - minDate + 1
    val vectors = Util.timed("align corpus", {
      allCurves.flatMap(curve => {
        if(curve.startDate < maxDate) {
          var data = Array.fill(numDates) { 0 }
          var pos = curve.startDate - minDate
          curve.counts.copyToArray(data, pos, curve.numDates)
          Some(data.map(_.toDouble))
        } else None
      })
    })
    val labels = allCurves.map(_.term)

    println("# kmeans 30 groups, "+vectors.size+" vectors, "+vectors(0).size+" dimensions")
    val clusters = Util.timed("cluster", { KMeans.kmeans(30, vectors, labels) })
    println("# Done clustering")
    clusters.zipWithIndex.foreach {
      case (contents, i) => {
        val topIndices = contents.sortBy(allCurves(_).weight).takeRight(10)
        val topTerms = topIndices.map(allCurves(_).term)
        println("Cluster "+i+": "+topTerms.mkString(", "))
      }
    }
    
  }
}
