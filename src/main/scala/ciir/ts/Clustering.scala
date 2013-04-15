package ciir.ts

import util.Random
import collection.mutable.ArrayBuilder


object KMeans {
  type DataVector = Array[Double]
  private def cosineDistance(v1: DataVector, v2: DataVector) = {
    var dotProduct = 0.0
    var mag1 = 0.0
    var mag2 = 0.0

    Util.loopUntil(v1.size)(idx => {
      val x = v1(idx)
      val y = v2(idx)

      dotProduct += x*y
      mag1 += x*x
      mag2 += y*y
    })

    val denominator = math.sqrt(mag1)*math.sqrt(mag2)
    if(denominator == 0.0) {
      0.0
    } else {
      1.0 - (dotProduct / denominator)
    }
  }

  private def cluster(clusters: Array[DataVector], data: Array[DataVector], enforceExpectedSize: Boolean): Array[Array[Int]] = {
    val numClusters = clusters.size
    var clusterBuilders = Array.fill(numClusters) { new ArrayBuilder.ofInt }
    val expectedSize = data.size / numClusters

    data.indices.map(vecIdx => {
      val vec = data(vecIdx)
      // sort cluster indices by distance to each cluster
      var dest = clusters.indices.sortBy(clIdx => {
        cosineDistance(vec, clusters(clIdx)) 
      })

      // drop any results with full buckets if required
      if(enforceExpectedSize) {
        val openBuckets = dest.filter(clusterBuilders(_).result.size > expectedSize)
        if(openBuckets.size != 0) {
          dest = openBuckets
        }
      }
      
      // put in best match cluster
      clusterBuilders(dest.head) += vecIdx
    })

    clusterBuilders.map(_.result())
  }

  private def centroid(clusterContents: Array[Int], allData: Array[DataVector]): DataVector = {
    val dim = allData(0).size
    var result = Array.fill(dim) { 0.0 }
    
    clusterContents.foreach(vecIdx => {
      val vec = allData(vecIdx)
      Util.loopUntil(vec.size)(idx => {
        result(idx) += vec(idx)
      })
    })
    
    result
  }

  private def clusterEvenly(num: Int, data: Array[DataVector], labels: Array[String]) = {
    val step = (data.size / num)
    var results = new Array[DataVector](num)
    Util.loopUntil(num)(idx => {
      results(idx) = centroid(Array.tabulate(10)(x => step*idx + x), data)
    })
    results
  }

  private def sampleEvenly(num: Int, data: Array[DataVector], labels: Array[String]) = {
    val step = (data.size / (num+1))
    var results = new Array[DataVector](num)
    Util.loopUntil(num)(idx => {
      results(idx) = data(step*idx)
      println("Sample: "+labels(step*idx))

    })
    results
  }

  def kmeans(num: Int, data: Array[DataVector], labels: Array[String]) = {
    require(num < data.size)

    // group based on first K elements
    //val firstCentroids = data.take(num)
    val firstCentroids = sampleEvenly(num, data, labels)
    //val firstCentroids = clusterEvenly(num, data, labels)
    val firstClusters = cluster(firstCentroids, data, true)

    // calculate the center of these naive clusters
    val newCentroids = firstClusters.map(centroid(_, data))

    // re-cluster data
    val finalResults = cluster(newCentroids, data, false)
    
    finalResults
  }

  def test() = {
  }
}

