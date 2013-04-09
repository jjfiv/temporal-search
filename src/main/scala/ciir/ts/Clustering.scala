package ciir.ts

import util.Random
import collection.mutable.ArrayBuilder


object KMeans {
  type DataVector = Array[Double]
  def cosineDistance(v1: DataVector, v2: DataVector) = {
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

  def maxIndex(data: Array[Double]): Int = {
    var maxIdx = 0
    var maxValue = Double.NegativeInfinity
    val len = data.size
    
    Util.loopUntil(len)(idx => {
      val curValue = data(idx)
      if(maxValue < curValue) {
        maxValue = curValue
        maxIdx = idx
      }
    })
    maxIdx
  }

  def cluster(clusters: Array[DataVector], data: Array[DataVector], enforceExpectedSize: Boolean): Array[Array[Int]] = {
    val numClusters = clusters.size
    var clusterBuilders = Array.fill(numClusters) { new ArrayBuilder.ofInt }
    val expectedSize = data.size / numClusters

    data.indices.map(vecIdx => {
      val vec = data(vecIdx)
      // get a score for each cluster
      val scores = clusters.map(clv => { cosineDistance(vec, clv) })
      // find best match cluster
      val bestCluster = maxIndex(scores)
      var cluster = bestCluster
      assert(cluster < numClusters && cluster >= 0)

      if(enforceExpectedSize) {
        // loop through clusters until back to original, looking for an okay place to put this guy..
        while(((cluster+1) % numClusters) != bestCluster && clusterBuilders(cluster).result().size > expectedSize) {
          cluster = (cluster + 1) % numClusters
        }
      }
      // put in best match cluster
      clusterBuilders(cluster) += vecIdx
    })

    clusterBuilders.map(_.result())
  }

  def centroid(clusterContents: Array[Int], allData: Array[DataVector]): DataVector = {
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

  def clusterEvenly(num: Int, data: Array[DataVector], labels: Array[String]) = {
    val step = (data.size / num)
    var results = new Array[DataVector](num)
    Util.loopUntil(num)(idx => {
      results(idx) = centroid(Array.tabulate(10)(x => step*idx + x), data)
    })
    results
  }

  def sampleEvenly(num: Int, data: Array[DataVector], labels: Array[String]) = {
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
    //val firstCentroids = sampleEvenly(num, data, labels)
    val firstCentroids = clusterEvenly(num, data, labels)
    val firstClusters = cluster(firstCentroids, data, true)

    // calculate the center of these naive clusters
    val newCentroids = firstClusters.map(centroid(_, data))

    // re-cluster data
    val finalResults = cluster(newCentroids, data, false)
    
    finalResults.zipWithIndex.map {
      case (contents, idx) => {
        contents.sortBy(vecIdx => cosineDistance(data(vecIdx), newCentroids(idx)))
      }
    }
  }

  def test() = {
    val data: Array[Tuple2[String, Array[Double]]] = Array(
      ("chocolate", Array(9, 9, 0, 0, 0)),
      ("vanilla", Array(0, 8, 7, 0, 0)),
      ("strawberry", Array(4, 8, 2, 0, 0)),
      
      ("puppies", Array(0, 1, 4, 6, 2)),
      ("kittens", Array(1, 0, 2, 8, 2)),
      ("ducklings", Array(0, 0, 2, 5, 5))
    )

    val vectors = data.map(_._2)
    val labels = data.map(_._1)

    //println(vectors.map("["+_.mkString(",")+"]").mkString(","))
    //println(labels.mkString(","))

    val clusters = kmeans(2, vectors, labels)

    clusters.zipWithIndex.foreach {
      case (contents, i) => println("Cluster "+i+": "+contents.map(labels(_)).mkString(","))
    }
  }
}

