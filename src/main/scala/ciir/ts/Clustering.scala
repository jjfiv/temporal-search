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

  def cluster(clusters: Array[DataVector], data: Array[DataVector]): Array[Array[Int]] = {
    val numClusters = clusters.size
    var clusterBuilders = Array.fill(numClusters) { new ArrayBuilder.ofInt }

    data.indices.map(vecIdx => {
      val vec = data(vecIdx)
      // get a score for each cluster
      val scores = clusters.map(clv => { cosineDistance(vec, clv) })
      // find best match cluster
      val cluster = maxIndex(scores)
      assert(cluster < numClusters && cluster >= 0)
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

  def kmeans(num: Int, data: Array[DataVector]) = {
    require(num < data.size)

    // group based on first K elements
    val firstCentroids = data.take(num)
    val firstClusters = cluster(firstCentroids, data)

    // calculate the center of these naive clusters
    val newCentroids = firstClusters.map(centroid(_, data))

    // re-cluster data
    val finalResults = cluster(newCentroids, data)
    
    finalResults
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

    val clusters = kmeans(2, vectors)

    clusters.zipWithIndex.foreach {
      case (contents, i) => println("Cluster "+i+": "+contents.map(labels(_)).mkString(","))
    }
  }
}

