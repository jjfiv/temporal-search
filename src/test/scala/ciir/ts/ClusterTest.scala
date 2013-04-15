package ciir.ts

import org.junit.Test

class ClusterTest {
  @Test
  def twoClusters {
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

    val clusters = KMeans.kmeans(2, vectors, labels)

    clusters.zipWithIndex.foreach {
      case (contents, i) => println("Cluster "+i+": "+contents.map(labels(_)).mkString(","))
    }
  }

}
