package ciir.ts

import org.junit.Test
import org.junit.Assert._

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

    val iceCream = clusters(0).map(labels(_)).toSet
    val littleThings = clusters(1).map(labels(_)).toSet

    assertTrue(iceCream.contains("chocolate"))
    assertTrue(iceCream.contains("vanilla"))
    assertTrue(iceCream.contains("strawberry"))
    assertEquals(iceCream.size, 3)
    assertTrue(littleThings.contains("puppies"))
    assertTrue(littleThings.contains("kittens"))
    assertTrue(littleThings.contains("ducklings"))
    assertEquals(littleThings.size, 3)

    //clusters.zipWithIndex.foreach {
    //  case (contents, i) => println("Cluster "+i+": "+contents.map(labels(_)).mkString(","))
    //}
  }

}
