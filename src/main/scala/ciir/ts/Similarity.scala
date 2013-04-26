package ciir.ts

abstract class SimilarityMethod(val qs: Array[Int]) {
  val dimension = qs.size
  def apply(xs: Array[Int]): Double
}

class CosineSimilarity(qs: Array[Int]) extends SimilarityMethod(qs) {
  // setup steps can be inefficient because we don't do them often
  val qMag = math.sqrt(qs.map(qi => qi*qi).sum.toDouble)
  val qis = qs.map(_.toDouble)

  def apply(xs: Array[Int]): Double = {
    assert(xs.size == dimension)

    var dot = 0.0
    var xMag = 0.0

    var idx = 0
    while(idx < dimension) {
      val qi = qis(idx)
      val xi = xs(idx).toDouble
      
      dot += qi*xi
      xMag += xi*xi

      idx += 1
    }

    dot / (qMag*math.sqrt(xMag))
  }
}

class DTWSimilarity(qs: Array[Int], val windowSize: Int) extends SimilarityMethod(qs) {
  var table = Array.fill(dimension+1, dimension+1) { Int.MaxValue }
  
  def min3(x: Int, y: Int, z: Int) = {
    math.min(x,math.min(y,z))
  }
  def resetTable() {
    // start value
    table(0)(0) = 0
    // rest of first row should be infinity
    var ii = 1
    while(ii <= dimension) {
      table(ii)(0) = Int.MaxValue
      ii+=1
    }
  }
  def difference(xs: Array[Int]) = {
    resetTable()

    var ii = 1
    while(ii <= dimension) {
      var jj = math.max(1,ii-windowSize)
      while(jj <= math.min(dimension, ii+windowSize)) {
        val cost = math.abs(qs(ii-1) - xs(jj-1))
        table(ii)(jj) = cost + min3(table(ii-1)(jj),
                                    table(ii)(jj-1),
                                    table(ii-1)(jj-1))

        jj+=1
      }
      ii+=1
    }
    table(dimension)(dimension).toDouble
  }
  def apply(xs: Array[Int]) = -difference(xs)
}

