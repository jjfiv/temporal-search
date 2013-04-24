package ciir.ts

object Math {
  def cosineSimilarity(as: Array[Int], bs: Array[Int]): Double = {
    assert(as.size == bs.size)

    var dot = 0.0
    var magA = 0.0
    var magB = 0.0

    var idx = 0
    while(idx < as.size) {
      val ai = as(idx).toDouble
      val bi = bs(idx).toDouble
      
      dot += ai*bi
      magA += ai*ai
      magB += bi*bi

      idx += 1
    }

    dot / (math.sqrt(magA)*math.sqrt(magB))
  }

  def earthMoverSimilarity(as: Array[Int], bs: Array[Int]): Double = {
    var runningSum = 0
    var lastValue = 0
    
    var asum = 0
    var bsum = 0

    var idx = 0
    while(idx < as.size) {
      asum += as(idx)
      bsum += bs(idx)
      val currentValue = (as(idx) + lastValue) - bs(idx)
      runningSum += math.abs(currentValue)
      lastValue = currentValue

      idx+=1
    }

    -runningSum.toDouble
  }
}

