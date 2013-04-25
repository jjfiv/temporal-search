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

    - runningSum.toDouble
  }

  def squaredSimilarity(as: Array[Int], bs: Array[Int]): Double = {
    var sum = 0.0

    var idx = 0
    while(idx < as.size) {
      val ai = as(idx).toDouble
      val bi = bs(idx).toDouble
      
      sum += (ai-bi)*(ai-bi)

      idx += 1
    }

    - math.log(sum)
  }

  // http://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  def JSDivergenceFlat(as: Array[Int], bs: Array[Int]): Double = {
    // calculate the size of the input arrays so as to turn them into proper probabilities
    val (asum, bsum) = {
      var atotal = 0
      var btotal = 0
      var idx = 0
      while(idx < as.size) {
        atotal += as(idx)
        btotal += bs(idx)
        idx += 1
      }
      
      (atotal.toDouble, btotal.toDouble)
    }

    // do the calculation of m, KL(a,m) and KL(b,m) in one loop for efficiency
    var amScore = 0.0
    var bmScore = 0.0
    var idx = 0
    while(idx < as.size) {
      val ai = as(idx).toDouble / asum
      val bi = bs(idx).toDouble / bsum
      val mi = ai + bi / 2.0

      if(ai != 0) amScore += math.log(ai/mi) * ai
      if(bi != 0) bmScore += math.log(bi/mi) * bi

      idx += 1
    }
    
    (amScore + bmScore) / 2.0
  }

  def JSSimilarity(as: Array[Int], bs: Array[Int]): Double = {
    1.0 - JSDivergenceFlat(as,bs)
  }
}

