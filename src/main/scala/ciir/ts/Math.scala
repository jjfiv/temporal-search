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

  def min3(x: Int, y: Int, z: Int) = {
    math.min(x,math.min(y,z))
  }
  def DTWDistance(as: Array[Int], bs: Array[Int], window: Int) = {
    val N = as.size
    var table = Array.fill(N+1,N+1) { Int.MaxValue }
    table(0)(0) = 0

    var ii = 1
    while(ii <= N) {
      var jj = math.max(1,ii-window)
      while(jj <= math.min(N, ii+window)) {
        val cost = math.abs(as(ii-1) - bs(jj-1))
        table(ii)(jj) = cost + min3(table(ii-1)(jj),
                                    table(ii)(jj-1),
                                    table(ii-1)(jj-1))

        jj+=1
      }
      ii+=1
    }
    table(N)(N)
  }
  def DTWSimilarity(as: Array[Int], bs: Array[Int]) = {
    - DTWDistance(as, bs, 10) // amount of year flexibility, hard coded as 10 for me for now
  }

  def movingAverage(input: Array[Int], window: Int=5) = {
    val windows = input.size - (window - 1)
    val output = new Array[Int](windows)
    
    output.indices.foreach(i => {
      (0 until window).foreach(j => {
        output(i) += input(i+j)
        j+=1
      })
    })
    output.indices.foreach(i => {
      output(i) = (output(i).toDouble / window.toDouble).toInt
    })
    output
  }
}

object Statistics {
  def mean(xs: Array[Int]) = {
    xs.sum.toDouble / xs.size.toDouble
  }
  def mean(xs: Array[Double]) = {
    xs.sum / xs.size
  }
  def median(xs: Array[Int]): Double = {
    val tmp = xs.sorted
    val mid = tmp.size/2
    if(tmp.size % 2 == 1) {
      tmp(mid)
    } else {
      (tmp(mid-1) + tmp(mid)) / 2
    }
  }
  def variance(xs: Array[Int]) = {
    val mu = mean(xs)
    mean(xs.map(x =>{
      val off = x.toDouble - mu
      off*off
    }))
  }
  def maxIndex(xs: Array[Int]) = { 
    xs.zipWithIndex.maxBy(_._1)._2
  }
  def minIndex(xs: Array[Int]) = { 
    xs.zipWithIndex.minBy(_._1)._2
  }
  
  def summary(data: Array[Int], base: Int=0) = {
    println("  total: "+data.sum)
    println("  max: "+data.max+" in "+(base+maxIndex(data)))
    println("  mean: "+mean(data))
    println("  variance: "+variance(data))
    println("  median: "+median(data))
    println("  min: "+data.min+" in "+(base+minIndex(data)))
  }
}


