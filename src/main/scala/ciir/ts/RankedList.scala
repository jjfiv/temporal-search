package ciir.ts

// hooray generics
// manifest is a trick to keep more type information
// and ordered implies that A will have a compare function defined
class RankedList[A <% Ordered[A]: Manifest](val numHits: Int) {
  private var results = new Array[A](numHits)
  private var count = 0

  def insert(newest: A) {
    if(count < numHits) {
      results(count) = newest
      count += 1
      return
    }

    // discard new things with tiny scores, special case
    if(results.last > newest) {
      return
    }

    // chop current array in pieces and reassemble
    val (better, worse) = results.partition(_ > newest)
    results = (better :+ newest) ++ worse.dropRight(1)
  }

  def done = {
    if (count <= numHits) {
      results.take(count).sorted.reverse
    } else {
      results.reverse
    }
  }
}

