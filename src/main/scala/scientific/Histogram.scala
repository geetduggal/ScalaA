import scala.math._
import scalala.tensor.dense._
package scalaa.utils.scientific

object Histogram {
  implicit def toOption[T]( x:T ): Option[T] = { Option(x) }
}

/** Class to create a histogram from a sequence of values */
class Histogram( vals: Seq[Double], drange: Option[(Double,Double)] = None, nbins: Int = 10, normed: Boolean = false ) {

  // The bin width.  Simply the bounds of the histogram divided by the number of bins
  private val h = {
    if ( !drange.isEmpty ) {
      (drange.get._2 - drange.get._1) / nbins
    } else {
      (vals.max - vals.min) / nbins
    }
  }

  private val numVal = vals.size
  private val minVal = vals.min
  private val invNormFact = if (normed) { 1.0 / (h*numVal) } else { 1.0 }
  private val b = 1e-10
  private def bin( v:Double ) = { max(0, floor((v - minVal - b) / h).toInt) }

  // Array of values for the histogram bins
  val hist = {
    val h = DenseVector.zeros[Double](nbins)
    // For each value, compute the bin in which it resides and
    // appropriately update that bin's frequency (weight)
    vals.foreach{
      v => h( bin(v) ) += invNormFact
    }
    h
  }

}
