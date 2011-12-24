package scalaa.scientific
import scalala.tensor.dense._
import scalala.library.Plotting._
import java.awt.Color

/** Miscellaneous functions for plotting */
object Plot { 

  /** Plot the distribution of a sample with nb bins */
  def dist( sample: Seq[Double], nb: Int) {
    val dh = new Histogram(sample, nbins = nb)
    //val y = dh.hist.map{ v => log(v+1) }
    val y = dh.hist
    //val x = DenseVector.range(0, y.size).map{ x => log(x+1) }
    val x = DenseVector.range(0, y.size)
    val s = DenseVector.ones[Double](y.size) * 0.05
  
    val labels : PartialFunction[Int,String] = { case i : Int => "" }
    val tips : PartialFunction[Int,String] = {case i : Int => i.toString }
    val literalColors : PartialFunction[Int,Color] = { case x : Int => Color.BLUE }
    
  
    scatter( x, y, s, literalColors, labels=labels, tips=tips)
    xlabel( "Histogram bin # (low => high)" )
    ylabel( "log(frequency)" )
  }
  
  /** Create a rank plot of the sample */
  def rank( sample: Seq[Double] ) {
    val y = DenseVector(sample:_*)
    val x = DenseVector.range(0, y.size)
    val s = DenseVector.ones[Double](y.size) * 0.01
  
    val labels : PartialFunction[Int,String] = { case i : Int => "" }
    val tips : PartialFunction[Int,String] = {case i : Int => i.toString }
    val literalColors : PartialFunction[Int,Color] = { case x : Int => Color.BLUE }
    
    scatter( x, y, s, literalColors, labels=labels, tips=tips)
    xlabel( "rank" )
    ylabel( "value" )
  }
}
