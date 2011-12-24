import scalaa.utils.time
import scala.math.Numeric

package scalaa.utils.console

/** Class that computes the running average of a variety of numeric types
  * Author: Rob Patro */
class RunningAverage {
  var _prev = 0.0
  var _n = 0L
 
  /** Add a number to the moving average */
  def add[@specialized(Int,Long,Float,Double) T](x:T)(implicit numeric:Numeric[T]) : Unit = {
    _prev = (numeric.toDouble(x) + (_n*_prev) ).toDouble / (_n+1)
    _n += 1
  }

  /** Get the moving average */
  def get = { _prev }
}

/** Class that displays a progress bar on the console 
  * Author: Rob Patro
  *
  * @constructor create a new progress bar with a maximum value
  * @param tot the maximum value of the progress bar
  * @param ms the string used to indicate an increment of progress
  * @param length the console-width of the progress bar (default 80)
  */
class ProgressBar(tot:Int, ms:String, length:Int = 80 )  {

  val dString = "[] 000.0% : ETA 00h 00m 00s"

  // Length of the actual bar is length - the 
  // space required by the other decorations
  val blength = length - dString.size
  var ltime = System.currentTimeMillis
  var updates = 0
  val timer = new Timer
  val updateTimer = new Timer
  var eta : Double = 0.0
  var neta : Double = 0.0
  val avg_update_time = new RunningAverage

  /** Convert a floating point time value to a string */
  def nanoToTime( m:Double) : String = {
    var t = m / 1000.0//scala.math.pow(10.0,9)
    val hours = (t / 3600.0 ).toInt
    t -= hours * 3600.0
    val minutes = (t / 60.0 ).toInt
    t -= minutes * 60.0
    val seconds = t.toInt

    return "%02dh %02dm %02ds" format (hours, minutes, seconds)
  }

  /** Update the progress bar with the current value */
  def update( v:Int ) = {
    if ( v < tot ) {
      updateTimer.stop
      updateTimer.start
      if ( v > 0 ) {         
        timer.stop
        timer.start
        val at = timer.elapsed / v.toDouble
        eta = (tot -  v) * at
      }

      //print(" [[[ updateTimer = "+updateTimer.elapsed+" ]]] ")
      if ( updateTimer.elapsed >= 1000.0 ) {
        neta = eta
        updateTimer.reset
      }
      
      val etas = nanoToTime(neta)

      val lam = (v / tot.toDouble)
      val s0 = ms * (blength * lam).toInt 
      val s1 = " " * (blength - (blength * lam).toInt) 
      val pers = String.format("%5.1f", Double.box(lam * 100.0))
      print("\r["+s0+s1+"] "+pers+"% : ETA "+etas)
    } else {
      val s0 = ms * blength 
      print("\r["+s0+"] 100.0% : ETA 00h 00m 00s")
    }
    
  }

  /** Finalize the progress bar */
  def done() = { print("\n") }

  update(0)
}
