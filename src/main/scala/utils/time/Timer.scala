package scalaa.utils.time

/** Class that provides the ability to simply measure the elapsed time
  *
  * @author Rob Patro 
  */
class Timer {
  private var _tstart = 0L
  private var _telapsed = 0L

  /** Start the timer */
  def start = { _tstart = System.currentTimeMillis }
  
  /** Stop the timer */
  def stop = { 
    val e = if(_tstart > 0L) { System.currentTimeMillis - _tstart } else { 0L } 
    _telapsed += e
  }
  
  /** Reset the timer */
  def reset = { _telapsed = 0L; _tstart = 0L }
  
  /** Return the amount of time that has elapsed in milliseconds since the
    * timer started */
  def elapsed = _telapsed
}
