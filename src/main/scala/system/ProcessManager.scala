package scalaa.system
import scala.actors.Future
import scala.actors.Futures._
import scala.sys.process._

/** Various tools for managing system processes
 *  This is still in its infancy. */
object ProcessManager { 
   /** Wait for a future executing a closure after a timeout */
   def runOrDie( t: Long, f: Future[Any], killfun: () => Unit ): Option[Any] = {
    val l = awaitAll(t, f)
    if ( ! l(0).isDefined ){ 
      killfun() 
      return Option.empty[Any]
    }
    l(0)
  }
 
  /** Convenience function to launch a process and kill it after a timeout */
  def launch(pname: String, timeout: Int): Option[Any] = { 
    val proc = Process(pname)
    val pio = new ProcessIO(stdin => { }, stdio => { }, stderr => { })

    val r = proc.run(pio)
    val f = future{ r.exitValue }
    runOrDie(timeout, f, () => { println("Killing process"); r.destroy } )
  }

}
