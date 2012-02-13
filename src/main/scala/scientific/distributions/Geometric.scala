package scalaa.scientific.distributions
import scala.math._
import scala.util.Random

class GeometricRV(val p: Double) {
    val random = new Random(System.currentTimeMillis)
    // removed the +1 below (we should be able to visit 0 more vertices)
    def next = { (log(random.nextDouble)/log(1-p)).toInt }
}
