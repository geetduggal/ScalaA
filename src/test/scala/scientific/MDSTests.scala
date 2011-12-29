import org.scalatest.FunSuite
import scalaa.scientific.MDS
import scalala.tensor.dense._
import scalala.library.LinearAlgebra._
import scalala.tensor.::
import scalala.operators._

class MDSTests extends FunSuite {
  val D = DenseMatrix.ones[Double](4,4) - DenseMatrix.eye[Double](4)
  val (x,error) = MDS.classicalMDS(D, 3)
  println(x)
  println(error)
}
