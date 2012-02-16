package scalaa.scientific
import scalaa.console.ProgressBar
import scala.collection.IterableLike
import scala.util.Random

import scala.collection._
import generic.CanBuildFrom
import mutable.Builder
import scala.collection.Set

object RichImplicits {
  implicit def wrapIndexable[A](seq: IndexedSeq[A]) = new RichIndexable(seq)
  implicit def wrapIterable[A, S[A] <: Iterable[A]](seq: S[A]) =
    new RichIterable(seq)
  
  // ArrayBuffer.fill(100000)(0).progress(.1).map{x=>x}
  
  class Progressable[A, S[A] <: Iterable[A]](seq: S[A], sample: Double) extends Iterable[A] {
    def iterator = seq.iterator
    override def foreach[U](f: A => U): Unit = {
      val pb = new ProgressBar(iterator.size, "*")
      val siter = (1/sample).toInt
      iterator.zipWithIndex.foreach{ case(a,i) =>
        f(a)
        if (i % siter == 0) { pb.update(i) }
      }
      pb.done()
    }
  }

  class RichIndexable[A](val seq: IndexedSeq[A]) {
    val random = Random
    def choice() = { seq(random.nextInt(seq.size)) }
  }

  class RichIterable[A, S[A] <: Iterable[A]](val seq: S[A]) {

    val b = seq.genericBuilder[A]
    val random = scala.util.Random

    def sample(k: Int) = {
      val n = seq.size
      // TODO: If you want more than half the elements, ...
      assert (k <= n, { println("k = "+k+" > n = "+n) } )
      var i = 0
      var numerator = k
      val it = seq.iterator
      while (numerator > 0) {
        val denominator = n - i
        val prob = numerator.toDouble/denominator
        val x = it.next
        if (random.nextDouble < prob) {
          b += x
          numerator -= 1
        }
        i += 1
      }
      b.result
    }

    def choice() = { sample(1).head }

    def progress(sample: Double = 1.0) = new Progressable(seq, sample)

  }

}

// Code bin

//implicit def wrapTraversable[A: ClassManifest, S <% Traversable[A]](seq: S) =
// new RichTraversable[A, S](seq)
//class RichTraversable[@specialized A: ClassManifest, S <% Traversable[A]](val seq: S) {
//  def diff()(implicit numeric:Numeric[A]): IndexedSeq[A] = {
//    val d = ArrayBuffer[A]()
//    var prev = seq.head
//    seq.tail.foreach{ x =>
//      //d += x-prev
//      d += numeric.minus(x, prev)
//      prev = x
//    }
//    d
//  }
//}

/*
class Progressable[A, C <: Iterable[A]]( xs: C with IterableLike[A,C] ) {
  def progress
}

trait ProgressableLike[+A, +S] extends IterableLike[A, S] {
  override def foreach[U](f: A => U): Unit = {
    val pb = new ProgressBar(iterator.size, "*")
    iterator.zipWithIndex.foreach{ case(a,i) =>
      f(a)
      pb.update(i)
    }
    pb.done()
  }
}

//trait Progressable[+A] extends ProgressableLike[A, Progressable[A]]

case class Progressor[A, S[A] <: Iterable[A]](val s: S[A] with IterableLike[A, S[A]]) extends IterableLike[A, S[A]]
//extends ProgressableLike[A, S[A]]
{

  //def newBuilder[A, That](implicit cbf: CanBuildFrom[S[A], A, That]): Builder[A, That] = cbf()
  def newBuilder[A]: Builder[A, S[A]] = s.genericBuilder//.mapResult{ x => Progressor( x.asInstanceOf[S[A]] ) }
  override def iterator = s.iterator
  override def foreach[U](f: A => U): Unit = {
    val pb = new ProgressBar(iterator.size, "*")
    iterator.zipWithIndex.foreach{ case(a,i) =>
      f(a)
      pb.update(i)
    }
    pb.done()
  }
}
*/
