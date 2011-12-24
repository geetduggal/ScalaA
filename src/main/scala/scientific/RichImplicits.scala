package scalaa.scientific

object RichImplicits {
  implicit def wrapIndexable[A](seq: IndexedSeq[A]) = new RichIndexable(seq)
  
  class RichIndexable[A](val seq: IndexedSeq[A]) { 
    val random = scala.util.Random
    def choice() = { seq(random.nextInt(seq.size)) }
  }
}
