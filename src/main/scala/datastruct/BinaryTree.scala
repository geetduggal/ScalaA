// Based on the implementation discussed in: http://aperiodic.net/phil/scala/s-99/

package scalaa.datastruct

sealed abstract class BinaryTree[+T]

case class BinaryTreeNode[+T](value: T, left: BinaryTree[T], right: BinaryTree[T]) 
     extends BinaryTree[T] {
  override def toString = 
    "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends BinaryTree[Nothing] {
  override def toString = "."
}

object BinaryTreeNode {
  def apply[T](value: T): BinaryTreeNode[T] = BinaryTreeNode(value, End, End)
}
