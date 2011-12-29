import org.scalatest.FunSuite
import scalaa.datastruct._

class BinaryTreeTests extends FunSuite {

  test("Simple Binary Tree Test") {
    // Based on the implementation discussed in: http://aperiodic.net/phil/scala/s-99/
    
    println(BinaryTreeNode('a',
     BinaryTreeNode('b', BinaryTreeNode('d'), BinaryTreeNode('e')),
     BinaryTreeNode('c', End, BinaryTreeNode('f', BinaryTreeNode('g'), End))))
  }

}
