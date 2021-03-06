package scalaa.datastruct

import scala.collection.mutable.{OpenHashMap=>Map}
import scala.collection.mutable.Set
import scala.collection.mutable.{ HashSet, Set => GraphList}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.Ordered

class SlimGraph[N <% Ordered[N]]() {
  val adjList = Map[N, GraphList[N]]()
  val weights = Map[(N,N), Double]()
  var size = 0
  
  def nodes = adjList.keys
  
  def hasEdge(u: N, v: N, directed: Boolean = false ) = { 
    val fwdEdge = if (adjList.contains(u)) { adjList(u).contains(v) } else { false }
    val bkEdge = if ( ! directed && adjList.contains(v) ) { adjList(v).contains(u) } else { false }
    if ( ! directed ) { fwdEdge && bkEdge } else { fwdEdge }
  }

  def connectedComponents = {
    val components = ArrayBuffer[SlimGraph[N]]()
    val seen = Set[N]()
    
    nodes.foreach{ v=>
      
        if (!seen.contains(v)) {
          val subcomp = Set[N]()
          
          traverse(v){ x=> 
              subcomp += x
              seen += x
            true
          }
          
          components += subgraph(subcomp)
          
        }
    }
    components
  }

  def getWeight(u: N, v: N) = weights((u,v))  

  def setWeight(u: N, v: N, weight:Double) = weights((u,v)) = weight

  def insertEdge(u: N, v: N, directed: Boolean = false, weight: Double = Double.MaxValue) {

    def insertAdjList(u: N, v: N) {

      if (!adjList.contains(u)) { adjList(u) = GraphList.empty[N] }
      if (!adjList.contains(v)) { adjList(v) = GraphList.empty[N] }
      adjList(u) += v
      size += 1
      if (weight < Double.MaxValue) {
        weights((u,v)) = weight
      }
    }

    insertAdjList(u,v)
    if (!directed) { insertAdjList(v,u) }
    
  }

  def removeEdge(u: N, v: N, directed: Boolean = false, nodeAlso: Boolean = false) {

    def removeAdjList(u: N, v: N) {
      adjList(u) -= v
      size -= 1
      weights -= ((u,v))
      if (adjList(u).size == 0 && nodeAlso) { removeNode(u) }
    }

    removeAdjList(u,v)
    if (!directed) { removeAdjList(v,u) }
  }
  
  def insertNode(u: N) = if( ! (adjList contains u) ) { adjList(u) = GraphList.empty[N] }
  def removeNode(u: N) = adjList -= u

  def neighbors(u: N) = { adjList(u) }

  def order = adjList.size

  def edges = {
    val es = ArrayBuffer[(N, N)]()

    nodes.foreach { u=>
      es ++= adjList(u).map{ v => (u,v) }
    }

    es
  }
  
  def subgraph(nodeSubset: Set[N]) = {
    val subg = SlimGraph[N]()

    nodeSubset.foreach { u=>
      adjList(u).foreach{ v =>
        if (nodeSubset.contains(v)) { subg.insertEdge(u,v, true) }
      }
    }

   subg
  }
  
  def copy = {
    val g = SlimGraph[N]()
    nodes.foreach{ u=> 
      adjList(u).foreach{ v=> g.insertEdge(u,v, directed=true) }
    }
    g
  }

  def edgesUndir = { edges.filter{ case (v,w) => v < w } }
  
  def traverse(t: N)(f: N => Boolean) {
    var nextLevel = Set.empty[N]
    val visited = Set.empty[N]
    nextLevel += t

    while ( nextLevel.size > 0 ) {
      val thisLevel = nextLevel
      nextLevel = Set.empty[N]

      thisLevel.foreach{ u =>

        if ( !visited(u) ) {
          visited.add( u )
          if(f(u)) {neighbors(u).foreach{ uval => nextLevel.add(uval)}}
        }

      }

    }
  }
  
  def completesTriangle(v: N, w: N): Boolean = {
    adjList.foreach{ case(u, s) => 
      if (s.contains(v) && s.contains(w)) { return true }
    }
    return false
  }
 
 /**
 * Return an array of the vertices in a topologically sorted order;
 * raises an exception if the graph is cyclic or undirected
 * 
 * DFS based on the wikipedia article at http://en.wikipedia.org/wiki/Topological_sorting
 */
 def topologicalOrdering(): ArrayBuffer[N] = {
    val L = new ArrayBuffer[N]( order )
    val visited = HashSet.empty[N]

    val inNeighbors = Map.empty[N, GraphList[N]]
    // Starts out empty
    nodes.foreach{ n => inNeighbors(n) = GraphList.empty[N] }
    // Build the incoming neighbor list
    nodes.foreach{ u => neighbors(u).foreach{ v => inNeighbors(v) += u } }

    // All nodes with no outgoing edges
    val S = scala.collection.mutable.Stack( nodes.filter{ n => neighbors(n).size == 0 } )
    nodes.foreach{ n => visit(n) }

    def visit( u: N ) {
      if ( ! visited.contains(u) ) {
        visited += u
        inNeighbors(u).foreach{ v =>  visit(v) }
        L += u
      }
    }
    // We have to have everything in L
    assert( L.size == nodes.size, "Size of topo order list is %d, but there are %d nodes".format(L.size, nodes.size) )
    L
 }

}

object GraphAlgorithms {

  def triangles[N <% Ordered[N]](graph: SlimGraph[N]) = {
    val tris = ArrayBuffer[(N, N, N)]()
    val graphc = graph.copy


    graphc.nodes.foreach{ v=>
      val vn = graphc.neighbors(v)

      vn.foreach{ w=>
        graphc.removeEdge(v,w)
        val wn = graphc.neighbors(w)
        val vnew = graphc.neighbors(v)
        (vnew & wn).foreach { x =>  tris += ((v,w,x)) }
 
      }
    
    }

    
    tris
  }


  def undirect[N <% Ordered[N]](digraph: SlimGraph[N]) = {
    val graph = SlimGraph[N]()
    digraph.edges.foreach{ case(v,w) => graph.insertEdge(v,w,false) }
    graph
  }

}

object EdgeListReader {

  def fromFile( fname: String, directed: Boolean = false, weighted: Boolean = false ) = {
    val fsrc = Source.fromFile( fname )
    val graph = SlimGraph[String]()

    fsrc.getLines.foreach{ l =>

        if ( !l.trim.startsWith("#") ) { // ignore comments
          val toks = l.split("""\s+""").toList
          val (u,v) = (toks(0), toks(1))

          val weight = if (weighted) { toks(2).toDouble } else { 1.0 }
          if (!graph.hasEdge(u, v, directed)) { graph.insertEdge(u,v,directed,weight) }

        }

    }

    graph
  }

}

object AdjListReader {

  def fromFile( fname: String, directed: Boolean = false, weighted: Boolean = false ) = {
    val fsrc = Source.fromFile( fname )
    val graph = SlimGraph[String]()

    fsrc.getLines.foreach{ l =>
      if ( !l.trim.startsWith("#") ) { // ignore comments
        val toks = l.split("""\s+""").toList
        val u = toks.head
        val vList = toks.tail
        val weight = 1.0
        // handle isolated vertex
        graph.insertNode(u)
        vList.foreach{ v => if(!graph.hasEdge(u,v,directed)) { graph.insertEdge(u,v,directed,weight) } }        
      }
    }
    graph
  }
}

object MultiAdjListReader {

  def fromFile( fname: String, directed: Boolean = false, weighted: Boolean = false ) = {
    val lineIt = Source.fromFile( fname ).getLines
    val graph = SlimGraph[String]()

    def addNeighbors( u: String, n: Int ) {
      (0 until n).foreach{ i =>
        val l = lineIt.next
        val toks = l.split("""\s+""").toList
        val v = toks(0)
        val weight = if (weighted) { toks(1).toDouble } else { 1.0 }
        if (!graph.hasEdge(u,v,directed)) { graph.insertEdge(u,v,directed,weight) }
      }
    }

    while (lineIt.hasNext) {
      val l = lineIt.next
      if ( !l.trim.startsWith("#") ) { // ignore comments
        val toks = l.split("""\s+""").toList
        val (u, n) = (toks(0), toks(1).toInt)
        addNeighbors(u, n)
      }
    }
    graph
  }
}

object SlimGraph {
  def apply[N <% Ordered[N]]() = { new SlimGraph[N]() }
  def empty[N <% Ordered[N]]() = { new SlimGraph[N]() }
}
