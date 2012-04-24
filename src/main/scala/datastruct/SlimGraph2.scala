import scala.collection.mutable.{ OpenHashMap => Map, HashSet }
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.Ordered

package scalaa.datastruct.SlimGraph2 {

  trait SimpleEdgeLike[N] {
    val source: N
    val target: N
  }

  trait UnorderedEdgeLike[N] extends SimpleEdgeLike[N] {
    override def hashCode() = { source.hashCode + target.hashCode }
    override def equals(other: Any) = other match {
      case o: SimpleEdgeLike[N] => {
        (o.source == source && o.target == target)
      }
      case _ => false
    }
  }

  trait OrderedEdgeLike[N] extends SimpleEdgeLike[N] {
    override def hashCode() = { source.hashCode + 2 * target.hashCode }
    override def equals(other: Any) = other match {
      case o: SimpleEdgeLike[N] => {
        (o.source == source && o.target == target)
      }
      case _ => false
    }
  }

  trait WeightedEdgeLike[W] {
    var weight: W
  }

  object UndirectedEdge {
    def factory[N <% Ordered[N]] = { (u: N, v: N) => UndirectedEdge(u, v) }
  }

  object DirectedEdge {
    def factory[N] = { (u: N, v: N) => DirectedEdge(u, v) }
  }

  object WeightedUndirectedEdge {
    def factory[N <% Ordered[N], W] = { (u: N, v: N, w: W) => WeightedUndirectedEdge(u, v, w) }
    def factoryWithDefault[N <% Ordered[N], W](w: W) = { (u: N, v: N) => WeightedUndirectedEdge(u, v, w) }
  }

  object WeightedDirectedEdge {
    def factory[N, W] = { (u: N, v: N, w: W) => WeightedDirectedEdge(u, v, w) }
    def factoryWithDefault[N, W](w: W) = { (u: N, v: N) => WeightedDirectedEdge(u, v, w) }
  }

  case class UndirectedEdge[N <% Ordered[N]](u: N, v: N) extends UnorderedEdgeLike[N] {
    val (source, target) = if (u > v) { (v, u) } else { (u, v) }
  }

  case class WeightedUndirectedEdge[N <% Ordered[N], WT](u: N, v: N, var weight: WT) extends UnorderedEdgeLike[N] with WeightedEdgeLike[WT] {
    val (source, target) = if (u > v) { (v, u) } else { (u, v) }
  }

  case class DirectedEdge[N](source: N, target: N) extends OrderedEdgeLike[N]

  case class WeightedDirectedEdge[N, WT](source: N, target: N, var weight: WT) extends OrderedEdgeLike[N] with WeightedEdgeLike[WT]

  trait EdgeChangedEvent[N, E <: SimpleEdgeLike[N]] {
    val e: E
  }

  case class EdgeAddedEvent[N, E <: SimpleEdgeLike[N]](e: E) extends EdgeChangedEvent[N, E]
  case class EdgeRemovedEvent[N, E <: SimpleEdgeLike[N]](e: E) extends EdgeChangedEvent[N, E]

  trait VertexChangedEvent[N] {
    val v: N
  }

  case class VertexAddedEvent[N](v: N) extends VertexChangedEvent[N]
  case class VertexRemovedEvent[N](v: N) extends VertexChangedEvent[N]

  trait ObservableGraph[N, E <: SimpleEdgeLike[N]] {
    type EdgeHandle
    type VertexHandle
    type EdgeCallback = (EdgeChangedEvent[N, E]) => Unit
    type VertexCallback = (VertexChangedEvent[N]) => Unit

    val edgeCallbacks = Map.empty[EdgeHandle, EdgeCallback]
    val vertexCallbacks = Map.empty[VertexHandle, VertexCallback]

    def observeEdge(callback: EdgeCallback): EdgeHandle = {
      val handle = createEdgeHandle(callback)
      edgeCallbacks += (handle -> callback)
      handle
    }

    def observeVertex(callback: VertexCallback): VertexHandle = {
      val handle = createVertexHandle(callback)
      vertexCallbacks += (handle -> callback)
      handle
    }

    def unobserveEdge(handle: EdgeHandle): Unit = {
      edgeCallbacks -= handle
    }
    def unobserveVertex(handle: VertexHandle): Unit = {
      vertexCallbacks -= handle
    }

    protected def createEdgeHandle(callback: (EdgeChangedEvent[N, E]) => Unit): EdgeHandle
    protected def createVertexHandle(callback: (VertexChangedEvent[N]) => Unit): VertexHandle
    protected def notifyEdgeListeners(e: EdgeChangedEvent[N, E]): Unit = for (callback <- edgeCallbacks.values) callback(e)
    protected def notifyVertexListeners(v: VertexChangedEvent[N]): Unit = for (callback <- vertexCallbacks.values) callback(v)
  }

  trait DefaultHandles[N, E <: SimpleEdgeLike[N]] extends ObservableGraph[N, E] {
    type EdgeHandle = ((EdgeChangedEvent[N, E]) => Unit)
    type VertexHandle = ((VertexChangedEvent[N]) => Unit)
    protected def createEdgeHandle(callback: (EdgeChangedEvent[N, E]) => Unit): EdgeHandle = callback
    protected def createVertexHandle(callback: (VertexChangedEvent[N]) => Unit): VertexHandle = callback
  }

  class ObservableSlimGraphUndirected[N <% Ordered[N], E <: SimpleEdgeLike[N]] extends SlimGraphUnDirected[N, E] with ObservableGraph[N, E] with DefaultHandles[N, E] {

    override def insertEdge(e: E) = {
      val r = super.insertEdge(e)
      if (r) { notifyEdgeListeners(EdgeAddedEvent(e)) }
      r
    }
    override def removeEdge(e: E) = {
      val r = super.removeEdge(e)
      if (r) { notifyEdgeListeners(EdgeRemovedEvent(e)) }
      r
    }
    override def insertVertex(n: N) = {
      val r = super.insertVertex(n)
      if (r) { notifyVertexListeners(VertexAddedEvent(n)) }
      r
    }
    override def removeVertex(n: N) = {
      val r = super.removeVertex(n)
      if (r) { notifyVertexListeners(VertexRemovedEvent(n)) }
      r
    }
  }

  class NeighborCache[N, E <: SimpleEdgeLike[N], G <: ObservableSlimGraphUndirected[N, E]](graph: G) {
    private val VertexHandle = graph.observeVertex(modifiedVertex)
    private val EdgeHandle = graph.observeEdge(modifiedEdge)

    //case class MarkedNeighbors( var changed: Boolean = true, val neighbors: Set[N] = Set.empty[N] )
    val neighbors = Map.empty[N, Set[N]]

    def stopObserving {
      graph.unobserveEdge(EdgeHandle)
      graph.unobserveVertex(VertexHandle)
    }

    def modifiedVertex(ve: VertexChangedEvent[N]) {
      ve match {
        case VertexAddedEvent(v) => Unit
        case VertexRemovedEvent(v) => {
          if (neighbors.contains(v)) {
            neighbors(v) = Set.empty[N]
          }
        }

      }
    }

    def modifiedEdge(ee: EdgeChangedEvent[N, E]) {
      ee match {
        case EdgeAddedEvent(e) => {
          if (neighbors.contains(e.source)) { neighbors(e.source) += e.target }
          if (neighbors.contains(e.target)) { neighbors(e.target) += e.source }
        }

        case EdgeRemovedEvent(e) => {
          if (neighbors.contains(e.source)) { neighbors(e.source) -= e.target }
          if (neighbors.contains(e.target)) { neighbors(e.target) -= e.source }
        }

      }
    }

    def neighborVertexSet(v: N): Set[N] = {
      if (!neighbors.contains(v)) {
        neighbors += (v -> (Set.empty[N] ++ graph.neighborVertices(v)))
      }
      neighbors(v)
    }

  }

  trait SimpleSlimGraphLike[N, E <: SimpleEdgeLike[N]] {

    object InOutEdges {
      def empty() = {
        InOutEdges(Map.empty[N, E], Map.empty[N, E])
      }
    }
    case class InOutEdges(incoming: Map[N, E], outgoing: Map[N, E])

    type EdgeMap = InOutEdges
    val adjList = Map.empty[N, EdgeMap]
    val edgeSet = Set.empty[E]

    def size() = edgeSet.size

    def vertices = adjList.keys

    def hasEdge(e: E) = { edgeSet.contains(e) }

    def order() = adjList.size

    def edges = { edgeSet }

    def isDirected(): Boolean

    def insertEdge(e: E) = {
      if (!edgeSet.contains(e)) {
        edgeSet += e
        adjList.getOrElseUpdate(e.source, InOutEdges.empty).outgoing += (e.target -> e)
        adjList.getOrElseUpdate(e.target, InOutEdges.empty).incoming += (e.source -> e)
        true
      } else {
        false
      }
    }

    def removeEdge(e: E) = {
      if (edgeSet.contains(e)) {
        edgeSet -= e
        adjList(e.source).outgoing -= e.target
        adjList(e.target).incoming -= e.source
        true
      } else {
        false
      }
    }

    def insertVertex(u: N) = {
      if (!adjList.contains(u)) {
        adjList(u) = InOutEdges.empty
        true
      } else {
        false
      }
    }

    def removeVertex(u: N) = {
      if (adjList.contains(u)) {
        adjList(u).outgoing.foreach {
          case (v, e) =>
            adjList(v).incoming -= u
            edges -= e
        }
        adjList(u).incoming.foreach {
          case (v, e) =>
            adjList(v).outgoing -= u
            edges -= e
        }
        adjList -= u
        true
      } else {
        false
      }
    }

    def neighborEdges(u: N) = { (adjList(u).incoming.valuesIterator ++ adjList(u).outgoing.valuesIterator) }

    def neighborVertices(u: N) = { (adjList(u).incoming.keysIterator ++ adjList(u).outgoing.keysIterator) }

    def neighbors(u: N) = { (adjList(u).incoming.iterator ++ adjList(u).outgoing.iterator) }

    def hasEdge(u: N, v: N): Boolean

    def edgeOption(u: N, v: N): Option[E]

  }

  class SlimGraphUnDirected[N <% Ordered[N], E <: SimpleEdgeLike[N]]() extends SimpleSlimGraphLike[N, E] {

    def isDirected() = { false }

    def copy = {
      val g = SlimGraphUnDirected[N, E]()
      vertices.foreach { u => g.insertVertex(u) }
      edges.foreach { e => g.insertEdge(e) }
      g
    }

    def hasEdge(u: N, v: N) = {
      if (adjList.contains(u)) {
        if (u < v) { adjList(u).outgoing.contains(v) } else { adjList(u).incoming.contains(v) }
      } else { false }
    }

    def edgeOption(u: N, v: N): Option[E] = {
      if (adjList.contains(u)) {
        if (u < v) {
          adjList(u).outgoing.lift(v)
        } else {
          adjList(u).incoming.lift(v)
        }
      } else {
        Option.empty[E]
      }
    }

    /*
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
  */
    /*
  def subgraph(nodeSubset: Set[N]) = {
    val subg = SlimGraph[N, E]()

    nodeSubset.foreach { u=>
      adjList(u).foreach{ v =>
        if (nodeSubset.contains(v)) { subg.insertEdge(u,v) }
      }
    }

   subg
  }
  */

    /*
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
  */
    /**
     * def completesTriangle(v: N, w: N): Boolean = {
     * adjList.foreach{ case(u, s) =>
     * if (s.contains(v) && s.contains(w)) { return true }
     * }
     * return false
     * }
     */

  }

  class SlimGraphDirected[N <% Ordered[N], E <: SimpleEdgeLike[N]]() extends SimpleSlimGraphLike[N, E] {

    def isDirected() = { true }

    def hasEdge(u: N, v: N) = { adjList.contains(u) && adjList(u).outgoing.contains(v) }

    def edgeOption(u: N, v: N): Option[E] = {
      if (adjList.contains(u) && adjList(u).outgoing.contains(v)) {
        Some(adjList(u).outgoing(v))
      } else {
        Option.empty[E]
      }
    }

    def copy = {
      val g = new SlimGraphDirected[N, E]()
      vertices.foreach { u => g.insertVertex(u) }
      edges.foreach { e => g.insertEdge(e) }
      g
    }

    def incomingEdges(u: N) = { adjList(u).incoming.valuesIterator }
    def outgoingEdges(u: N) = { adjList(u).outgoing.valuesIterator }
    def predecessors(u: N) = { adjList(u).incoming.keysIterator }
    def successors(u: N) = { adjList(u).outgoing.keysIterator }

    /**
     * Return an array of the vertices in a topologically sorted order;
     * raises an exception if the graph is cyclic or undirected
     *
     * DFS based on the wikipedia article at http://en.wikipedia.org/wiki/Topological_sorting
     */
    def topologicalOrdering(): ArrayBuffer[N] = {
      val L = new ArrayBuffer[N](order)
      val visited = HashSet.empty[N]

      // All nodes with no outgoing edges
      val S = vertices.filter { n => successors(n).isEmpty }
      S.foreach { n => visit(n) }

      def visit(u: N) {
        if (!visited.contains(u)) {
          visited += u
          predecessors(u).foreach { v => visit(v) }
          L += u
        }
      }
      // We have to have everything in L
      assert(L.size == vertices.size, "Size of topo order list is %d, but there are %d nodes".format(L.size, vertices.size))
      L
    }

  }

  object EdgeGraphAlgorithms {

    def triangles[N, E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((N, N, N)) => Boolean = (t: (N, N, N)) => true) = {
      val tris = ArrayBuffer[(N, N, N)]()
      val graphc = graph.copy
      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      graphc.vertices.foreach { v =>
        val vn = graphc.neighborEdges(v)

        vn.foreach { e =>
          graphc.removeEdge(e)
          val w = otherVertex(v, e)
          val wn = graphc.neighborVertices(w).toSet
          val vnew = graphc.neighborVertices(v).toSet
          (vnew & wn).foreach { x =>
            val t = ((v, w, x))
            if (f(t)) { tris += ((v, w, x)) }
          }

        }

      }
      tris
    }

    def trianglesByEdge[N, E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((E, E, E)) => Boolean = (t: (E, E, E)) => true) = {

      val tris = ArrayBuffer[(E, E, E)]()
      val graphc = graph.copy

      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      while (graphc.edges.size > 0) {
        val e1 = graphc.edges.head
        graphc.removeEdge(e1)
        val u = e1.source
        val v = e1.target
        var neigh = graphc.neighbors(u) //neighborEdges(u)
        neigh.foreach {
          case (w, e2) =>
            val e3 = graphc.edgeOption(v, w)
            if (e3.isDefined) {
              val t = ((e1, e2, e3.get))
              if (f(t)) { tris += (t) }
            }
        }
      }
      tris
    }

    def fastTriangles[N <% Ordered[N], E <: SimpleEdgeLike[N]](
      graph: SlimGraphUnDirected[N, E],
      f: ((N, N, N)) => Boolean = (t: (N, N, N)) => true) = {
      val tris = ArrayBuffer[(N, N, N)]()
      val graphc = new ObservableSlimGraphUndirected[N, E]()
      graph.edges.foreach { e => graphc.insertEdge(e) }
      graph.vertices.foreach { n => graphc.insertVertex(n) }
      val nc = new NeighborCache[N, E, graphc.type](graphc)

      def otherVertex(u: N, e: E) = {
        if (e.source == u) { e.target } else { e.source }
      }

      graphc.vertices.foreach { v =>
        val vn = graphc.neighborEdges(v)
        vn.foreach { e =>
          graphc.removeEdge(e)
          val w = otherVertex(v, e)
          val wn = nc.neighborVertexSet(w)
          val vnew = nc.neighborVertexSet(v)

          (vnew & wn).foreach { x =>
            val t = ((v, w, x))
            if (f(t)) { tris += ((v, w, x)) }
          }

        }

      }
      tris
    }
    /*
  def undirect[N <% Ordered[N]](digraph: SlimGraph[N]) = {
    val graph = SlimGraph[N]()
    digraph.edges.foreach{ case(v,w) => graph.insertEdge(v,w,false) }
    graph
  }
  */
  }

  object MMSEdge {
    def fromLine(s: String) = {
      val toks = s.split("""\s+""")
      new MMSEdge(toks(0), toks(1), toks(2).toDouble, toks(3).toDouble)
    }
  }

  class MMSEdge(u: String, v: String, val weight: Double, val freq: Double) extends UnorderedEdgeLike[String] {
    val (source, target) = if (u < v) { (u, v) } else { (v, u) }
  }

  object MMSGraphReader {

    def fromFile(fname: String) = {
      val fsrc = Source.fromFile(fname)
      val graph = new SlimGraphUnDirected[String, MMSEdge]()

      fsrc.getLines.foreach { l =>
        graph.insertEdge(MMSEdge.fromLine(l))
      }
      graph
    }

  }

  package object GraphReader {
    implicit val asInt = (s: String) => { augmentString(s).toInt }
    implicit val asLong = (s: String) => { augmentString(s).toLong }
    implicit val asFloat = (s: String) => { augmentString(s).toFloat }
    implicit val asDouble = (s: String) => { augmentString(s).toDouble }
  }
  package GraphReader {

    trait DefaultReader {

      def _readFromFile[E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String) => E): Boolean

      def fromFile(
        fname: String,
        efact: (String, String) => UndirectedEdge[String]) = {
        val g = new SlimGraphUnDirected[String, UndirectedEdge[String]]()
        _readFromFile(fname, g, UndirectedEdge.factory[String])
        g
      }

      def fromFile(
        fname: String,
        efact: (String, String) => DirectedEdge[String]) = {
        val g = new SlimGraphDirected[String, DirectedEdge[String]]()
        _readFromFile(fname, g, DirectedEdge.factory[String])
        g
      }

      def fromFile[T](
        fname: String,
        efact: (String, String, T) => WeightedUndirectedEdge[String, T],
        weight: T) = {
        val g = new SlimGraphUnDirected[String, WeightedUndirectedEdge[String, T]]()
        _readFromFile(fname, g, efact(_: String, _: String, weight))
        g
      }

      def fromFile[T](
        fname: String,
        efact: (String, String, T) => WeightedDirectedEdge[String, T],
        weight: T) = {
        val g = new SlimGraphDirected[String, WeightedDirectedEdge[String, T]]()
        _readFromFile(fname, g, efact(_: String, _: String, weight)) //WeightedDirectedEdge.factoryWithDefault[String, Double](weight))
        g
      }

    }

    trait WeightedReader extends DefaultReader {
      def _readFromFile[T, E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String, T) => E)(implicit weightFun: (String) => T): Boolean

      def fromFile[T](
        fname: String,
        efact: (String, String, T) => WeightedDirectedEdge[String, T])(implicit weightFun: (String) => T) = {
        val g = new SlimGraphDirected[String, WeightedDirectedEdge[String, T]]()
        _readFromFile(fname, g, WeightedDirectedEdge.factory[String, T])
        g
      }

      def fromFile[T](
        fname: String,
        efact: (String, String, T) => WeightedUndirectedEdge[String, T])(implicit weightFun: (String) => T) = {
        val g = new SlimGraphUnDirected[String, WeightedUndirectedEdge[String, T]]()
        _readFromFile(fname, g, WeightedUndirectedEdge.factory[String, T])
        g
      }
    }

    object EdgeListReader extends DefaultReader with WeightedReader {
      def _readFromFile[E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String) => E) = {

        val fsrc = Source.fromFile(fname)
        val directed = graph.isDirected

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val e = efact(toks(0), toks(1))
            graph.insertEdge(e)
          }
        }
        true
      }

      def _readFromFile[T, E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String, T) => E)(implicit weightFun: (String) => T) = {

        val fsrc = Source.fromFile(fname)
        val directed = graph.isDirected

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            //val e = efact(toks(0), toks(1), toks(2))
            graph.insertEdge(efact(toks(0), toks(1), weightFun(toks(2))))
          }
        }
        true
      }

    }

    object AdjListReader extends DefaultReader {
      def _readFromFile[E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String) => E) = {

        val fsrc = Source.fromFile(fname)

        fsrc.getLines.foreach { l =>
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val u = toks.head
            val vList = toks.tail
            graph.insertVertex(u)
            vList.foreach { v => graph.insertEdge(efact(u, v)) }
          }
        }
        true
      }

    }

    object MultiAdjListReader extends DefaultReader with WeightedReader {
      def _readFromFile[T, E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String, T) => E)(implicit weightFun: (String) => T) = {

        val lineIt = Source.fromFile(fname).getLines

        def addNeighbors(u: String, n: Int) {
          (0 until n).foreach { i =>
            val l = lineIt.next
            val toks = l.split("""\s+""").toList
            val v = toks(0)
            //val weight = toks(1).toDouble 
            graph.insertEdge(efact(u, v, weightFun(toks(1))))
          }
        }

        while (lineIt.hasNext) {
          val l = lineIt.next
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks: List[String] = l.split("""\s+""").toList
            val (u, numVerts) = (toks(0), asInt(toks(1)))
            graph.insertVertex(u)
            addNeighbors(u, numVerts)
          }
        }
        true
      }

      def _readFromFile[E <: SimpleEdgeLike[String], G <: SimpleSlimGraphLike[String, E]](
        fname: String,
        graph: G,
        efact: (String, String) => E) = {

        val lineIt = Source.fromFile(fname).getLines

        def addNeighbors(u: String, n: Int) {
          (0 until n).foreach { i =>
            val l = lineIt.next
            val toks = l.split("""\s+""").toList
            val v = toks(0)
            //val weight = toks(1).toDouble 
            graph.insertEdge(efact(u, v))
          }
        }

        while (lineIt.hasNext) {
          val l = lineIt.next
          if (!l.trim.startsWith("#")) { // ignore comments
            val toks = l.split("""\s+""").toList
            val (u, numVerts) = (toks(0), asInt(toks(1)))
            graph.insertVertex(u)
            addNeighbors(u, numVerts)
          }
        }
        true
      }

    }
  }

}