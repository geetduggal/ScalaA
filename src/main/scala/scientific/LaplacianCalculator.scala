package scalaa.scientific

import scala.math._
import scala.collection.JavaConversions._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MGraph}
import no.uib.cipr.matrix.sparse.{ FlexCompRowMatrix => SparseRowMat }
import scalala.tensor.dense._

class LaplacianCalculator[N](G: MGraph[N, UnDiEdge]) { 

  def compute : (SparseRowMat, DenseVector[Double]) = { 

    val N = G.order
    //println("Constructing Laplacian of size "+N+" x "+N)
    val diagWeights = DenseVector.zeros[Double](N)
    val wmat = new SparseRowMat(N,N)
    val laplacian = new SparseRowMat(N,N)
 
    val vertIDMap = G.nodes.zipWithIndex.toMap

    // First, we build the weight matrix W and the diagonal D
    // Iterate over each vertex (row)
    G.nodes.foreach{ vi =>
	    var dweight = 0.0
      val cidx = vertIDMap(vi)
	    // Iterate over v_i's adjacent edges
	    vi.neighbors.foreach{ wi =>
	      val (src, tgt) = (vi, wi)
	      val (i,j) = (vertIDMap(src), vertIDMap(tgt))
	      dweight += 1
	      if (i == cidx) { 
	        wmat.set(i, j, 1)
	      } else { 
	        wmat.set(j, i, 1) 
	      }
	    }

      diagWeights(cidx) = dweight
    }

    // Now, we can form L = I - D^{-1/2} * W * D^{-1/2}

    // Iterate over each vertex (row)
    G.nodes.foreach{ vi =>
	    val cidx = vertIDMap(vi)
	    var rowSum = 0.0
	    // Iterate over v_i's adjacent edges
	    vi.neighbors.foreach{ wi =>
	      val (src, tgt) = (vi, wi)
	      val (i, j) = (vertIDMap(src), vertIDMap(tgt))
	      val (di, dj) = (diagWeights(i), diagWeights(j))
	      var ow = 0.0
	      if (i == cidx) { 
	        ow = wmat.get(i,j) / (sqrt(di * dj))
	        laplacian.set(i, j, -ow)
	      } else { 
	        ow = wmat.get(j,i) / (sqrt(di * dj))
	        laplacian.set(j, i, -ow)
	      }
	    rowSum  += ow 
	}
      laplacian.set(cidx, cidx, 1.0)
    }

    return ( laplacian, diagWeights )
  }

}
