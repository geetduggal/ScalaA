package scalaa.utils.system

/** Procedures for storing/reading Serializable classes to/from disk */
object Marshall {
 
  /** Write a serializable object obj to a file fname */
  def write( fname: String, obj : Serializable ) {
    val fos = new java.io.FileOutputStream( fname )
    val oos = new java.io.ObjectOutputStream( fos )
    oos.writeObject( obj )
    oos.close()
  }

  /** Read into memory the serializable object of type T from file fname */
  def read[T <: Serializable]( fname: String ) = {
    val fis = new java.io.FileInputStream( fname )
    val ois = new java.io.ObjectInputStream(fis)
    val o = ois.readObject().asInstanceOf[T]
    ois.close()
    o
  }

}
