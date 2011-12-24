package scalaa.utils.system

/** Procedures for storing/reading Serializable classes to/from disk 
  *
  * @author Rob Patro
  * @author Geet Duggal
  */
object Marshall {
 
  /** Write a serializable object to a file
    *
    * @param obj The object to be written 
    * @param fname The name of the file to which the object should be written 
    */
  def write( fname: String, obj : Serializable ) {
    val fos = new java.io.FileOutputStream( fname )
    val oos = new java.io.ObjectOutputStream( fos )
    oos.writeObject( obj )
    oos.close()
  }

  /** Read a seralized object of the specified type into memory from file
    *
    * @param fname The name of the file from which to read the object
    * @tparam T The type of the serialized object to be read (must be provided explicitly)
    */
  def read[T <: Serializable]( fname: String ) = {
    val fis = new java.io.FileInputStream( fname )
    val ois = new java.io.ObjectInputStream(fis)
    val o = ois.readObject().asInstanceOf[T]
    ois.close()
    o
  }

}
