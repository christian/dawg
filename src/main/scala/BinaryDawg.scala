import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class BinaryDawg(filePath: String) {

  val END_OF_BRANCH = 1 // specify that branching has finished
  val END_OF_WORD = 2 // specify that word has finished

  // Note that there might be an overhead memory wise with this approach.
  // Although, these would be allocated on demand
  case class BinNode(letter: Char, extraFlag: Int, ptr: Int) {

    def children(): List[BinNode] = {
      var tmpChildren: ListBuffer[BinNode] = ListBuffer()
      var shouldStop = false // extraFlag == END_OF_BRANCH

      if (ptr == 0 && extraFlag != 0)
        shouldStop = true // leaf nodes that have no children; their child points by default to root

      var i = 0

      while (!shouldStop) {
        val arrOfBytes = data.slice(ptr * 4 + i * 4, ptr * 4 + i * 4 + 4) // data.slice(i * 4, i * 4 + 4)
        val theInt = ByteBuffer.wrap(arrOfBytes).getInt
        val (p, e, l) = fromInt(theInt) // pointer, extraFlag, letter
        val child = BinNode(l, e, p)
        tmpChildren += child
        shouldStop = e != 0 // either END_OF_BRANCH, or END_OF_WORD or both
        i += 1
      }

      tmpChildren.toList
    }

  }

  val path: Path = Paths.get(filePath)
  val data: Array[Byte] = Files.readAllBytes(path)
  val root: BinNode = BinNode('-', 0, 0)

  def findLastNodeInPrefix(prefix: String, node: BinNode): Option[BinNode] = {
    if (prefix == "") {
      Some(node)
    } else {
      var foundNode: Option[BinNode] = None
      for (child <- node.children()) {
        if (child.letter == prefix.head) {
          foundNode = findLastNodeInPrefix(prefix.tail, child)
        }
      }
      foundNode
    }
  }

  var visited: ListBuffer[BinNode] = mutable.ListBuffer()
  def getSufixes(node: BinNode, tmpSuffix: String, acc: List[String]): List[String] = {
    visited += node
    val children: List[BinNode] = node.children()
    if (children.isEmpty) {
      tmpSuffix.toString :: acc
    } else {
      children.filter(child => !visited.contains(child)).flatMap { child =>
        getSufixes(child, tmpSuffix + child.letter, acc)
      }
    }
  }

  def suggest(prefix: String): List[String] = {
    val node = findLastNodeInPrefix(prefix, root)
    node match {
      case Some(n) => getSufixes(n, "", List()).map { prefix + _}
      case None => List()
    }
  }

  def lookup(word: String): Boolean = { search(word, root) }

  // FIXME at the moment, it returns also true if the prefix exists in the tree
  def search(word: String, node: BinNode): Boolean = {
    if (word == "") { // word was "consumed"
      true
    } else {
      node.children().foldLeft(false) { (acc, child) =>
        if (child.letter == word.head)
          acc || search(word.tail, child)
        else
          acc
      }
    }
  }

  // FIXME at the moment, it returns also true if the prefix exists in the tree
  // Note: this is sort of a BFS
  //  private def search(word: String, posCurrentLetter: Int, seek: Int): Boolean = {
  //    if (posCurrentLetter >= word.length) { // all letters have been tested
  //      true // word was found
  //    } else {
  //      // create an Int from first 4 bytes
  //      val arrOfBytes = data.slice(seek * 4, seek * 4 + 4)
  //      val theInt = ByteBuffer.wrap(arrOfBytes).getInt // Big Endian
  //      val (ptr, extraFlag, letter) = fromInt(theInt)
  //
  //      if (letter == word(posCurrentLetter)) {
  //        search(word, posCurrentLetter + 1, ptr) // move on to next letter
  //      } else {
  //        var shouldStop = extraFlag != 0
  //        var i = 1
  //        while (!shouldStop) { // e.g has end-of-word been reached?
  //
  //          val arrOfBytes = data.slice(seek * 4 + i * 4, seek * 4 + i * 4 + 4)
  //          val theInt = ByteBuffer.wrap(arrOfBytes).getInt
  //          val (ptr, extraFlag, letter) = fromInt(theInt)
  //
  //          if (letter == word(posCurrentLetter)) {
  //            return search(word, posCurrentLetter + 1, ptr)
  //          } else {
  //            i += 1
  //            shouldStop = extraFlag != 0
  //          }
  //        }
  //        false // shouldStop is true at this point and the letter was not found on any branch
  //      }
  //    }
  //
  //  }

  def suggest() = {}

  /**
   * Decode Int to tuple (ptr, extraFlag, letter).
   * @param value the int
   * @return the tuple
   */
  def fromInt(value: Int): (Int, Int, Char) = {
    val ptr =       (0xFFFF0000 & value) >> 16
    val extraFlag = (0x00000F00 & value) >> 8
    val letter =     0x000000FF & value
    (ptr, extraFlag, letter.toChar)
  }
}
