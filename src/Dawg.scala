import java.io.FileOutputStream
import java.nio.{Buffer, ByteBuffer}
import java.nio.file.{Files, Path, Paths}

import scala.StringBuilder
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.reflect.io.File

class Node {
  val id = Id.gen
  var isFinal = false
  var edges: Map[Char, Node] = Map()  // One node can have multiple external edges

  var visited = false // Needed for traversing DFS

  override def toString = {
    edges.toString()
  }
}

class MyHashMap extends scala.collection.mutable.HashMap[Node, Node] {
  protected override def elemEquals(node1: Node, node2: Node): Boolean = {
    node1.toString() == node2.toString()
  }
}

class Dawg {
  val root = new Node()
  var previousWord = ""

  // Nodes that are not yet checked
  var uncheckedNodes: Array[(Node, Char, Node)] = Array()

  // Unique nodes that have been checked for duplication.
  // The string represents the path to the Node.
  var minimizedNodes = new mutable.HashMap[String, Node]()

  val END_OF_BRANCH = 1 // specify that branching has finished
  val END_OF_WORD = 2 // specify that word has finished

  /**
    * @return Number of nodes in the DAWG
    */
  lazy val nodeCount = minimizedNodes.size

  /**
    * @return Number of edges in the DAWG
    */
  lazy val edgeCount = edges().size

  /**
    * Insert the word into the DAWG.
    * @param word the word to be inserted.
    */
  def insert(word: String) = {
    if (word < previousWord) throw new RuntimeException(s"Words are not in lexical order: $word $previousWord")

    val commonPrefixSize = commonPrefix(word, previousWord)

    minimize(commonPrefixSize)

    // Add the sufix, starting from the correct node, mid-way through the graph
    // At this point uncheckedNodes ...
    var node = if (uncheckedNodes.isEmpty)
      root
    else
      uncheckedNodes.last._3

    word.drop(commonPrefixSize).foreach { letter =>
      val nextNode = new Node()
      node.edges += (letter -> nextNode)
      uncheckedNodes = uncheckedNodes :+ ((node, letter, nextNode))
      node = nextNode
    }

    node.isFinal = true
    previousWord = word
  }

  /**
    * Goes from the last letter in the word down to the one at position commonPrefixSize.
    * @param commonPrefixSize the position of where the words begin to have different letters
    */
  private def minimize(commonPrefixSize: Int) = {
    for (i <- uncheckedNodes.length - 1 until commonPrefixSize - 1 by -1 ) {
      var (parent, letter, child) = uncheckedNodes(i)

      val key = child.edges.toString()
      if (minimizedNodes.contains(key)) {
        uncheckedNodes(i)._1.edges += (letter -> minimizedNodes.get(key).get)
      } else {
        // add the state to the minimized nodes
        minimizedNodes += (key -> child)
      }
      uncheckedNodes = uncheckedNodes.slice(0, i) // .pop (remove last element); this is very weird to be changed here
    }
  }

  /**
    * Return the position where the common prefix of word and previousWord ends.
    * @param word word
    * @param previousWord the other word
    * @return an int representing the position where the common prefix ends.
    */
  private def commonPrefix(word: String, previousWord: String): Int = {
    var commonPrefix = 0
    for (i <- 0 until Math.min(word.length, previousWord.length)) {
      if (word(i) != previousWord(i)) return commonPrefix
      commonPrefix += 1
    }
    commonPrefix
  }

  /**
    * Minimize all unchecked nodes. So that the last word is also minimized in the DAWG.
    */
  def finish() = {
    minimize(0)
  }

  /**
    * Check if a word exists in the DAWG.
    * @param word the word
    * @return true if the word exists, false otherwise
    */
  def lookup(word: String): Boolean = {
    var node = root
    word.foreach { letter =>
      node.edges.get(letter) match {
        case None => return false
        case Some(found) => node = node.edges.get(letter).get
      }
    }
    node.isFinal
  }

  /**
    * Suggest some words based on the input.
    */
  def suggest(word: String) = {

  }

  /**
    * Load a dawg from a file.
    * @param filePath file to load the dawg from
    */
  def load(filePath: String) = {
    for (line <- Source.fromFile(filePath).getLines) {
      // println(line)
      insert(line)
    }
    finish()
  }

  /**
    * Helper Class
    * @param extPos external Position (see the toArray function)
    * @param startNode start node id
    * @param endNode end node id
    * @param letter which letter we have on the edge from start node to end node
    * @param extraFlag END_OF_BRANCH = this node is the last child of it's parent's children,
    *                  END_OF_WORD = end of word
    *                  END_OF_WORD | END_OF_BRANCH = both last children and end of word
    */
  case class Edge(extPos: Int, startNode: Int, endNode: Int, letter: Char, extraFlag: Int)

  /**
    * Return the edges in the Dawg.
    */
  def edges(): List[Edge] = {
    var edgeList: List[Edge] = List()
    val queue: collection.mutable.Queue[Node] = collection.mutable.Queue()
    var vis: mutable.Map[Int, Boolean] = mutable.Map[Int, Boolean]()
    vis += (root.id -> true)
    queue.enqueue(root)
    var pos = 0
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      node.edges.zipWithIndex.foreach { case (child, id) =>

        var extraFlag = 0
        if (id == node.edges.size - 1) extraFlag = extraFlag | END_OF_BRANCH
        if (child._2.isFinal) extraFlag = extraFlag | END_OF_WORD

        edgeList = edgeList :+ Edge(pos, node.id, child._2.id, child._1, extraFlag)
        pos += 1

        if (!vis.contains(child._2.id)) {
          queue.enqueue(child._2)
          vis += (child._2.id -> true)
        }
      }
    }
    edgeList
  }

  /**
    * Make a DOT representation of the DAWG.
    * @return a string representation of the DAWG.
    */
  def toDot: String = {
    var nodesStr = ""
    var edgesStr = ""

    val queue: collection.mutable.Queue[Node] = collection.mutable.Queue()
    queue.enqueue(root)
    root.visited = true
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      nodesStr += s"""${node.id} [label="${node.id}" ${if (node.isFinal) "shape=\"doublecircle\"" else ""} ]\n"""
      node.edges.foreach { case (letter, childNode) =>

        // Store connection for later; they will appear after all the nodes are defined
        edgesStr += s"""${node.id} -> ${childNode.id} [label="$letter"]\n"""

        if (!childNode.visited) {
          queue.enqueue(childNode)
          childNode.visited = true
        }
      }

    }
    "digraph graphname {" + nodesStr + edgesStr + "}"
  }

  /**
    * Array representation of the digraph as an Array of tuples (Int, Int, Char), where:
    *
    *  ._1 - external position
    *  ._2 - start node id
    *  ._3 - end node id
    *
    * E.g.
    *
    *    0  1  2   3   4   --> external position
    *    \  |  /   |   |
    *       0      1   2   --> start node id (parent)
    *    /  |  \   |   |
    *    1  3  6   2   0   --> end node id (child)
    *
    * 0 on the last row means end of word.
    * @return a List of Edges.
    */
  def toArray: Array[(Int, Int, Char)] = {
    val edgeList = edges()

    val arr: Array[(Int, Int, Char)] = Array.fill(edgeList.size) { (0, 0, 0) }

    def find(n: Int, edgeMap: List[Edge]): Int =
      edgeMap.find(edge => edge.startNode == n).map{_.extPos}.getOrElse(0) // potentially inefficient
    edgeList.foreach { case edge =>
      arr.update(edge.extPos, (find(edge.endNode, edgeList), edge.extraFlag, edge.letter))
    }

    arr
  }

  /**
    * Compressed representation of the toArray representation (using Ints).
    * Each edge is describe by a 4 byte word (Int). Reference: http://www.wutka.com/dawg.html
    *
    * 4 - bytes - reference to the next word
    * 2 - bytes - extra Flag
    * 1 - byte - the letter
    *
    * @param arr the array representation of the DAWG (obtained using the toArray function)
    */
  def toInts(arr: Array[(Int, Int, Char)]): Array[Int] = {
    arr.map { case (ptr: Int, extraFlag: Int, letter: Char) =>  // TODO strange that I have to use case here
      toInt(ptr, extraFlag, letter)
    }
  }

  /**
    * Encode tuple (ptr, extraFlag, letter) to Int.
    * @param ptr pointer to next node
    * @param extraFlag extraFlag
    * @param letter letter
    * @return the encoded Int
    */
  def toInt(ptr: Int, extraFlag: Int, letter: Char) = {
    var a = 0x00000000
    a = a | (ptr << 16)      // 0x00320000
    a = a | (extraFlag << 8) // 0x00000100
    a = a | letter           // 0x00000063
    a
  }


  /**
    * Write the array to binary file.
    * @param filePath where to write the data
    */
  def writeToBinaryFile(arr: Array[Int], filePath: String): Unit = {
    try {
      val path = Paths.get(filePath)
      println(s"Writing binary data to file $filePath. Array length ${arr.length}")

      val b = ByteBuffer.allocate(4 * arr.length)
      arr.foreach {x => b.putInt(x) }

      Files.write(path, b.array())
      println(s"Wrote ${4 * arr.length} bytes")
    } catch {
      case x: Exception => println("Wow exception ")
        x.printStackTrace()
    }
  }
}

// Should be in Swift
case class BinaryDawg(filePath: String) {

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

object DawgTest {

  def createDawg() = {
    val dawg = new Dawg()

    val start = System.currentTimeMillis()
    dawg.load("data/input.txt")
    // dawg.load("data/words.100")

    val end = System.currentTimeMillis()
    println(s"Loaded in ${end - start}ms")

    File("data/dawg.dot").writeAll(dawg.toDot)

    println("Number of nodes: " + dawg.nodeCount + " (root not counted)")
    println("Number of edges: " + dawg.edgeCount)

    val array = dawg.toArray
    array.foreach(println(_))

    val compressedInts = dawg.toInts(array)

    dawg.writeToBinaryFile(compressedInts, "data/words.dwg")
  }

  def main(args: Array[String]): Unit = {

    createDawg()

    println("Loading the dawg")
    val binDawg = BinaryDawg("data/words.dwg")

    println("Searching")
    List("cats", "cale", "frig", "draw", "craw", "brown").foreach { word =>
      println(s"$word ${binDawg.lookup(word)}")
    }

    println("Suggesting")
    println(binDawg.suggest("ca"))

  }

}