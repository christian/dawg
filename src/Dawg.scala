import scala.collection.mutable
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

  /**
    * Number of nodes in the DAWG
    * @return
    */
  def nodeCount = minimizedNodes.size

  def edgeCount = edges().size // TODO Freaking refactor

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


  case class Edge(extPos: Int, startNode: Int, endNode: Int, letter: Char)

  /**
    *
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
      node.edges.foreach { child =>
        edgeList = edgeList :+ Edge(pos, node.id, child._2.id, child._1)
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
    * Binary representation of the digraph.
    */
  def toBinary(filePath: String) = {
    val edgeList = edges()

    val arr: Array[(Int, Char)] = Array.fill(edgeList.size) { (0, 0) }

    def find(n: Int, edgeMap: List[Edge]): Int =
      edgeMap.find(edge => edge.startNode == n).map{_.extPos}.getOrElse(-1) // potentially ineficient

    edgeList.foreach { case edge =>
      arr.update(edge.extPos, (find(edge.endNode, edgeList), edge.letter))
    }

    arr
    println("hi")

  }
}

// Should be in Swift
object BinaryDawg {
  def init(filePath: String) = {}
  def lookup(): String = { "explanation" }
  def suggest() = {}
}

object DawgTest {

  def main(args: Array[String]): Unit = {
    val dawg = new Dawg()
    // dawg.load("data/words.sorted")

    val start = System.currentTimeMillis()
    dawg.load("data/input.txt")
    // dawg.load("data/input2.txt")
    val end = System.currentTimeMillis()

//    if (dawg.lookup("caj"))
//      println("it is there")
//    else
//      println("it is NOT there")
//
    println(s"Loaded in ${end - start}ms")

    File("data/dawg.dot").writeAll(dawg.toDot)

    println("Number of nodes: " + dawg.nodeCount + " (root not counted)")
    println("Number of edges: " + dawg.edgeCount)

    dawg.toBinary("bla")
  }

}