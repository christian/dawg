import scala.collection.mutable
import scala.io.Source

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

  /**
    * Insert the word into the DAWG.
    * @param word the word to be inserted.
    */
  def insert(word: String) = {
    if (word < previousWord) throw new RuntimeException("Words are not in lexical order")

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
    * Create a Map ( "NodeId-NodeId" -> Char ).
    */
  def letterIds(): Map[String, Char] = {
    var lettersIds = Map[String, Char]()
    val queue: collection.mutable.Queue[Node] = collection.mutable.Queue()
    var vis: mutable.Map[Int, Boolean] = mutable.Map[Int, Boolean]()
    vis += (root.id -> true)
    queue.enqueue(root)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      node.edges.foreach { child =>
        lettersIds += (s"${node.id}-${child._2.id}" -> child._1)
        if (!vis.contains(child._2.id)) {


          queue.enqueue(child._2)
          vis += (child._2.id -> true)
        }
      }
    }
    lettersIds
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
      nodesStr += s"""${node.id} [label="${node.id}" ${if (node.isFinal) "shape=\"doublecircle\""} ]\n"""
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
    val edges = letterIds()
    var output = Array.fill(edges.size) { (-1, -1, -1) }

    // NodeId -> (Child1, Child2, Child3)
    def fillChilds() = {
      var childs: Array[Map[Int, List[Int]]] = Array.fill(minimizedNodes.size + 1){ Map() }
      var vis: mutable.Map[Int, Boolean] = mutable.Map[Int, Boolean]() // nodes that have been visited; a sorted array could be enough
      var q: mutable.Queue[Node] = mutable.Queue()
      q.enqueue(root)
      vis += (root.id -> true)
      var i = 0
      while (q.nonEmpty) {
        val node = q.dequeue()
        childs.update(i, Map(node.id -> node.edges.map (_._2.id).toList ))
        i += 1
        node.edges.foreach { child =>
          if (!vis.contains(child._2.id)) {
            q.enqueue(child._2)
            vis += (child._2.id -> true)
          }
        }
      }
      childs
    }

    val neighbors: Array[Map[Int, List[Int]]] = fillChilds()
    neighbors.zipWithIndex.foreach { case (el: Map[Int, List[Int]], i: Int) =>
      val (parentId, value: List[Int]) = el.head
      value.foreach { childId =>
        val pos = neighbors.indexWhere { case m: Map[Int, List[Int]] => m.contains(childId) }

        val edge = s"$parentId-$childId"
        output.update(i, (pos, 0, edges.get(edge).get)) // There is an implicit conversion from char to int here
      }
    }

    output
    println("done")

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
    // daws.load("data/input.txt")
    dawg.load("data/input.txt")
    val end = System.currentTimeMillis()

//    if (dawg.lookup("caj"))
//      println("it is there")
//    else
//      println("it is NOT there")
//
    println(s"Loaded in ${end - start}ms")

//    println(dawg.toDot)
    println("Number of nodes: " + dawg.nodeCount + " (root not counted)")

    dawg.toBinary("bla")
  }

}