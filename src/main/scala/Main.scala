import java.nio.file.{Files, Paths}

object Main extends App {

    val dawg = new Dawg()

    val start = System.currentTimeMillis()
    dawg.load("data/words.txt")

    val end = System.currentTimeMillis()
    println(s"Loaded in ${end - start}ms")

    Files.write(Paths.get("data/dawg.dot"), dawg.toDot.getBytes())

    println("Number of nodes: " + dawg.nodeCount + " (root not counted)")
    println("Number of edges: " + dawg.edgeCount)

    val array = dawg.toArray
    array.foreach(println(_))

    val compressedInts = dawg.toInts(array)

    dawg.writeToBinaryFile(compressedInts, "data/words.dwg")

    println("Loading the binary dawg")
    val binDawg = new BinaryDawg("data/words.dwg")

    println("Searching")
    List("cats", "cale", "frig", "draw", "craw", "brown").foreach { word =>
      println(s"$word ${binDawg.lookup(word)}")
    }

    println("Suggesting")
    println(binDawg.suggest("ca"))

}