import java.io.{FileNotFoundException, IOException}
import scala.io.Source

object Main extends App {

  def LineParser(sourceFile: String): Unit = {
    // Using try/catch minimizes the risk of unhandled exceptions and unexpected behaviour
    // in the application
    try {
      val lineParser = Source.fromFile(sourceFile)

      // The Source library gives access to the content of the file passed as a parameter
      val lines = lineParser.getLines().toList

      println("Number of elements:" + lines.length + "\nElements in the list:")

      // This can be done using lines.map as well if you don't need to print every line
      // on a separate row. Using regular expression we divide every element in the list
      // using a specific delimiting character

      lines.foreach(line => line.split(" " + "\n").foreach(println))

      // After everything is done the stream MUST be closed to prevent memory leakage
      // and let the processes unlock the file
      lineParser.close()
      LineMatcher()
    } catch {
      // Handling different exceptions
      case e: FileNotFoundException => println("Couldn't find that file!")
      case e: IOException => println("Had an IOException trying to read the file")
    }
  }

  def LineMatcher (line: String): Unit = {
    line match {
      case "abc" => // Do something
    }
  }

  LineParser("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\animals.txt")
}


case class Elephant(sound: String, runSpeed: String, dangerous: String, size: String) {

}

case class Wolf(sound: String, runSpeed: String, dangerous: String) {

}

case class Lion(runSpeed: String, dangerous: String, size: String) {

}

case class Monkey(jumps: String, dangerous: String) {

}

case class Rhino(runSpeed: String, dangerous: String, sound: String, misc: String) {

}