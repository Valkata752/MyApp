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

      // This prints every new line found

      // println("Number of elements:" + lines.length + "\nElements in the list:")
      // lines.foreach(line => line.split("\n").foreach(println))


      // Using regular expression we divide every element in the list
      // using a specific delimiting character and we pass it into the case class
      // where a match in animal name is found and process it

      lines.foreach(line => LineMatcher(line.split(":").head))

      // After everything is done the stream MUST be closed to prevent memory leakage
      // and let the processes unlock the file
      lineParser.close()
    } catch {
      // Handling different exceptions
      case e: FileNotFoundException => println("Couldn't find that file!")
      case e: IOException => println("Had an IOException trying to read the file")
    }
  }

  // Do something when a match is found
  def LineMatcher(line: String): Unit = {
    line match {
      case "Elephant" =>

        println("It's an elephant")

        //TODO Така се задават променливите така че да са в обект, а в случай с case class?

          Elephant.Set("Toot", "Slow", "Not really", "Very Big")

      case "Wolf" =>
        println("It's a Wolf")
        Wolf.Set("Howl", "Fast", "Yes")

      case "Lion" =>
        println("It's a lion")
        Lion.Set("Fast", "Yes", "Big")

      case "Rhino" =>
        println("It's a rhino")
        Rhino.Set("Slow", "Not really", "Thump", "A Big Animal")

      case "Monkey" =>
        println("It's a monkey")
        Monkey.Set(jumps = true, "no")

      case _ => println("Match not found")
    }
  }

  LineParser("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\animals.txt")

  Elephant.Get()
  Rhino.Get()
  Monkey.Get()
  Wolf.Get()
  Lion.Get()

}

// TODO В случая са 5 животни, а в случай, че са повече или има неназовано до момента такова, как ще се извлече?

object Elephant {
  var (soundC, rC, dC, sC) = ("", "", "", "")

  def Set(sound: String, runSpeed: String, dangerous: String, size: String): Unit = {
    soundC = sound
    rC = runSpeed
    dC = dangerous
    sC = size
  }

  def Get(): Unit = {
    println("The elephant is: " + soundC, rC, dC, sC)
  }
}

object Wolf {
  var (soundC, rC, dC) = ("", "", "")

  def Set(sound: String, runSpeed: String, dangerous: String): Unit = {
    soundC = sound
    rC = runSpeed
    dC = dangerous
  }

  def Get(): Unit = {
    println("The Wolf is: "  + rC, dC)
  }
}

object Lion {
  var (rC, dC, sC) = ("", "", "")

  def Set(runSpeed: String, dangerous: String, size: String): Unit = {
    rC = runSpeed
    dC = dangerous
    sC = size
  }

  def Get(): Unit = {
    println("The lion is: " + rC, dC, sC)
  }
}

object Monkey {
  var (jC, dC) = (false, "")

  def Set(jumps: Boolean, dangerous: String): Unit = {
    jC = jumps
    dC = dangerous
  }

  def Get(): Unit = {
    println("The monkey is: " + jC, dC)
  }
}

object Rhino {
  var (rC, dC, sC, mC) = ("", "", "", "")

  def Set(runSpeed: String, dangerous: String, sound: String, misc: String) : Unit = {
    rC = runSpeed
    dC = dangerous
    sC = sound
    mC = misc
  }

  def Get(): Unit = {
    println("The Rhino is: " + rC, dC, sC, mC)
  }
}