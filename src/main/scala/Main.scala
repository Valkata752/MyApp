import org.json4s._
import org.json4s.jackson.Serialization.{read, write, writePretty}

import java.io.{File, PrintWriter}
import scala.io.Source

trait Animal

case class Wolf(character: Map[String, String]) extends Animal

case class Elephant(character: Map[String, String]) extends Animal

case class Rhino(character: Map[String, String]) extends Animal

case class Lion(character: Map[String, String]) extends Animal

case class DefaultAnimal(name: String, character: Map[String, String]) extends Animal

// You can do it that way, but you need to know all the features of the animal and insert a default value in case there isn't
// any data inside the expected feature place
case class Monkey(jumps: String, dangerous: String) extends Animal

object Main extends App with Animal {
  var listCC: Map[String, Animal] = Map.empty

  implicit val formats: DefaultFormats.type = DefaultFormats

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

      lines.foreach(line => lineMatcher(line))
      // lines.foreach(line => lineMatcher(line.split(":").head))

      // After everything is done the stream MUST be closed to prevent memory leakage
      // and let the processes unlock the file
      lineParser.close()
    } catch {
      // Handling different exceptions and printing reason
      case e: Exception => println("Error during proccess" + e.getMessage)
    }
  }

  // Do something when a match is found
  def lineMatcher(line: String): Unit = {
    val nameParser = "(\\w+):(.+)\\s?".r
    // Find the first element which is actually the name of the animal
    // as well as the first one of its characteristics and pass it onto the second regex to match
    // the next ones
    val nameCharacterMap = nameParser.findFirstMatchIn(line).map(m => (m.group(1).trim(), m.group(2).trim()))

    val characterParser = "(\\w+)=([\\w ]+)(,\\s)?".r
    // Find all the elements matching the characteristics regex and put them inside a Map collection
    val characterMap = characterParser.findAllMatchIn(nameCharacterMap.get._2).map(m => (m.group(1).trim(), m.group(2).trim())).toMap
    val animalMap: (String, Map[String, String]) = nameCharacterMap.get._1 -> characterMap

    //    addToList(animalMap)
    createCaseClass(animalMap)

    someChecker(animalMap)

    /*
    This Regular Expression usage has quite a specific use because all the lines must be formatted exactly alike
    otherwise in case of a parameter not in place it won't be recognized as valid and will throw an exception

        val patternWolf = "^Wolf:sound=([a-zA-Z]*),runSpeed=([a-zA-Z]*),dangerous=([a-zA-Z]*)$".r
        val patternElephant = "^Elephant:name=([a-zA-Z]*),sound=([a-zA-Z]*),runSpeed=([a-zA-Z]*),dangerous=([a-zA-Z]*),size=([a-zA-Z]*)$".r
        val patternRhino = "^Rhino:name=([a-zA-Z]*),runSpeed=([a-zA-Z]*),dangerous=([a-zA-Z]*),sound=([a-zA-Z]*),misc=([a-z A-Z]*)$".r
        val patternLion = "^Lion:name=([a-zAZ]*),runSpeed=([a-zA-Z]*),dangerous=([a-zA-Z]*),size=([a-zA-Z]*)$".r
        val patternMonkey = "^Monkey:name=([a-zA-Z]*),jumps=([a-zA-Z]*),dangerous=([a-zA-Z]*)$".r
     */

    //    line match {
    //
    //      case patternWolf(name, sound, runspeed, dangerous) =>
    //        val wolfInst = Wolf(sound = sound, runSpeed = runspeed, dangerous = dangerous)
    //        addToList(Wolf.getClass.getSimpleName -> wolfInst)
    //
    //      case patternElephant(name, sound, runspeed, dangerous, size) =>
    //        val elInst = Elephant(name = name, sound = sound, runSpeed = runspeed, dangerous = dangerous, size = size)
    //        addToList(Map(elInst.getClass.getName -> elInst))
    //
    //      case patternRhino(name, runSpeed, dangerous, sound, misc) =>
    //        val rhInst = Rhino(name = name, runSpeed = runSpeed, dangerous = dangerous, sound = sound, misc = misc)
    //        addToList(Map(rhInst.getClass.getName -> rhInst))
    //
    //      case patternLion(name, runspeed, dangerous, size) =>
    //        val lionInst = Lion(name = name, runSpeed = runspeed, dangerous = dangerous, size = size)
    //        addToList(Map(lionInst.getClass.getName -> lionInst))
    //
    //      case patternMonkey(name, jumps, dangerous) =>
    //        val monkeyInst = Monkey(name = name, jumps = jumps, dangerous = dangerous)
    //        addToList(monkeyInst.getClass.getName -> monkeyInst))
    //      case _ => println("No parser found for: " + line);
    //    }
    //
    //     */
  }

  LineParser("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\animals.txt")

  // Printing result from writing to file JSON.txt in the form of a bool -
  // true at success and false at failure
  writeJSON(listCC)
  foldList(List(1, 2, 3, 4, 5))
//  readJSON("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\JSON.txt")

  def createCaseClass(caseClass2be: (String, Map[String, String])): Unit = {
    val it = caseClass2be._2
    caseClass2be._1 match {
      case "Lion" =>
        val lionInst = Lion(caseClass2be._2)
        listCC = listCC + (lionInst.getClass.getSimpleName -> lionInst)

      case "Rhino" =>
        val rhInst = Rhino(caseClass2be._2)
        listCC = listCC + (rhInst.getClass.getSimpleName -> rhInst)

      case "Wolf" =>
        val wolfInst = Wolf(caseClass2be._2)
        listCC = listCC + (wolfInst.getClass.getSimpleName -> wolfInst)

      case "Elephant" =>
        val elInst = Elephant(caseClass2be._2)
        listCC = listCC + (elInst.getClass.getSimpleName -> elInst)

      case "Monkey" =>
        val monkeyInst = Monkey(it.getOrElse("jumps", "Value not found"), it.getOrElse("dangerous", "Value not found"))
        listCC = listCC + (monkeyInst.getClass.getSimpleName -> monkeyInst)

      case _ =>
        val defInst = DefaultAnimal(caseClass2be._1, caseClass2be._2)
        listCC = listCC + (caseClass2be._1 -> defInst)
    }
  }

  //  def addToList(item: (String, Animal): Unit = {
  //    // Check if the list of animals is empty and assign the animal as an initial item in the map
  //    if (listCC.isEmpty) {
  ////      animalsList: Map[String, Map[String, String]] = Map.empty
  //      listCC = Map(item._1 -> item._2)
  //    } else {
  //      // or add it to the list ot map key -> values
  //      listCC = listCC + (item._1 -> item._2)
  //    }

  def writeJSON(list: Map[String, Animal]): Unit = {
    val jsonStr = writePretty("Animals" -> list)
    val writer = new PrintWriter(new File("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\JSON.txt"))
    writer.write(jsonStr)
    writer.close()
  }

  //  def readJSON(fileToRead: String): Unit = {
  //    val src = Source.fromFile("C:\\Users\\LENOVO\\IdeaProjects\\MyApp\\src\\main\\scala\\JSON.txt")
  //    src.close()
  //  }

  // Function for some actions with the maps
  def someChecker(animalMap: (String, Map[String, String])): Unit = {
    if (animalMap._2.contains("size")) {
      println("The " + animalMap._1 + " is " + animalMap._2("size"))
    } else {
      println("The " + animalMap._1 + " does not contain size")
    }
  }

  def readJSON(input: String): Unit = {
    val string = read("{\"Animals\":{\"Money\":{\"name\":\"Money\",\"character\":{\"sound\":\"nice\",\"value\":\"none\"}},\"Lion\":{\"character\":{\"runSpeed\":\"Quick\",\"dangerous\":\"Yes\",\"size\":\"Big\"}},\"Monkey\":{\"jumps\":\"Yes\",\"dangerous\":\"Maybe\"},\"Wolf\":{\"character\":{\"sound\":\"Howl\",\"runSpeed\":\"Quick\",\"dangerous\":\"Yes\"}},\"Elephant\":{\"character\":{\"sound\":\"Toot\",\"runSpeed\":\"Slow\",\"dangerous\":\"No\",\"size\":\"Big\"}},\"Rhino\":{\"character\":{\"runSpeed\":\"Slow\",\"dangerous\":\"Yes\",\"sound\":\"Unknown\",\"misc\":\"Has a nose\"}}}}").toString
  }

  def foldList(list: List[Int]) : Unit = {
    val sum = list.foldRight(0)((_ ,y) => y + 3)
    println(sum)
//    assert(sum == 15)
  }
}