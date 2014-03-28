
class ControlFunctionFactory {
  def create = new Bot().respond _
}

case class Direction(val x:Int, val y:Int) {
  override def toString = "direction=" + x + ":" + y
}

case class Position(val x:Int, val y:Int) {
  override def toString = "position=" + x + ":" + y
}

case class BotProperty(val k:String, val v: String) {
  override def toString = k + "=" + v
}

//case class StateValue, 
//Set(key=value,...)

trait BotUtils {
  def log(str:String) = "Log(text=" + str + ")"
  def move(x:Int, y:Int):String = "Move(direction=" + x + ":" + y + ")"
  def move(dir:Direction):String = dir match {
    case Direction(x,y) => move(x,y)
  }

  val generationKey = "generation"
  val viewKey = "view"

  def spawn(dir:Direction, name:String, energy:Int) = "Spawn(" + dir.toString + ",name=" + name + ",energy=" + energy + ")"

  // haha abusing Set.toString to get Set(....)
  def setKV(kvs:Set[BotProperty]) = kvs.toString

  def explode(s:Int) = "Explode(" + s + ")"

  def say(s:String) = "Say(" + s + ")"

  def status(s:String) = "Status(" + s + ")"

  //MarkCell(position=int:int,color=string)
  def markcell(p:Position, color:String) = "MarkCell(" + p + "," + color + ")"
}

class Bot extends BotUtils {
  def respond(input: String) = {
    //"Status(text=Hello World)"
    val inputmap = CommandParser(input)

    //println("map=" + inputmap)
    //log(CommandParser(input).toString)

    inputmap match {
      case ("Welcome", _) => 
        val kvset = Set(BotProperty("one", "one!"), BotProperty("two", "two!"))
        println(setKV(kvset))
        ""

      case ("React", m:Map[String,String]) => 
        react(m)

      case (str, m:Map[String,String]) => 
        println("unknown opcode: " + str)
        move(Direction(-1,-1))
    }

  }

  def react(m:Map[String,String]) = {

    val generation = m.getOrElse(generationKey, "0").toInt

    // ugh
    if (generation > 0) {
      SlaveBot.react(m)
    } else {
      println("React " + {m get viewKey})
      move(Direction(1,1))
    }
  }

}

object SlaveBot extends BotUtils {
  def react(m:Map[String, String]) = {
    move(Direction(1,1))
  }
}

/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
    /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
    def apply(command: String): (String, Map[String, String]) = {
        /** "key=value" => ("key","value") */
        def splitParameterIntoKeyValue(param: String): (String, String) = {
            val segments = param.split('=')
            (segments(0), if(segments.length>=2) segments(1) else "")
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)
        val opcode = segments(0)
        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
        (opcode, keyValuePairs)
    }
}
