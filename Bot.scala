
class ControlFunctionFactory {
  def create = new Bot().respond _
}

case class Direction(val x:Int, val y:Int) {
  override def toString = "direction=" + x + ":" + y
}

class Bot {
  def respond(input: String) = {
    //"Status(text=Hello World)"
    println(CommandParser(input).toString)
    //log(CommandParser(input).toString)
    move(Direction(1,1))
  }

  def log(str:String) = "Log(text=" + str + ")"

  def move(x:Int, y:Int):String = "Move(direction=" + x + ":" + y + ")"
  def move(dir:Direction):String = dir match {
    case Direction(x,y) => move(x,y)
  }

  def spawn(dir:Direction, name:String, energy:Int) = "Spawn(" + dir.toString + ",name=" + name + ",energy=" + energy + ")"
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
