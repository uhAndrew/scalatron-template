
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

case class View(val viewStr:String) {
  // process viewStr here
  val sideLength = math.sqrt(viewStr.length).toInt
  val selfPos = Position(sideLength / 2, sideLength / 2)

  // where to go to get food
  def foodDirection:Direction = {
    Direction(-1,-1)
  }

  // don't go there??
  def dangerDirection = {
    Direction(0,0)
  }

  def otherBotDirection:Direction = {
    Direction(1,1)
  }

  def safeDirection = {
    Direction(0,0)
  }
}

//case class StateValue, 
//Set(key=value,...)

trait BotUtils {

  type inputMap = Map[String, String]

  val debug = false

  def log(str:String) = "Log(text=" + str + ")"
  def move(x:Int, y:Int):String = "Move(direction=" + x + ":" + y + ")"
  def move(dir:Direction):String = dir match {
    case Direction(x,y) => move(x,y)
  }

  val generationKey = "generation"
  val viewKey = "view"
  val energyKey = "energy"
  val spawnDelayKey = "spawndelay"

  def energySpawnMin = 400
  def spawnDelayTicks = 10

  def spawn(dir:Direction, name:String, energy:Int) = "Spawn(" + dir.toString + ",name=" + name + ",energy=" + energy + ")"

  def spawn(dir:Direction) = "Spawn(" + dir.toString + ")"

  // haha abusing Set.toString to get Set(....)
  def setKV(kvs:Set[BotProperty]) = kvs.toString

  def explode(s:Int) = "Explode(" + s + ")"

  def say(s:String) = "Say(" + s + ")"

  def status(s:String) = "Status(" + s + ")"

  //MarkCell(position=int:int,color=string)
  def markcell(p:Position, color:String) = "MarkCell(" + p + "," + color + ")"

  def react(m:inputMap, v:View):String

  def canSpawn(m:inputMap):Boolean = false

  def prependBar(s:String) = "|" + s

  def spawnDelay = setKV(Set(BotProperty(spawnDelayKey, spawnDelayTicks.toString)))

  def decSpawnDelay(m:inputMap) = {
    val ticks = m.getOrElse(spawnDelayKey, "1").toInt
    val decTicks = if (ticks > 0) ticks - 1 else ticks
    setKV(Set(BotProperty("spawndelay", decTicks.toString)))
  }

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

      case ("React", m:inputMap) => 
        val str = dispatchReact(m)
        if (debug) println(str)
        str

      case (str, m:inputMap) => 
        println("unknown opcode: " + str)
        move(Direction(-1,-1))
    }

  }

  override def canSpawn(m:inputMap) = {
    val energy = m.getOrElse(energyKey, "0").toInt
    val spawndelay = m.getOrElse(spawnDelayKey, "0").toInt
    energy > energySpawnMin && spawndelay == 0
  }

  def maybeSpawn(m:inputMap, v:View):String = {
    if (canSpawn(m)) {
      prependBar(spawn(v.otherBotDirection)) + prependBar(spawnDelay)
    } else {
      prependBar(decSpawnDelay(m))
    }
  }

  override def react(m:inputMap, v:View) = {
      //println("React " + {m get viewKey})
      move(v.foodDirection) + maybeSpawn(m, v)
  }

  def dispatchReact(m:inputMap) = {
    val generation = m.getOrElse(generationKey, "0").toInt
    val view = View(m(viewKey))

    if (generation > 0) {
      SlaveBot.react(m, view)
    } else {
      react(m, view)
    }
  }

}

object SlaveBot extends BotUtils {
  def explodeEnergy(m:inputMap) = m.getOrElse(energyKey, "0").toInt

  def nearOtherBot(m:inputMap, v:View) = {
    false
  }

  def maybeExplode(m:inputMap, v:View) = {
    if (nearOtherBot(m:inputMap, v:View)) {
      prependBar(explode(explodeEnergy(m)))
    } else {
      ""
    }
  }

  override def react(m:inputMap, v:View) = {
    move(v.otherBotDirection) + maybeExplode(m, v)
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
