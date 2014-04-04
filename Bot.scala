import scala.util.Random

class ControlFunctionFactory {
  def create = new Bot().respond _
}

case class Direction(val x:Int, val y:Int) {
  override def toString = "direction=" + x + ":" + y
}

case class Position(val x:Int, val y:Int) {
  override def toString = "position=" + x + ":" + y

  def add(pos:Position, sideLength: Int):Position = {
    var newx = x + pos.x
    var newy = y + pos.y

    if (newx >= sideLength) {
      newx = newx % sideLength
      newy += 1
    }

    Position(newx, newy)
  }

  def toIndex(sideLength:Int) = x * sideLength + y
}

case class BotProperty(val k:String, val v: String) {
  override def toString = k + "=" + v
}

case class View(val viewStr:String) extends Config {
  // process viewStr here
  val sideLength = math.sqrt(viewStr.length).toInt
  val selfPos = Position(sideLength / 2, sideLength / 2)

  // http://daily-scala.blogspot.ca/2010/05/zipwithindex.html
  // uses "view" 
  /*
   * scala> s.view.zipWithIndex
   * res0: Seq[(Char, Int)] = SeqViewZ(...)
   */

  val indexedViewStr = viewStr.view.zipWithIndex

  def dumpView = {
    val rows = indexedViewStr.grouped(sideLength)

    rows.foreach { row => 
      val str = row.foldLeft(""){ case (acc, (c, i)) => acc + c}
      println(str)
    }
  }

  def verifySelfPos = {

  }


  // where to go to get food
  // very dumb
  def foodDirection2:Direction = {
    val allFoodCells = indexedViewStr.filter {
      case (c, idx) => c == 'P' || c == 'B'
    }

    val blah = allFoodCells.sortBy {
      case (c, idx) => {idx - viewStr.length/2}.abs
    }

    Direction(1,1)
  }

  def randomDirection = {
    Direction(Random.nextInt(3) - 1, Random.nextInt(3) - 1)
  }

  def isDirectionSafe(dir:Direction):Boolean = {
    val newPos = selfPos.add(Position(dir.x, dir.y), sideLength)
    val idx = newPos.toIndex(sideLength)
  }

  lazy val PossibleMoves = for {
    x <- Seq(-1,0,1)
    y <- Seq(-1,0,1)
  } yield Position(x,y)

  def foodDirection = {
    
    randomDirection
  }

  // don't go there??
  def dangerDirection = {
    Direction(0,0)
  }

  def otherBotDirection:Direction = randomDirection

  def safeDirection(d:Direction) = {
    Direction(0,0)
  }

}

//case class StateValue, 
//Set(key=value,...)

trait Config {
  val debug = true
  val chatty = true
}

trait BotUtils extends Config {

  type inputMap = Map[String, String]


  def log(str:String) = "Log(text=" + str + ")"
  def move(x:Int, y:Int):String = "Move(direction=" + x + ":" + y + ")"
  def move(dir:Direction):String = dir match {
    case Direction(x,y) => move(x,y)
  }

  val generationKey = "generation"
  val viewKey = "view"
  val energyKey = "energy"
  val spawnDelayKey = "spawndelay"

  def energySpawnMin = 200
  def spawnDelayTicks = 7

  def spawn(dir:Direction, name:String, energy:Int) = "Spawn(" + dir.toString + ",name=" + name + ",energy=" + energy + ")"

  def spawn(dir:Direction) = "Spawn(" + dir.toString + ")"

  // haha abusing Set.toString to get Set(....)
  def setKV(kvs:Set[BotProperty]) = kvs.toString

  def explode(s:Int) = "Explode(size=" + s + ")"

  def say(s:String) = "Say(" + s + ")"

  def status(s:String) = "Status(text=" + s + ")"

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

    if (debug) {
      println("sd=" + spawndelay + " esm=" + energySpawnMin + " e=" + energy)
    }

    spawndelay == 0 && (energySpawnMin == -1 || energy > energySpawnMin)
    energy > energySpawnMin && spawndelay == 0
  }

  def maybeSpawn(m:inputMap, v:View):String = {
    if (debug) {
      println("in maybeSpawn")
    }
    if (canSpawn(m)) {
      if (debug) println("canSpawn=yes")
      prependBar(spawn(v.otherBotDirection)) + prependBar(spawnDelay)
    } else {
      prependBar(decSpawnDelay(m))
    }
  }

  def energyStatus(m:inputMap) = {
      val energy = m.getOrElse(energyKey, "0").toInt

      chatty match {
        case true => 
          if (energy < 0) 
            prependBar(status("uh_oh")) 
          else 
            prependBar(status("yay"))

        case _ => ""
      }
  }

  override def react(m:inputMap, v:View) = {
      //println("React " + {m get viewKey})
      if (debug) {
        //println("Bot react")
        v.dumpView
      }
      
      move(v.foodDirection) + maybeSpawn(m, v) + energyStatus(m)
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
    Random.nextInt(100) < 50
  }

  def maybeExplode(m:inputMap, v:View) = {
    if (nearOtherBot(m, v)) {
      if (debug) println("maybeExplode=yes")
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
