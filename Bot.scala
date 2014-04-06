import scala.util.Random

class ControlFunctionFactory {
  def create = new Bot().respond _
}

case class Direction(val x:Int, val y:Int) {
  override def toString = "direction=" + x + ":" + y
}

case class Position(val x:Int, val y:Int) {
  override def toString = "position=" + x + ":" + y

  def add(dir:Direction, sideLength: Int):Position = {
    var newx = x + dir.x
    var newy = y + dir.y

    if (newx >= sideLength) {
      newx = newx % sideLength
      newy += 1
    }

    Position(newx, newy)
  }

  def toIndex(sideLength:Int) = y * sideLength + x

  def delta(pos:Position) = {
    val xDelta = {pos.x - x}.abs
    val yDelta = {pos.y - y}.abs
    xDelta + yDelta
  }

}

object Position {
  /*
   *
   * e.g. 5x5 grid
   *     0 1 2 3 4
   *     5 6 7 8 9
   *     ...
   *
   */
  def fromIndex(idx:Int, sideLength:Int) = Position(idx % sideLength, idx / sideLength)
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

  // takes an index and gives you a direction that
  // will get you closer to that index
  def directionTowardIndex(idx:Int):Direction = {
    val pos = Position.fromIndex(idx, sideLength)
    var x = pos.x - selfPos.x
    var y = pos.y - selfPos.y

    // normalize
    if (x.abs > 0) x = x/x.abs
    if (y.abs > 0) y = y/y.abs

    Direction(x, y)
  }


  def nearOtherBot = nearCharSet(enemyBot, 4)

  def nearCharSet(charSet:Set[Char], threshold:Int):Boolean = {

    val cells = indexedViewStr.filter {
      case (c, idx) => charSet contains c
    }

    if (cells.length == 0) {
      false
    } else {

      // go to the closest
      val closest = cells.sortBy {
        // viewStr.length/2 is our position, right in the middle?
        case (c, idx) => 
          val pos = Position.fromIndex(idx, sideLength)
          selfPos.delta(pos)
      }

      val distance = selfPos.delta(Position.fromIndex(closest.head._2, sideLength))
      distance <= threshold
    }
  }

  def foodDirection = directionToward(foodSet)
  def enemyBotDirection = directionToward(enemyBot)
  def enemyCreatureDirection = directionToward(enemyCreature)
  def masterDirection = directionToward(master)

  def directionToward(charSet:Set[Char]):Direction = {
    val cells = indexedViewStr.filter {
      case (c, idx) => charSet contains c
    }

    // go to the closest
    val blah = cells.sortBy {
      // viewStr.length/2 is our position, right in the middle?
      case (c, idx) => {idx - viewStr.length/2}.abs
    }

    val safeDirectionsToward = blah map { b => directionTowardIndex(b._2) } filter { d => isDirectionSafe(d) }

    if (safeDirectionsToward.length > 0) {
      val ret = safeDirectionsToward.head
      //println("going for it " + ret)
      ret
    } else {
      val safe = safeDirections
      if (safe.length > 0) {
        //println("going for safe " + safe.head)
        Random.shuffle(safe).head
      } else {
        //println("staying put")
        Direction(0,0)
      }
    }
  }

  def randomDirection = {
    Direction(Random.nextInt(3) - 1, Random.nextInt(3) - 1)
  }

  lazy val foodSet = Set('P', 'B')
  lazy val otherBot = Set('m')
  lazy val otherSlave = Set('s')
  lazy val badCreature = Set('p', 'b')
  lazy val enemyBot = otherBot ++ otherSlave
  lazy val enemyCreature = otherBot ++ Set('b')
  lazy val badCell = enemyBot ++ badCreature ++ Set('W')
  lazy val master = Set('M')

  def isFood(c:Char) = foodSet contains c
  def isBad(c:Char) = badCell contains c
  def isSafe(c:Char) = !isBad(c)

  def isPositionSafe(pos:Position):Boolean = {
    val idx = pos.toIndex(sideLength)
    //println("ispossafe: " + pos + " " + idx + " " + viewStr(idx))
    isSafe(viewStr(idx))
  }

  def isDirectionSafe(dir:Direction):Boolean = {
    val newPos = selfPos.add(dir, sideLength)
    isPositionSafe(newPos)
  }

  lazy val possibleMoves = for {
    x <- Seq(-1,0,1)
    y <- Seq(-1,0,1)
  } yield Direction(x,y)

  def safeDirections = possibleMoves filter { d =>
    val newpos = selfPos.add(d, sideLength)
    isPositionSafe(newpos)
  }

  // don't go there??
  def dangerDirection = {
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

  def returnEnergyThreshold:Int = Random.nextInt(800) + 500

  val generationKey = "generation"
  val viewKey = "view"
  val energyKey = "energy"
  val botTypeKey = "name"
  val spawnDelayKey = "spawndelay"

  def energySpawnMin = 200
  def spawnDelayTicks = 2

  def spawn(dir:Direction, name:String, energy:Int) = "Spawn(" + dir.toString + ",name=" + name + ",energy=" + energy + ")"
  def spawn(dir:Direction, name:String) = "Spawn(" + dir.toString + ",name=" + name + ")"
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
      prependBar(spawn(v.enemyBotDirection, "slave")) + prependBar(spawnDelay)
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
        //v.dumpView
        //println(v.safeDirections)
      }
      
      move(v.foodDirection) + maybeSpawn(m, v) + energyStatus(m)
  }

  def dispatchReact(m:inputMap) = {
    val generation = m.getOrElse(generationKey, "0").toInt
    val view = View(m(viewKey))

    if (generation > 0) {
      println(m)
      m(botTypeKey) match {
        case "slave" => SlaveBot().react(m, view)
        case "missile" => MissileBot().react(m, view)
      }
    } else {
      react(m, view)
    }
  }

}

case class SlaveBot() extends BotUtils {
  def energy(m:inputMap) = m.getOrElse(energyKey, "0").toInt

  def botType = "slave"
  def identity = prependBar(setKV(Set(BotProperty(botTypeKey, botType))))

  def shouldReturnToMaster(m:inputMap) = energy(m) >= returnEnergyThreshold

  override def react(m:inputMap, v:View) = {
    if (debug) {
      //v.dumpView
      //println(m)
    }
    //move(v.enemyBotDirection) + maybeExplode(m, v)

    val ret = 
    if (shouldReturnToMaster(m)) {
      move(v.masterDirection) + identity
    } else {
      move(v.foodDirection) + identity
    }

    if (debug) {
      println(ret)
    }
    ret
  }

}

case class MissileBot() extends SlaveBot {

  def explodeEnergy(m:inputMap) = energy(m)
  
  override def botType = "missile"

  def maybeExplode(m:inputMap, v:View) = {
    if (v.nearOtherBot) {
      prependBar(explode(explodeEnergy(m)))
    } else {
      ""
    }
  }

  override def react(m:inputMap, v:View) = {
    if (debug) {
      //v.dumpView
      //println(m)
    }
    move(v.enemyBotDirection) + maybeExplode(m, v) + identity
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
