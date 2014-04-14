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
    //if (x.abs > 0) x = x/x.abs
    //if (y.abs > 0) y = y/y.abs

    Direction(x, y)
  }

  def directionAwayIndex(idx:Int):Direction = {
    val dir = directionTowardIndex(idx)
    Direction(-dir.x, -dir.y)
  }

  def nearOtherBot = nearCharSet(enemyBot, 4)
  def nearEnemyCreature(nearness:Int) = nearCharSet(enemyCreature, nearness)
  def nearDanger = nearCharSet(danger, 3)

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
  def fleeDirection = directionAway(badCell)
  def enemyBotDirection = directionToward(enemyBot)
  def enemyCreatureDirection = directionToward(enemyCreature)
  def masterDirection = directionToward(master) 

  /* go toward high value food as first choice */
  def smartFoodDirection = directionTowardOpt(prefferredFood) match {
    case Some(dir) => dir
    case None => directionToward(secondChoiceFood)
  }

  def masterDirectionFromInputMap(m:Map[String,String]) = {
    //master -> 0:17
    val masterDir = m("master")
    val xy = masterDir.split(':')

    val dir = Direction(xy(0).toInt, xy(1).toInt)

    if (isNormalizedDirectionSafe(dir)) {
      dir
    } else {
      //randomSafeDirection
      foodDirection
    }
  }

  //def masterDirection = if (nearDanger) fleeDirection else directionToward(master)

  def optionDirection = {
    /*
    if (nearDanger) {
      fleeDirection
    } else {
      foodDirection
    }
    */
   foodDirection
  }

  def randomSafeDirection = {
    val safe = safeDirections
    if (safe.length > 0) {
      Random.shuffle(safe).head
    } else {
      Direction(0,0)
    }
  }

  def directionToward(charSet:Set[Char]):Direction = directionTowardOpt(charSet) match {
    case Some(dir) => dir
    case None => randomSafeDirection
  }

  def directionTowardOpt(charSet:Set[Char]):Option[Direction] = {
    val cells = indexedViewStr.filter {
      case (c, idx) => charSet contains c
    }

    // go to the closest
    val blah = cells.sortBy {
      // viewStr.length/2 is our position, right in the middle?
      case (c, idx) => 
        //{idx - viewStr.length/2}.abs
          val pos = Position.fromIndex(idx, sideLength)
          selfPos.delta(pos)
    }

    //val safeDirectionsToward = blah map { b => directionTowardIndex(b._2) } filter { d => isDirectionSafe(d) }
    val safeDirectionsToward = blah map { b => directionTowardIndex(b._2) } filter { d => isNormalizedDirectionSafe(d) }

    if (safeDirectionsToward.length > 0) {
      val ret = safeDirectionsToward.head
      Some(ret)
    } else {
      None
    }
  }

  def directionAway(charSet:Set[Char]):Direction = {
    val cells = indexedViewStr.filter {
      case (c, idx) => charSet contains c
    }

    // find the one that's closest
    val blah = cells.sortBy {
      // viewStr.length/2 is our position, right in the middle?
      case (c, idx) => 
        //{idx - viewStr.length/2}.abs
          val pos = Position.fromIndex(idx, sideLength)
          -selfPos.delta(pos)
    }

    val safeDirectionsAway = blah map { b => directionAwayIndex(b._2) } filter { d => isDirectionSafe(d) }

    if (safeDirectionsAway.length > 0) {
      val ret = safeDirectionsAway.head
      ret
    } else {
      val safe = safeDirections
      if (safe.length > 0) {
        Random.shuffle(safe).head
      } else {
        Direction(0,0)
      }
    }
  }

  def randomDirection = {
    Direction(Random.nextInt(3) - 1, Random.nextInt(3) - 1)
  }

  // TODO: somehow prefer to chase more valuable food?
  lazy val foodSet = Set('P', 'B')
  lazy val prefferredFood = Set('B')
  lazy val secondChoiceFood = Set('P')
  lazy val otherBot = Set('m')
  lazy val otherSlave = Set('s')
  lazy val badCreature = Set('p', 'b')
  lazy val enemyBot = otherBot ++ otherSlave

  //lazy val enemyCreature = otherBot ++ Set('b')
  //lazy val enemyCreature = otherBot ++ otherSlave ++ Set('b')
  lazy val enemyCreature = otherBot ++ otherSlave
  //lazy val enemyCreature = otherBot ++ Set('b')

  lazy val danger = enemyBot ++ Set('b')
  lazy val badCell = enemyBot ++ badCreature ++ Set('W')
  lazy val master = Set('M')

  def isFood(c:Char) = foodSet contains c
  def isBad(c:Char) = badCell contains c
  def isSafe(c:Char) = !isBad(c)

  def isPositionSafe(pos:Position):Boolean = {
    val idx = pos.toIndex(sideLength)
    isSafe(viewStr(idx))
  }

  def isDirectionSafe(dir:Direction):Boolean = {
    val newPos = selfPos.add(dir, sideLength)
    isPositionSafe(newPos)
  }

  def normalizeDirection(dir:Direction) = {
    val x = if (dir.x == 0) 0 else dir.x/dir.x.abs
    val y = if (dir.y == 0) 0 else dir.y/dir.y.abs
    Direction(x,y)
  }

  def isNormalizedDirectionSafe(dir:Direction):Boolean = isDirectionSafe(normalizeDirection(dir))

  lazy val possibleMoves = for {
    x <- Seq(-1,0,1)
    y <- Seq(-1,0,1)
  } yield Direction(x,y)

  def safeDirections = possibleMoves filter { d =>
    val newpos = selfPos.add(d, sideLength)
    isPositionSafe(newpos)
  }
}

//case class StateValue, 
//Set(key=value,...)

trait Config {
  val debug = false
  val chatty = true
}

trait BotUtils extends Config {

  type inputMap = Map[String, String]


  def log(str:String) = "Log(text=" + str + ")"
  def move(x:Int, y:Int):String = "Move(direction=" + x + ":" + y + ")"
  def move(dir:Direction):String = dir match {
    case Direction(x,y) => move(x,y)
  }

  def createReturnEnergyThreshold:String = {Random.nextInt(500) + 500}.toString

  val generationKey = "generation"
  val viewKey = "view"
  val energyKey = "energy"
  val botTypeKey = "botType"
  val spawnDelayKey = "spawndelay"
  val returnEnergyKey = "returnEnergyAmount"
  val assassinOptionKey = "assissinOption"
  val explodeRadiusKey = "explodeRadius"
  def nearnessKey = "nearnessFactor"
  val spawnCountKey = "spawnCount"
  //def slaveCanSpawn = if (Random.nextInt(100) < 50) "yes" else "no"
  def slaveCanSpawn = if (Random.nextInt(100) < 35) "yes" else "no"
  def canSpawnKey = "canSpawn"

  def energySpawnMin = 200
  def assassinOptionMin = 500
  def maxSpawnCount = 3
  def spawnAssassin = false //true //Random.nextInt(100) < 90
  //def spawnDelayTicks = 2
  def spawnDelayTicks = 0
  val explodeRadius = Random.nextInt(5) + 5
  val nearnessFactor = Random.nextInt(3) + 3

  def spawn(dir:Direction, botType:String, energy:Int) = "Spawn(" + dir.toString + ",botType=" + botType + ",energy=" + energy + ")"
  def spawn(dir:Direction, botType:String) = "Spawn(" + dir.toString + ",botType=" + botType + ")"
  def spawn(dir:Direction, botType:String, extra:String) = "Spawn(" + dir.toString + ",botType=" + botType + "," + extra + ")"
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

    inputmap match {
      case ("Welcome", _) => 
        ""

      case ("Goodbye", _) =>
        ""

      case ("React", m:inputMap) => 
        val str = dispatchReact(m)
        //if (debug) println(str)
        str

      case (str, m:inputMap) => 
        println("unknown opcode: " + str)
        move(Direction(-1,-1))
    }

  }

  override def canSpawn(m:inputMap) = {
    val energy = m.getOrElse(energyKey, "0").toInt
    val spawndelay = m.getOrElse(spawnDelayKey, "0").toInt

    if (false && debug) {
      println("sd=" + spawndelay + " esm=" + energySpawnMin + " e=" + energy)
    }

    //spawndelay == 0 && (energySpawnMin == -1 || energy > energySpawnMin)
    energy > energySpawnMin && spawndelay == 0
  }

  def maybeLaunch(m:inputMap, v:View):String = {
    if (v.nearDanger) {
      val extra = nearnessKey + "=" + nearnessFactor + "," + explodeRadiusKey + "=" + explodeRadius
      prependBar(spawn(v.enemyCreatureDirection, "missile", extra))
    } else {
      ""
    }
  }

  def maybeAssassin(m:inputMap, v:View):String = {
      val energy = m.getOrElse(energyKey, "0").toInt
      if (energy > assassinOptionMin && spawnAssassin) {
        assassinOptionKey + "=yes"
      } else {
        ""
      }
  }

  def maybeSlaveCanSpawn(m:inputMap, v:View):String = "," + canSpawnKey + "=" + slaveCanSpawn

  def maybeSpawn(m:inputMap, v:View):String = {
    if (canSpawn(m)) {
      val launch = maybeLaunch(m,v)

      val spawnString = if (launch.isEmpty) {
        val extra = maybeAssassin(m,v) + maybeSlaveCanSpawn(m,v)
        prependBar(spawn(v.foodDirection, "slave", extra))
      } else {
        launch
      }

      spawnString + prependBar(spawnDelay)

    } else {
      prependBar(decSpawnDelay(m))
    }
  }

  def energyStatus(m:inputMap) = {
      val energy = m.getOrElse(energyKey, "0").toInt

      chatty match {
        case true => 
          if (energy < 0) 
            prependBar(status("uh oh")) 
          else 
            prependBar(status("yay"))

        case _ => ""
      }
  }

  override def react(m:inputMap, v:View) = {
      if (debug) {
        //println("Bot react")
        //v.dumpView
        //println(v.safeDirections)
      }
      
      move(v.optionDirection) + maybeSpawn(m, v) + energyStatus(m)
  }

  def dispatchReact(m:inputMap) = {
    val generation = m.getOrElse(generationKey, "0").toInt
    val view = View(m(viewKey))

    if (generation > 0) {
      m(botTypeKey) match {
        case "slave" => SlaveBot().react(m, view)
        case "missile" => MissileBot().react(m, view)
      }
    } else {
      //println(m)
      react(m, view)
    }
  }

}

case class SlaveBot() extends Bot {
  def energy(m:inputMap) = m.getOrElse(energyKey, "0").toInt
  def getReturnEnergyThreshold(m:inputMap) = m.getOrElse(returnEnergyKey, createReturnEnergyThreshold)
  def getAssassinOption(m:inputMap) = m.getOrElse(assassinOptionKey, "no")
  def getSpawnCount(m:inputMap) = m.getOrElse(spawnCountKey, "0")
  def getCanSpawn(m:inputMap) = m.getOrElse(canSpawnKey, "no")


  var botType = "slave"
  var sc = 0
  var canSpawn = "no"
  def identity(m:inputMap) = prependBar(setKV(Set(BotProperty(botTypeKey, botType), 
                                         BotProperty(returnEnergyKey, getReturnEnergyThreshold(m)),
                                         BotProperty(assassinOptionKey, getAssassinOption(m)),
                                         BotProperty(spawnCountKey, sc.toString),
                                         BotProperty(canSpawnKey, canSpawn)
                                         )))

  // TODO: make SlaveBot launch defensive missiles...
  // and also spawn more slavebots?
  
  override def maybeSpawn(m:inputMap, v:View) = {
    val generation = m.getOrElse(generationKey, "0").toInt
    canSpawn = getCanSpawn(m)

    if (generation == 1 && canSpawn == "yes") {
      // uh oh if a bot can spawn it will continually spawn until it is dead/reclaimed?
      super.maybeSpawn(m,v)
    } else {
      ""
    }
  }

  override def react(m:inputMap, v:View) = {
    if (debug) {
      //v.dumpView
      //println(m)
    }

    //println(m)

    // gah this is set twice the first time... once here and once when identity is set the first time
    val returnEnergyThreshold = getReturnEnergyThreshold(m).toInt

    val ret = 
    if (energy(m) > returnEnergyThreshold) {
      if (getAssassinOption(m) == "yes") {
        if (debug) println("assassin becoming missile")
        botType = "missile"
        // TODO: get explodeRadius/nearness pair
        move(v.enemyBotDirection) + identity(m)
      } else {
        move(v.masterDirectionFromInputMap(m)) + identity(m)
      }
    } else {
      val ret = 
      move(v.foodDirection) + maybeSpawn(m, v) + identity(m)
      //Move(direction=1:1)|Spawn(direction=-1:1,botType=missile)|Spawn(direction=0:-1,botType=slave,)|Set(spawndelay=0)|Set(botType=slave,
      //returnEnergyAmount=593, assissinOption=no, spawnCount=1)
      //
      //println(ret)
      ret
    }

    if (debug) {
      //println(ret)
    }
    ret
  }

}

case class MissileBot() extends SlaveBot {

  botType = "missile"

  def getExplodeRadius(m:inputMap) = m.getOrElse(explodeRadiusKey, explodeRadius.toString)
  def getNearnessFactor(m:inputMap) = m.getOrElse(nearnessKey, nearnessFactor.toString)
  override def identity(m:inputMap) = super.identity(m) + prependBar(setKV(Set(BotProperty(explodeRadiusKey, getExplodeRadius(m)),
    BotProperty(nearnessKey, getNearnessFactor(m)))))

  def maybeExplode(m:inputMap, v:View) = {
    if (v.nearEnemyCreature(getNearnessFactor(m).toInt)) {
      prependBar(explode(getExplodeRadius(m).toInt))
    } else {
      ""
    }
  }

  def missileStatus = prependBar(status("M"))

  override def react(m:inputMap, v:View) = {
    if (debug) {
      //v.dumpView
      //println(m)
    }
    move(v.enemyCreatureDirection) + maybeExplode(m, v) + identity(m)
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
