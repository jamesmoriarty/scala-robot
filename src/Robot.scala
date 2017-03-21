object Directions extends Enumeration {
  val North, East, South, West = Value
}

case class Robot(direction: Directions.Value, x: Int, y: Int) {
  def move: Robot = direction match {
    case Directions.North => copy(this.direction, this.x, this.y + 1)
    case Directions.East => copy(this.direction, this.x + 1, this.y)
    case Directions.South => copy(this.direction, this.x, this.y - 1)
    case Directions.West => copy(this.direction, this.x - 1, this.y)
  }
}

object ToyRobot extends App {
  def tokenize(line: String): Array[String] = line.toLowerCase.split("( |,)+")

  def place(direction: String, x: String, y: String): Option[Robot] = {
    try {
      Some(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt))
  } catch {
    case e: Exception => None
  }
}

  def exec(tokens: Array[String], robot: Option[Robot]): Option[Robot] = (tokens, robot) match {
    case (Array("place", direction: String, x: String, y: String), _) => place(direction, x, y)
    case (Array("move"), Some(robot)) => Some(robot.move)
    case (_, Some(robot)) => {
      println("unknown command")
      Some(robot)
    }
    case _ => {
      println("must place robot")
      None
    }
  }

  override def main(args:Array[String] ): Unit = {
    var robot: Option[Robot] = None

    for (line <- args) {
      val tokens = tokenize(line)
      robot = exec(tokens, robot)
      println(robot)
    }
  }
}

