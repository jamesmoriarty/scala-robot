import scala.util.Try

object Directions extends Enumeration {
  val North, East, South, West = Value

  def rotate(direction: Directions.Value, n: Int): Directions.Value = {
    Directions.apply((direction.id + n + Directions.maxId) % Directions.maxId)
  }
}

object Board {
  def maxX = 5
  def maxY = 5
  def onBoard(x: Int, y: Int): Boolean = {
    x < maxX && x >= 0 && y < maxY && y >= 0
  }
}

case class Robot(direction: Directions.Value, x: Int, y: Int) {
  def move: Robot = direction match {
    case Directions.North => copy(this.direction, this.x, this.y + 1)
    case Directions.East => copy(this.direction, this.x + 1, this.y)
    case Directions.South => copy(this.direction, this.x, this.y - 1)
    case Directions.West => copy(this.direction, this.x - 1, this.y)
  }

  def left: Robot = copy(Directions.rotate(this.direction, 1), this.x, this.y)

  def right: Robot = copy(Directions.rotate(this.direction, -1), this.x, this.y)
}

object ToyRobot extends App {
  def tokenize(line: String): Array[String] = line.toLowerCase.split("( |,)+")

  def exec(tokens: Array[String], robot: Option[Robot]): Option[Robot] = (tokens, robot) match {
    case (Array("place", direction: String, x: String, y: String), _) => Try(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt)).toOption
    case (Array("move"), Some(robot)) => Some(robot.move)
    case (Array("left"), Some(robot)) => Some(robot.left)
    case (Array("right"), Some(robot)) => Some(robot.right)
    case _ =>  robot
  }

  def validate(robot: Option[Robot]): Option[Robot] = robot match {
    case Some(robot) => {
      if(Board.onBoard(robot.x, robot.y)) {
        Some(robot)
      } else {
        None
      }

    }
    case _ => None
  }

  override def main(args:Array[String] ): Unit = {
    var robot: Option[Robot] = None

    for (line <- args) {
      val tokens = tokenize(line)

      robot = validate(exec(tokens, robot)) match {
        case Some(newRobot) => Some(newRobot)
        case None => robot
      }

      println(robot)
    }
  }
}

