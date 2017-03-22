import scala.util.{Failure, Success, Try}

object Directions extends Enumeration {
  val North, East, South, West = Value

  def rotate(direction: Directions.Value, n: Int): Directions.Value = {
    Directions.apply((direction.id + n + Directions.maxId) % Directions.maxId)
  }
}

object Board {
  def onBoard(x: Int, y: Int): Boolean = x < 5 && x >= 0 && y < 5 && y >= 0
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

  def exec(tokens: Array[String], robot: Option[Robot]): Try[Robot] = (tokens, robot) match {
    case (Array("place", direction: String, x: String, y: String), _) => Try(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt))
    case (Array("move"), Some(robot)) => Success(robot.move)
    case (Array("left"), Some(robot)) => Success(robot.left)
    case (Array("right"), Some(robot)) => Success(robot.right)
    case _ =>  Failure(new Exception("Command Failed"))
  }

  def validate(robot: Robot): Try[Robot] = if(Board.onBoard(robot.x, robot.y)) Success(robot)
  else Failure(new Exception("Invalid Robot"))

  override def main(args:Array[String] ): Unit = {
    var robot: Option[Robot] = None

    for (line <- args) {
      val tokens = tokenize(line)

      robot = exec(tokens, robot).map(validate(_)) match {
        case Success(newRobot) => newRobot.toOption
        case Failure(e) => robot
      }

      println(robot)
    }
  }
}

