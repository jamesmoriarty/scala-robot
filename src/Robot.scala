import scala.util.{Failure, Success, Try}

object Directions extends Enumeration {
  val North, East, South, West = Value

  def rotate(direction: Directions.Value, n: Int): Directions.Value = {
    Directions((direction.id + n + Directions.maxId) % Directions.maxId)
  }

  def apply(string: String): Directions.Value = {
    Directions.withName(string.capitalize)
  }
}

case class Robot(direction: Directions.Value, x: Int, y: Int) {
  def move: Robot = direction match {
    case Directions.North => copy(y=this.y + 1)
    case Directions.East => copy(x=this.x + 1)
    case Directions.South => copy(y=this.y - 1)
    case Directions.West => copy(x=this.x - 1)
  }

  def left: Robot = copy(direction=Directions.rotate(this.direction, -1))

  def right: Robot = copy(direction=Directions.rotate(this.direction, 1))
}

object ToyRobot extends App {
  def exec(line: String, robot: Option[Robot]): Try[Robot] = (tokenize(line), robot) match {
    case ("place" :: direction :: x :: y :: Nil, _) => Try(Robot(Directions(direction), x.toInt, y.toInt))
    case ("move" :: Nil, Some(robot)) => Success(robot.move)
    case ("left" :: Nil, Some(robot)) => Success(robot.left)
    case ("right" :: Nil, Some(robot)) => Success(robot.right)
    case ("report" :: Nil, Some(robot)) => {
      println(robot)
      Success(robot)
    }
    case _ => Failure(new Exception("Command Failed"))
  }

  def onBoard(robot: Robot): Try[Robot] =
    if (robot.x < 5 && robot.x >= 0 && robot.y < 5 && robot.y >= 0)
      Success(robot)
    else
      Failure(new Exception("Invalid Robot"))

  def tokenize(line: String): List[String] = line.toLowerCase.split("( |,)+").toList

  override def main(args: Array[String]): Unit = {
    val robot: Option[Robot] = None
    args.foldLeft(robot) { (robot: Option[Robot], line: String) =>
      exec(line, robot).map(onBoard(_)) match {
        case Success(newRobot) => newRobot.toOption
        case Failure(e) => robot
      }
    }
  }
}

