import scala.util.{Failure, Success, Try}

sealed trait Direction {
  def left: Direction
  def right: Direction
  def fromString(name: String) = Map("north" -> NORTH, "east" -> EAST, "west" -> WEST, "south" -> SOUTH)(name)
}
case object NORTH extends Direction {
  def left: Direction = WEST
  def right: Direction = EAST
}
case object EAST extends Direction {
  def left: Direction = NORTH
  def right: Direction = SOUTH
}
case object WEST extends Direction {
  def left: Direction = SOUTH
  def right: Direction = NORTH
}
case object SOUTH extends Direction {
  def left: Direction = EAST
  def right: Direction = WEST
}

case class Robot(direction: Direction, x: Int, y: Int) {
  def move: Robot = direction match {
    case NORTH => copy(y=this.y + 1)
    case EAST => copy(x=this.x + 1)
    case SOUTH => copy(y=this.y - 1)
    case WEST => copy(x=this.x - 1)
  }

  def left: Robot = copy(direction=direction.left)

  def right: Robot = copy(direction=direction.right)
}

object ToyRobot extends App {
  def exec(line: String, robot: Option[Robot]): Try[Robot] = (tokenize(line), robot) match {
    case ("place" :: direction :: x :: y :: Nil, _) => Try(Robot(NORTH.fromString(direction), x.toInt, y.toInt))
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

