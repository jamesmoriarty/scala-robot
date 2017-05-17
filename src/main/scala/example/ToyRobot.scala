package example

import scala.util.{Failure, Success, Try}

object ToyRobot extends App {
  def exec(line: String, robot: Option[Robot]): Try[Robot] = (tokenize(line), robot) match {
    case ("place" :: direction :: x :: y :: Nil, _) => Try(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt))
    case ("move" :: Nil, Some(robot)) => Success(robot.move)
    case ("left" :: Nil, Some(robot)) => Success(robot.left)
    case ("right" :: Nil, Some(robot)) => Success(robot.right)
    case ("report" :: Nil, Some(robot)) => println(robot); Success(robot)
    case _ => Failure(new Exception("Command Failed"))
  }

  def tokenize(line: String): List[String] = line.toLowerCase.split("( |,)+").toList

  override def main(args: Array[String]): Unit = {
    val board: Board = new Board(5, 5)
    val robot: Option[Robot] = None

    args.foldLeft(robot) { (robot: Option[Robot], line: String) =>
      exec(line, robot).filter((newRobot) => board.within(newRobot.x, newRobot.y)) match {
        case Success(newRobot) => Some(newRobot)
        case _ => robot
      }
    }
  }
}
