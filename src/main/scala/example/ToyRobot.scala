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
