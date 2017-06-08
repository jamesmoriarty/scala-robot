package example

import scala.util.{Failure, Success, Try}

object ToyRobot extends App {
  def exec(line: String, robot: Option[Robot]): (Try[Robot], Option[String]) = (tokenize(line), robot) match {
    case ("place" :: direction :: x :: y :: Nil, _) => (Try(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt)), None)
    case ("move" :: Nil, Some(robot))               => (Success(robot.move), None)
    case ("left" :: Nil, Some(robot))               => (Success(robot.left), None)
    case ("right" :: Nil, Some(robot))              => (Success(robot.right), None)
    case ("report" :: Nil, Some(robot))             => (Success(robot), Some(robot.toString))
    case _                                          => (Failure(new Exception("Command Failed")), None)
  }

  def tokenize(line: String): List[String] = line.toLowerCase.split("( |,)+").toList

  override def main(args: Array[String]): Unit = {
    val board: Board = new Board(5, 5)

    args.foldLeft(None: Option[Robot]) { (robot, line) =>
      val (newRobot, output) = exec(line, robot)
      output.foreach(println)
      newRobot.filter((newRobot) => board.within(newRobot.x, newRobot.y)) match {
        case Success(newRobot) => Some(newRobot)
        case _ => robot
      }
    }
  }
}
