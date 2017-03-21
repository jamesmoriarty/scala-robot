object Directions extends Enumeration {
  val North, East, South, West = Value
}

case class Robot(direction: Directions.Value, x: Int, y: Int)


object ToyRobot extends App {
  def tokenize(line: String): Array[String] = line.toLowerCase.split("( |,)+")

  def exec(tokens: Array[String], robot: Option[Robot]): Option[Robot] = tokens match {
    case Array("place", direction: String, x: String, y: String) => {
      Some(Robot(Directions.withName(direction.capitalize), x.toInt, y.toInt))
    }
    case _ => robot
  }

  override def main(args:Array[String] ): Unit = {
    var robot: Option[Robot] = null

    for (line <- args) {
      val tokens = tokenize(line)
      robot = exec(tokens, robot)
      println(robot)
    }
  }
}

