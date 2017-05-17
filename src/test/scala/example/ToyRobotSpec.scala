package example

import org.scalatest._

class ToyRobotSpec extends FlatSpec with Matchers {
  val stream = new java.io.ByteArrayOutputStream()

  Console.withOut(stream) {
    ToyRobot.main(Array("place west 1 1", "report", "move", "report", "left", "report", "right", "report"))
  }

  stream.toString shouldEqual "Robot(West,1,1)\nRobot(West,0,1)\nRobot(South,0,1)\nRobot(West,0,1)\n"
}
