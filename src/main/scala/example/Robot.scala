package example

import example.Directions._

case class Robot(direction: Directions.Value, x: Int, y: Int) {
  def move: Robot = direction match {
    case North => copy(y=this.y + 1)
    case East => copy(x=this.x + 1)
    case South => copy(y=this.y - 1)
    case West => copy(x=this.x - 1)
  }

  def left: Robot = copy(direction=Directions.rotate(this.direction, -1))

  def right: Robot = copy(direction=Directions.rotate(this.direction, 1))
}
