package example

object Directions extends Enumeration {
  val North, East, South, West = Value

  def rotate(direction: Directions.Value, n: Int): Directions.Value = {
    Directions((direction.id + n + Directions.maxId) % Directions.maxId)
  }
}
