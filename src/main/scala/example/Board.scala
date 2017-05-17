package example

case class Board(width: Int, height: Int) {
  def within(x: Int, y: Int): Boolean = x < width && x >= 0 && y < height && y >= 0
}
