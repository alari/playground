package learn.kv

trait ValueAppender[T] {
  def wrap(value: T): T

  def append(to: T, values: T*): T

  def isCorrect(v1: T, v2: T): Boolean = {
    require(wrap(v1) == append(v1), "Result of wrapping value and appending nothing to it should be equal")
    require(append(v1, v2) == append(wrap(v1), v2), "Appending to a wrapped value should be equal to appending to a wrapped value")
    true
  }
}