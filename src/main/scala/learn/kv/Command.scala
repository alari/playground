package learn.kv

sealed trait Command

object Command {
  sealed trait KeyCommand[T] extends Command {
    def id: String

    def path: List[String] = KeyValueStorage.split(id)
  }

  case class Set[T](id: String, value: T, expires: Long) extends KeyCommand[T]

  case class Add[T](id: String, value: T, expires: Long) extends KeyCommand[T]

  case class Delete[T](id: String) extends KeyCommand[T]

  case class Expire(lessThen: Long) extends Command
}
