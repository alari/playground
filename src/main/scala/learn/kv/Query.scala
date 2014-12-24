package learn.kv

sealed trait Query

object Query {
  case object GetKeys extends Query {
    def apply[T](state: KeyValueStorage[T]) = state.list
  }

  case class Get(id: String) extends Query {
    def path: List[String] = KeyValueStorage.split(id)

    def apply[T](state: KeyValueStorage[T]) = state.get(id)
  }
}