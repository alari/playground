package learn.kv

case class Value[T](value: T, expires: Long)