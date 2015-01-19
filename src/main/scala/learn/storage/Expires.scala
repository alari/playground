package learn.storage

case class Expires[T](value: T, expires: Long) extends Expires.Box

object Expires {

  case class ExpireCommand(threshold: Long)

  case class ExpiredEvent(threshold: Long)

  trait Box {
    def expires: Long
  }

  trait ExpireStore[T <: Box] {
    it: KeyValueEventSourced[T] =>

    abstract override def receiveRecover: Receive = it.receiveRecover.orElse {
      case ExpiredEvent(t) =>
        mutate(_.span(_.expires < t)._2)
    }

    abstract override def receiveCommand: Receive = it.receiveCommand.orElse {
      case ExpireCommand(t) =>
        persistAsync(ExpiredEvent(t)){ e =>
          val (drop, keep) = store.span(_.expires < e.threshold)
          mutate(_ => keep)
          onStoreExpired(drop)
        }
    }

    def onStoreExpired(expired: KeyValue[T]): Unit = ()
  }

}