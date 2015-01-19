package learn.storage

import akka.actor.Actor
import akka.persistence.PersistentActor

import scala.reflect.ClassTag
import scalaz.Monoid

trait KeyValueEventSourced[T] extends Actor with PersistentActor{

  import KeyValueEventSourced._

  private[this] var _store: KeyValue[T] = KeyValue()
  private[storage] def mutate(f: KeyValue[T] => KeyValue[T]) = _store = f(_store)
  protected def store = _store

  def receiveRecoverKV(implicit m: Monoid[T], ct: ClassTag[T]): Receive = {
    case ValueSet(key, value: T) =>
      mutate(_.update(key, value))
    case ValueAdded(key, value: T) =>
      mutate(_.add(key, value))
    case ValueRemoved(key: String) =>
      mutate(_.unset(key))
    case BranchRemoved(key: String) =>
      mutate(_.removeBranch(key))
  }

  def receiveCommandKV(implicit m: Monoid[T], ct: ClassTag[T]): Receive = {
    case SetValue(key, value: T) =>
      persistAsync(ValueSet(key, value)) {e =>
        mutate(_.update(e.key, e.value))
        sendKeyValue(key, store(key))
      }

    case AddValue(key, value: T) =>
      persistAsync(ValueAdded(key, value)) {e =>
        mutate(_.add(key, value))
        sendKeyValue(key, store(key))
      }

    case RemoveValue(key) =>
      persistAsync(ValueRemoved(key)) {e =>
        sendKeyValue(key, store(key))
        mutate(_.unset(key))
      }

    case RemoveBranch(key) =>
      persistAsync(BranchRemoved(key)) {e =>
        mutate(_.removeBranch(key))
        sendKeyAck(key)
      }

    case GetValue(key) =>
      sendKeyValue(key, store(key))

    case GetKeys(key) =>
      sendKeys(store.flattenBranch(key).keySet)

    case GetBranch(key) =>
      sendBranch(key, store.flattenBranch(key))
  }

  def sendKeyValue(key: String, value: Option[T]): Unit = {
    sender() ! value
  }

  def sendKeyAck(key: String): Unit ={
    sender() ! key
  }

  def sendKeys(keys: Set[String]): Unit ={
    sender() ! keys
  }

  def sendBranch(key: String, branch: Map[String,T]): Unit ={
    sender() ! branch
  }
}

object KeyValueEventSourced {
  sealed trait Command

  case class SetValue[T](key: String, value: T) extends Command
  case class AddValue[T](key: String, value: T) extends Command
  case class RemoveValue(key: String)
  case class RemoveBranch(key: String)

  sealed trait Query

  case class GetValue(key: String) extends Query
  case class GetKeys(key: String) extends Query
  case class GetBranch(key: String) extends Query

  sealed trait Event

  case class ValueSet[T](key: String, value: T) extends Event
  case class ValueAdded[T](key: String, value: T) extends Event
  case class ValueRemoved(key: String) extends Event
  case class BranchRemoved(key: String) extends Event

}