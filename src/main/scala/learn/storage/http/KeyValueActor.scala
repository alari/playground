package learn.storage.http

import akka.actor.ActorLogging
import learn.storage.KeyValueEventSourced

import scalaz.Monoid

class KeyValueActor extends KeyValueEventSourced[String] with ActorLogging {
  println("kv actor with path = "+self.path)

  implicit val stringMonoid = Monoid.instance[String](_ + _, "")

  override def receiveCommand: Receive = {
    case (id: String, msg) =>
      log.info("FORWARD "+msg)
      self.tell(msg, sender())
    case m => log.error("WTF? "+m)

  }

  override def receiveRecover: Receive = super.receiveRecoverKV

  override def persistenceId: String = "kv-" + self.path.name

  override def postStop(): Unit = {
    log.error("IT WAS STOPPED! WHY?")
    super.postStop()
  }
}