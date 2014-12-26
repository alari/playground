package learn.session

import akka.actor.Actor
import akka.persistence.PersistentActor

class SessionActor extends Actor with PersistentActor{
  var state: Option[SessionState] = None

  import SessionActor._

  override def receiveRecover: Receive = ???

  override def receiveCommand: Receive = {
    case i@Initialize(userId, expires) if state.isEmpty =>
      persist(i) {_ =>
        state = Some(SessionState(id = self.path.name, userId = i.userId, timeout = 0, expires = i.expires))
      }
  }

  override def persistenceId: String = "s-session-"+self.path.name
}

object SessionActor {
  sealed trait Command
  case class Initialize(userId: String, expires: Long) extends Command
  case object Delete extends Command

  sealed trait Query
  case object Get extends Query
}