package learn.session

import akka.actor.Actor
import akka.contrib.pattern.ClusterSharding
import akka.persistence.PersistentActor

import scala.reflect.ClassTag

class UserActor[D: ClassTag](emptyData: D, dataIsEmpty: D => Boolean) extends Actor with PersistentActor {
  import UserActor._

  var state: UserState[D] = UserState(id = self.path.name, sessions = Set.empty, data = emptyData)

  val sessions = ClusterSharding(context.system).shardRegion("sessions")

  def persistenceId = "s-user-"+self.path.name

  override def receiveRecover = {
    case e: UserState.Event[D] =>
      state = e(state)
  }

  override def receiveCommand = {
    case GetData =>
      sender() ! state.data

    case SetData(d: D) =>
      if(d != state.data) {
        persist(UserState.DataChanged[D](d)){ e =>
          state = e(state)
          sender() ! e.data
        }
      } else {
        sender() ! state.data
      }

    case CreateSession(d: D) if dataIsEmpty(d) =>
      persist(UserState.SessionCreated[D]("")) {
        e =>
          // TODO: send message to create a session
          state = e(state)
          //e.id
      }

    case ListSessions =>
      sender() ! state.sessions

    case Delete =>
      // TODO: delete till the last message
      deleteMessages(-1, permanent = true)

    case DeleteSession(id) if state.sessions.contains(id) =>
      persist(UserState.SessionRemoved[D](id)){
        e => state = e(state)
        // TODO: send ok
      }

    case DeleteSession(id) =>
    // TODO: send not found
  }
}

object UserActor {
  sealed trait Command
  case class SetData[D](data: D) extends Command
  case class CreateSession[D](data: D) extends Command
  case object Delete extends Command
  case class DeleteSession(id: String) extends Command

  sealed trait Query
  case object ListSessions extends Query
  case object GetData extends Query
}