package learn.session

case class UserState[D](id: String, sessions: Set[String], data: D) {
  def apply(event: UserState.Event[D]) = event(this)
}

object UserState {

  sealed trait Event[D] {
    def apply(state: UserState[D]): UserState[D]
  }

  case class SessionCreated[D](id: String) extends Event[D] {
    override def apply(state: UserState[D]): UserState[D] =
      state.copy(sessions = state.sessions + id)
  }

  case class SessionRemoved[D](id: String) extends Event[D] {
    override def apply(state: UserState[D]): UserState[D] =
      state.copy(sessions = state.sessions - id)
  }

  case class DataChanged[D](data: D) extends Event[D] {
    override def apply(state: UserState[D]): UserState[D] =
      state.copy(data = data)
  }

}