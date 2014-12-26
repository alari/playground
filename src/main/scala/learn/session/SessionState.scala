package learn.session

case class SessionState(id: String, userId: String, timeout: Int, expires: Long)