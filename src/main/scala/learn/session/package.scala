package learn


package object session {

  case class SessionState(
                      userId: String,
                      data: Option[AnyRef],
                      expiresIn: Long
                      )


trait KeyValueController {

}

  object KeyValueController {
import ScalaRoutingDSL._
  }

}


//GET /users/:id
//POST /users/:id
//GET /users/:id/sessions
//
//GET /users/:id/keys
//GET /users/:id/keys/*key
//POST /users/:id/keys/*key
//PUT /users/:id/keys/*key
//DELETE /users/:id/keys/*key
//
//GET /sessions/:id
//DELETE /sessions/:id
//GET /sessions/:id/keys
//GET /sessions/:id/keys/*key
//POST /sessions/:id/keys/*key
//PUT /sessions/:id/keys/*key
//DELETE /sessions/:id/keys/*key
