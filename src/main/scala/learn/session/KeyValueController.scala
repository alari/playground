package learn.session

import akka.actor.ActorSystem
import akka.http.Http
import akka.http.marshalling.Marshaller._
import akka.http.model._
import akka.http.server.Directives._
import akka.http.server._
import akka.http.unmarshalling.{FromRequestUnmarshaller, Unmarshaller}
import akka.stream.FlowMaterializer

import scala.concurrent.Future


trait SessionController {
  val usersRoute: Route =
    path("users" / Segment)(userRoute)

  case class Exp(data: String)

  implicit val expUnm: FromRequestUnmarshaller[Exp] = Unmarshaller[akka.http.model.HttpRequest, Exp] { req =>
    req.entity.contentType().mediaType match {
      case MediaTypes.`application/json` =>
        Future.successful(Exp(req.entity.toString))
      case _ =>
        Future.failed(new IllegalArgumentException("Can parse json only"))
    }
  }

  def userRoute(id: String): Route =
    pathEndOrSingleSlash {
      get {
        complete(HttpResponse(entity = "GET " + id))
      } ~ (post & entity(expUnm)) { exp =>
        ctx =>

          ctx.complete(HttpResponse(entity = "POST " + id + exp))
      } ~ delete {
        ctx =>
          ctx.complete(HttpResponse(entity = "DELETE " + id))
      }
    } ~ pathSuffix("sessions") {
      get {
        complete(HttpResponse(entity = "GET sessions " + id))
      } ~ post {
        complete(HttpResponse(entity = "CREATE NEW SESSION " + id))
      }
    }

  def sessionRoute(id: String): Route =
    pathEndOrSingleSlash {
      get {
        complete(HttpResponse(entity = "GET SESSION " + id))
      } ~ delete {
        complete(HttpResponse(entity = "DELETE SESSION " + id))
      }
    }

  val sessionsRoute =
    path("sessions" / Segment)(sessionRoute)

  //GET /users/:id
  //POST /users/:id
  //GET /users/:id/sessions
  //POST /users/:id/sessions
  //DELETE /users/:id
  //GET /sessions/:id
  //DELETE /sessions/:id
}


object KeyValueController extends SessionController {

  implicit val system = ActorSystem("http-system")
  implicit val materializer = FlowMaterializer()

  import akka.http.server.RoutingSettings.default
  import learn.session.KeyValueController.system.dispatcher

  implicit val setup = RoutingSetup.apply

  val router: Route =
    sessionsRoute ~ usersRoute ~
      get {
        complete(HttpResponse(entity = "HIPONG!"))
      }

  //Http(system).bind(interface = "localhost", port = 8383).startHandlingWith(Route.handlerFlow(router))

}