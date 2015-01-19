package learn.storage.http

import akka.actor.ActorRef
import akka.http.marshalling.ToResponseMarshaller
import akka.http.server.Directives._
import akka.http.server.Route
import akka.http.unmarshalling.FromRequestUnmarshaller
import akka.pattern.ask
import learn.storage.KeyValueEventSourced

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

trait KeyValueHttp {
  def routeKeyValue[T: ToResponseMarshaller : FromRequestUnmarshaller : ClassTag](id: String, actor: ActorRef)(implicit to: akka.util.Timeout, ec: ExecutionContext): Route =
  {
    path("values" / Segments) { keys =>
      val key = keys.mkString("/")
      get {
        ctx =>
          for {
            v <- (actor ? (id -> KeyValueEventSourced.GetValue(key))).mapTo[Option[T]]
            r <- ctx.complete(v)
          } yield r
      } ~ (post & entity(implicitly[FromRequestUnmarshaller[T]])) { v =>
        ctx =>
          for {
            v <- (actor ? (id -> KeyValueEventSourced.SetValue(key, v))).mapTo[Option[T]]
            r <- ctx.complete(v)
          } yield r
      }
    }
  }

  // GET /values/*id
  // POST /values/*id
  // PUT /values/*id
  // DELETE /values/*id

  // DELETE /keys/*id
  // GET /keys/*id
  // GET /keys
}

