package learn

import akka.actor.ActorSystem
import akka.cluster.Cluster
import akka.http.Http
import akka.http.model.HttpResponse
import akka.http.server.Directives._
import akka.http.server._
import akka.stream.ActorFlowMaterializer

import scala.concurrent.ExecutionContext.Implicits.global

object EchoApp extends App {

  implicit val system = ActorSystem("http-system")
  implicit val materializer = ActorFlowMaterializer()

  import akka.http.server.RoutingSettings.default

  implicit val setup = RoutingSetup.apply

  val router: Route =
    get {
      complete(HttpResponse(entity = "PONG! "+testScalaz))
    }

  def testScalaz: String = {
    import scalaz._
    import Scalaz._

    def addString(s: String) = Kleisli[Option, Int, String] {
      a =>
        (a + s).some
    }

    def markPong = Kleisli[Option, String, String] {
      a => (a + " - pong").some
    }

    def pongPong = addString("-p-o-n-g") andThen markPong

    type V[T] = \/[String,T]

    def otherEffect = Kleisli[V, String, String] {
      s =>
        if(s == "ok") \/-("right")
        else -\/("left")
    }

    case class Domain[T](v: String)

    sealed trait TState
    sealed trait TOddState extends TState
    sealed trait TEvenState extends TState
    trait T_0 extends TEvenState
    trait T_1 extends TOddState
    trait T_2 extends TEvenState
    trait T_3 extends TOddState

    type TOddDomain = Domain[_ <: TOddState]
    type TEvenDomain = Domain[_ <: TEvenState]
    type T1Domain = Domain[T_1]
    type T0Domain = Domain[T_0]

    def printOdd = Kleisli[V, TOddDomain, String]{
      t => \/-("odd "+t.v)
    }

    def printEven = Kleisli[V, TEvenDomain, String]{
      t => \/-("even "+t.v)
    }

    def toOdd = Kleisli[V, T0Domain, TOddDomain]{
      t => t.copy[T_1]().right.map(identity[TOddDomain])
    }

    printOdd.run( Domain[T_1]("1") )
    printEven.run( Domain[T_2]("1") )


    pongPong.run(1).getOrElse("ping")
  }

  Http(system).bind(interface = "localhost", port = 9393).startHandlingWith(Route.handlerFlow(router))

  val cluster = Cluster.get(system)
  cluster.join(cluster.selfAddress)
}
