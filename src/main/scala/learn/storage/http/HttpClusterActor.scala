package learn.storage.http

import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, ActorRef, Props, Actor}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{MemberUp, MemberEvent, InitialStateAsEvents}
import akka.contrib.pattern.{ShardRegion, ClusterSharding}
import akka.http.Http
import akka.http.server.Directives._
import akka.http.server.PathMatchers.Segment
import akka.http.server._
import akka.stream.FlowMaterializer

import scalaz.Monoid

class HttpClusterActor extends Actor with KeyValueHttp {

  val cluster = Cluster.get(context.system)
  implicit val materializer = FlowMaterializer()

  import akka.http.server.RoutingSettings.default
  import context.dispatcher

  implicit val setup = RoutingSetup.apply

  implicit val timeout = akka.util.Timeout(1, TimeUnit.SECONDS)

  // subscribe to cluster changes, re-subscribe when restart
  override def preStart(): Unit = {
    //#subscribe
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent])
    //#subscribe
  }

  override def postStop(): Unit = cluster.unsubscribe(self)


  def receive = {
    case MemberUp(member) if member.address == cluster.selfAddress =>
      init()

  }

  def init(): Unit = {
    val kvStoreRef = HttpClusterActor.startRegion(context.system)

    val routes: Route =
      pathPrefix(Segment) { id =>
        routeKeyValue[String](id, kvStoreRef)
      }

    Http(context.system).bind(interface = "localhost", port = 8383).startHandlingWith(Route.handlerFlow(routes))
  }

}

object HttpClusterActor {



  private[http] def startRegion(implicit system: ActorSystem): ActorRef = system.actorOf(Props[KeyValueActor], name = "kva") /*ClusterSharding(system).start(
    typeName = "kv-store",
    entryProps = Some(Props(new KeyValueActor[String])),

    idExtractor = idExtractor,
    shardResolver = shardResolver)*/

  val idExtractor: ShardRegion.IdExtractor = {
    case (id: String, msg) ⇒
      println("extracted id = "+id)
      (id, msg)

    case m =>
      println("???????? "+m)
      "1" -> "2"
  }

  val shardResolver: ShardRegion.ShardResolver = {
    case (id: String, _) ⇒
      println("shard = " + (id.hashCode % 12))
      (id.hashCode % 12).toString
  }
}