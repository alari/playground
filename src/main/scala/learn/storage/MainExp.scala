package learn.storage

import akka.actor.{ActorSystem, Props}
import akka.cluster.Cluster
import learn.storage.http.HttpClusterActor

object MainExp extends App {

  implicit val system = ActorSystem("http-system")

  val cluster = Cluster.get(system)
  cluster.join(cluster.selfAddress)

  system.actorOf(Props[HttpClusterActor], "cluster-listener")
}
