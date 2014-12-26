package learn.shared

import akka.actor.{ActorSystem, Props}
import akka.cluster.Cluster

object SharedMain{
  val system = systemInit("launch")
  val cluster = Cluster.get(system)
  cluster.join(cluster.selfAddress)

  (1 to 2).foreach { i => Cluster.get(systemInit("secondary" + i)).join(cluster.selfAddress)}


  def systemInit(name: String): ActorSystem = {
    val system = ActorSystem("learn")

    system.actorOf(Props[LaunchActor], name = "launch")

    system
  }
}
