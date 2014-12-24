package learn.shared

import akka.actor.{Actor, ActorLogging, Props}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import akka.contrib.pattern.{ClusterSharding, ClusterSingletonManager, ClusterSingletonProxy, ShardRegion}

class LaunchActor extends Actor with ActorLogging {

  val cluster = Cluster(context.system)

  // subscribe to cluster changes, re-subscribe when restart
  override def preStart(): Unit = {
    //#subscribe
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember])
    //#subscribe
  }

  override def postStop(): Unit = cluster.unsubscribe(self)

  def receive = {
    case MemberUp(member) =>
      if (member.address == cluster.selfAddress) {
        init()
        log.info("SELF UP!!!")
      }

      // log.info("Member is Up: {}", member.address)

      context.system.actorSelection("/user/singleProxy") ! "Up!"

    case UnreachableMember(member) =>
      // log.info("Member detected as unreachable: {}", member)

      context.system.actorSelection("/user/singleProxy") ! "Unreachable!"

    case MemberRemoved(member, previousStatus) =>
      // log.info("Member is Removed: {} after {}", member.address, previousStatus)

      context.system.actorSelection("/user/singleProxy") ! "Removed!"
    case _: MemberEvent => // ignore

    case i: Int =>
      log.info("got " + i + " from singleton")
  }

  def init(): Unit = {

    val idExtractor: ShardRegion.IdExtractor = {
      case id: Int ⇒ (id.toString, id)
    }

    val shardResolver: ShardRegion.ShardResolver = {
      case id: Int ⇒ (id % 12).toString
    }

    ClusterSharding(context.system).start(
      typeName = "ash",
      entryProps = Some(Props[ShardedActor]),
      idExtractor = idExtractor,
      shardResolver = shardResolver)

    context.system.actorOf(ClusterSingletonManager.props(
      singletonProps = Props(classOf[SingleActor]),
      singletonName = "single",
      terminationMessage = SingleActor.End,
      role = None),
      name = "singleton")

    context.system.actorOf(ClusterSingletonProxy.props(
      singletonPath = "/user/singleton/single",
      role = None),
      name = "singleProxy")
  }
}