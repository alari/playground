package learn.shared

import akka.actor.{Actor, ActorLogging}
import akka.contrib.pattern.ClusterSharding

class SingleActor extends Actor with ActorLogging {
  var i: Int = 0

  def receive = {
    case SingleActor.End =>
      log.warning("GOT END!!!!!!!!")


    case m =>
      log.info("Singleton got " + m + " from " + sender())
      i = i + 1
      ClusterSharding(context.system).shardRegion("ash") ! i

  }
}

object SingleActor {

  case object End

}