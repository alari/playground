package learn.shared

import akka.actor.{ActorLogging, Actor}

class ShardedActor extends Actor with ActorLogging {
  def receive = {
    case m =>
      log.info("Sharded Got "+m)
  }
}
