package com.pastry

import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef

class PastryNode(nodeID:Long, b:Int = 4, l:Int = 16) extends Actor
{
  /* L:ist of this nodes neighbors */
  protected val neighbor = ArrayBuffer.empty[ActorRef]
  protected val route:RoutingTable[ActorRef] = null
  
  def receive =
  {
    case x =>
  }
}