package com.simplepastry

import scala.collection.concurrent.TrieMap
import akka.actor.Actor
import akka.actor.ActorRef
import com.pastry.BaseNValue
import com.pastry.Route
import scala.actors.threadpool.AtomicInteger

/**
 * The master keeps track of SimplePastry
 * @param n	The largest value that will be in the  
 */
class Master(n:Int, base:Int) extends Actor
{
  case class Data(node:ActorRef, id:BaseNValue, var messagesSent:AtomicInteger, var messagesReceived:AtomicInteger, var totalHops:AtomicInteger, var messagesForwarded:AtomicInteger)
  
  /* Maps a nodes id to the relevant data */
  val map:TrieMap[BigInt, Data] = new TrieMap[BigInt, Data]
  
  def receive =
  {
    case ReceivedSimple(id, hop) => 
      map.get(id).get.totalHops.addAndGet(hop)
      map.get(id).get.messagesReceived.addAndGet(hop)
    case Forward(id) => map.get(id).get.messagesForwarded.incrementAndGet()
    case StartSend(from, to) => startSend(from, to)
    case AddNode(id, node) => map += ((id, Data(node, new BaseNValue(id, base), new AtomicInteger(0), new AtomicInteger(0), new AtomicInteger(0), new AtomicInteger(0))))
    case x =>
  }
  
  def startSend(from:BigInt, to:BigInt):Unit =
  {
    val data:Data = map.get(from).get
    if (data != null) data.node ! Route(new BaseNValue(to, base), Simple(0))
    data.messagesSent.incrementAndGet()
  }
}