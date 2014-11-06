package com.simplepastry

import scala.collection.concurrent.TrieMap
import akka.actor.Actor
import akka.actor.ActorRef
import com.pastry.BaseNValue
import com.pastry.Route
import scala.actors.threadpool.AtomicInteger
import scala.collection.mutable.Queue
import com.pastry.Node
import com.pastry.Credentials
import com.pastry.PastryInit
import akka.event.Logging

/**
 * The master keeps track of SimplePastry
 * @param n	The largest value that will be in the  
 */
class Master(n:Int, base:Int) extends Actor
{
  case class Data(node:ActorRef, id:BaseNValue, messagesSent:AtomicInteger, messagesReceived:AtomicInteger, totalHops:AtomicInteger, messagesForwarded:AtomicInteger)
  
  /* Logger */
  var log = Logging(context.system, this)
  
  /* Maps a nodes id to the relevant data */
  val map:TrieMap[BigInt, Data] = new TrieMap[BigInt, Data]
  
  /* Initialization queue */
  val initQueue:Queue[Node[ActorRef]] = Queue()
  
  var nodeInitializing = false
  
  def receive =
  {
    case ReceivedSimple(id, hop) => 
      println("master received simple" + id + " " + hop)
      map.get(id).get.totalHops.addAndGet(hop)
      map.get(id).get.messagesReceived.addAndGet(hop)
    case Forward(id) => map.get(id).get.messagesForwarded.incrementAndGet()
    case StartSend(from, to) => startSend(from, to)
    case AddNode(id, node) => 
      map += ((id, Data(node, new BaseNValue(id, base), new AtomicInteger(0), new AtomicInteger(0), new AtomicInteger(0), new AtomicInteger(0))))
      initQueue += new Node(new BaseNValue(id, base), node)
      println("received addnode for " + id)
      if(!nodeInitializing) 
      {
        println("")
        var child = initQueue.dequeue
        child.node ! PastryInit(new Credentials(child.id), new SimpleApplication)
        nodeInitializing = true
      }
    case PrintTotalMessages => printTotalMessages
    case Initialized(id) =>
      println("received initialized from " + id)
      if(!initQueue.isEmpty)
      {
        var node = initQueue.dequeue
        node.node ! PastryInit(new Credentials(node.id), new SimpleApplication)
      }
      else nodeInitializing = false
    case x =>
  }
  
  def startSend(from:BigInt, to:BigInt):Unit =
  {
    val data:Data = map.get(from).get
    if (data != null) data.node ! Route(new BaseNValue(to, base), Simple(0))
    data.messagesSent.incrementAndGet()
  }
  
  def printTotalMessages() =
  {
    println("aggregating messages")
    var totalMessages = 0
    map.foreach(totalMessages += _._2.totalHops.get())
    println("total messages: " + totalMessages)
  }
}