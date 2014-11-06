package com.simplepastry

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import java.security.MessageDigest
import com.pastry.Node
import com.pastry.PastryNode
import java.math.BigInteger
import com.pastry.BaseNValue
import scala.util.Random
import com.pastry.Application
import com.pastry.BaseNValue
import com.pastry.PastryInit
import com.pastry.Credentials

/**
 * This class will simulate a pastry network.
 * @param n				The number of nodes in the pastry network
 * @param base			The base of the numeric system used by this pastry
 * 						network
 * @param l				A parameter that affects the size of the leaf set and 
 * 						neighborhood set
 * @paran messageCount	The total number of messages to be sent
 */
class BootStrapper(n:Int = 10, base:Int = 4, l:Int = 16, messageCount:Int)
{
  case class Tuple(id:BigInt, node:ActorRef){}
  
  /* b should ideally be related to the number of digits in our base. This will
   * be the log[base 2] of our base so that the routing table can have a 
   * number of columns equal to the number of digits in the base. This assumes
   * that the base is a multiple of 2 */
  val b = (Math.log(base)/Math.log(2)).toInt
  var sys:ActorSystem = _
  var master:ActorRef = _
  var nodes:IndexedSeq[Tuple] = IndexedSeq()
  val sha = MessageDigest.getInstance("SHA-256")
  
  def start():Unit =
  {
    var id:BigInt = null
    var node:ActorRef = null
    var neighbor:List[Node[ActorRef]] = List()
    var first:BigInt = 0
    
    sys = ActorSystem("pastrysystem") 
    master = sys.actorOf(Props(classOf[Master], n, base), name = "master")
    
    println("creating network")
    /* Create network */
    for(i <- 0 until n)
    {
      sha.digest(("pastrysyste/usr/%s".format(i)).getBytes)
      id = i + 1
      if(nodes.size > 0) first = findClosest(id, nodes.toList)
      node = sys.actorOf(Props(classOf[SimplePastry], BigInt.apply(id.toByteArray), n, base, b, l, BigInt.apply(first.toByteArray), neighbor.toList, master))
      master ! AddNode(id, node)
      neighbor = List(Node[ActorRef](new BaseNValue(id, base), node))
      nodes = nodes :+ Tuple(id, node)
    }
    println("network created")
    
    Thread.sleep(1000)
    
    println("sending mesages")
    /* Send messages */
    var rng = new Random(50)
    var to:BigInt = 0
    var from:BigInt = 0
    var same = true
    //nodes.toList.foreach(println(_))
    for(i <- 0 until messageCount)
    {
      while(same)
      {
        from = nodes(rng.nextInt.abs % n).id
        to = nodes(rng.nextInt.abs % n).id
        if(to != from) same = false
      }
      same = true
      println("Sending from " + from + " to " + to)
      master ! StartSend(from, to)
    }
    println("messages sent")
    
    Thread.sleep(1000)
    
    master ! PrintTotalMessages
    
    println("print sent")
  }
  
  def findClosest(id:BigInt, list:List[Tuple]):BigInt =
  {
    var closest:BigInt = null
    var diff:BigInt = 0
    
    if(list.size > 0)
    {
      diff = (id - list(0).id).abs
      closest = list(0).id
    }
    
    for(i <- 1 until list.size)
    {
      if((id - list(i).id).abs < diff)
      {
        diff = (id - list(i).id).abs
        closest = list(i).id
      }
    }
    return closest
  }
  
}