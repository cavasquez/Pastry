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

/**
 * This class will simulate a pastry network.
 * @param n				The number of nodes in the pastry network
 * @param base			The base of the numeric system used by this pastry
 * 						network
 * @param l				A parameter that affects the size of the leaf set and 
 * 						neighborhood set
 */
class BootStrapper(n:Int = 10, base:Int = 4, l:Int = 16, messageCount:Int)
{
  case class Tuple(id:BigInt, node:ActorRef){}
  
  /* b should ideally be related to the number of digits in our base. This will
   * be the log[base 2] of our base so that the routing table can have a 
   * number of columns equal to the number of digits in the base. This assumes
   * that the base is a multiple of 2 */
  val b = Math.log(base).toInt
  var sys:ActorSystem = _
  var master:ActorRef = _
  var nodes:IndexedSeq[Tuple] = _
  val sha = MessageDigest.getInstance("SHA-256")
  
  def start():Unit =
  {
    var id:BigInt = null
    var node:ActorRef = null
    var neighbor:List[Node[ActorRef]] = List(null)
    var first:BigInt = null
    
    sys = ActorSystem("pastrysystem") 
    master = sys.actorOf(Props(classOf[Master], n, base), name = "master")
    
    /* Create network */
    for(i <- 0 until n)
    {
      sha.digest(("pastrysyste/usr/%s".format(i)).getBytes)
      id = BigInt.apply(sha.digest(("pastrysyste/usr/%s".format(i)).getBytes)).abs
      first = findClosest(id, neighbor)
      node = sys.actorOf(Props(classOf[PastryNode], BigInt.apply(id.toByteArray), n, base, b, l, first, neighbor))
      master ! AddNode(id, node)
      neighbor = List(Node[ActorRef](new BaseNValue(id, base), node))
      nodes :+ Tuple(id, node)
    }
    
    /* Send messages */
    var rng = new Random()
    var to:BigInt = 0
    var from:BigInt = 0
    for(i <- 0 until messageCount)
    {
      from = neighbor(rng.nextInt % n).id.toBase10
      to = neighbor(rng.nextInt % n).id.toBase10
      master ! StartSend(from, to)
    }
  }
  
  def findClosest(id:BigInt, list:List[Node[ActorRef]]):BigInt =
  {
    var closest:BigInt = null
    var diff:BigInt = 0
    
    if(list.size > 0)
    {
      diff = (id - list(0).id.toBase10).abs
      closest = list(0).id.toBase10
    }
    
    for(i <- 1 until list.size)
    {
      if((id - list(i).id.toBase10).abs < diff)
      {
        diff = (id - list(i).id.toBase10).abs
        closest = list(i).id.toBase10
      }
    }
    return closest
  }
  
}