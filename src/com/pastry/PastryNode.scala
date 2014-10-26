package com.pastry

import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef

abstract class PastryNode(nodeID:BigInt, b:Int = 4, l:Int = 16) extends Actor
{
  /* L:ist of this nodes neighbors */
  protected val route:RoutingTable[ActorRef] = null
  protected val leaf:LeafSet[ActorRef] = null
  protected val neighborhood:NeighborhoodSet[ActorRef] = null
  protected val ID:BaseNValue = new BaseNValue(nodeID, b)
  
  /**
   * Package will be used to pass the neighbor and message between functions
   */
  protected case class Package(var key:BaseNValue, var message:Message)
  
  def receive =
  {
    case Route(key, message) => route(Package(key, message))
    case Join(node:Node[ActorRef], hop) => join(node, hop)
    case Init(cred, app) => pastryInit(cred, app)
    case x =>
  }
  
  /**
    * If node.ID is in leaf, forward to leaf such that D - leafnode is minimal
    * else, use routing table:
    * 	if there exists a node in route at row = ID.longestCommonPrefix(node.ID)
    *  and col = node.id's "next prefix" of the common prefix,
    *  then: forward to that node
    *  
    *  else: forward to a node in leaf, routing table, or neighborhood such 
    *  that the longest common prefix of node.id and the given element is
    *  greater than or equal to the longest common prefix of ID
    */
  def findRoute(key:BaseNValue):Node[ActorRef] =
  {
    
    var node:Node[ActorRef] = null
    
    /* Check to see if key is within range our our leaf */
    node = leaf.findClosest(id = key)
    if(node == null)
    {
      /* Check to see if a node with a similar key can be found in our route 
       * table. Similar = shares the same first n digits as key where n is the
       * longest matching prefix between key and nodeID + 1 */
      node = route.findRouteableNode(id = key)
      if(node == null)
      {
        /* This is a rare case. Now we will check all three tables to find a
         * node with key that has a number of matching prefix digits greater 
         * than or equal to the longest matching prefix between key and nodeID */
        var firstTime = true
        var temp = leaf.findLongestMatchingPrefix(id = key)
        node = route.findLongestMatchingPrefix(id = key)
        
        while(firstTime)
        {
          if(node.id.numOfDigits(base = key.base) == temp.id.numOfDigits(base = key.base))
          {
            if(node.id.longestMatchingPrefix(key) < temp.id.longestMatchingPrefix(key)) node = temp
          }
          else
          {
            /* Choose the node with the smallest difference in digits */
            if(node.id.numOfDigits(base = key.base) - key.numOfDigits(base = key.base).abs > (node.id.numOfDigits(base = key.base) - key.numOfDigits(base = key.base).abs )) node = temp
          }
          
          temp = neighborhood.findLongestMatchingPrefix(id = key) 
          firstTime = false
        }
      }
    }
    return node
  }
  
  /**
   * Causes the local node to join an existing Pastry network (or start a new 
   * one), initialize all relevant state, and return the local node's nodeID.
   */
  private[pastry] def pastryInit(cred:Credentials, app:Application):BaseNValue =
  {
    return null
  }
  
  /**
   * Causes pastry to route the given message to the node with ID numerically 
   * closest to the key, among all live Pastry nodes.
   * @param key		The key of the node being routed to
   * @param message	The message being sent to the node with id key
   */
  private[pastry] def route(pack:Package):Unit =
  {
    /* Check to see if package has arrived at destination. If not, forward it
     * to this node and then send it off to the next node. If it has, deliver
     * the package */
    if(pack.key != ID)
    {
      var nextHop = findRoute(pack.key)
      var newPack = new Package(nextHop.id, pack.message)
      forward(pack.key, newPack)
      if(newPack != null) nextHop.node ! Route(newPack.key, newPack.message)
    }
    else deliver(pack)
  }
  
  /**
   * Send state tables to node and possibly route to target
   */
  private[pastry] def join(node:Node[ActorRef], hop:Int):Unit = 
  {
    
  }
  
  /**
   * Called by Pastry when a message is received and the local node's ID is 
   * numerically closest to key, among all live nodes.
   */
  protected def deliver(pack:Package):Unit
  
  /**
   * Called by Pastry just before a message is forwarded to the node with 
   * ID = nextID. The application may change the contents of the message or the 
   * value of nextID (via pack). Setting the nextID to NULL terminates the 
   * message at the local node.
   */
  protected def forward(key:BaseNValue, pack:Package):Unit
  
  /**
   * Called by Pastry whenever there is a change in the local nodes' leaf set. 
   * This provides the application with an opportunity to adjust 
   * application-specific invariants based on the leaf set.
   */
  protected def newLeafs(leaf:LeafSet[ActorRef])
}