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
  protected var firstNeighbor:Node[ActorRef];
  
  def receive =
  {
    case Route(key, message) => route(key, PastryMessage(key, message))
    case Join(target, node:Node[ActorRef], hop) => join(target, node, hop)
    case PastryInit(cred, app) => pastryInit(cred, app)
    case StateTables(hop, nodeID, leaf, route, neighborhood) => stateTables(hop, nodeID, leaf, route, neighborhood)
    case PastryMessage(key, mssg) => deliver(key, mssg)
    case x => /* do nothing */
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
    /* Assume that NeighborhoodSet already has some values to begin with. */
    return cred.id 
  }
  
  /**
   * Causes pastry to route the given message to the node with ID numerically 
   * closest to the key, among all live Pastry nodes.
   * @mssg	The key and message
   */
  private[pastry] def route(key:BaseNValue, mssg:PastryMessage):Unit =
  {
    /* Check to see if package has arrived at destination. If not, forward it
     * to this node and then send it off to the next node. If it has, deliver
     * the package */
    if(key != ID)
    {
      var nextHop = findRoute(key)
      var newMssg = forward(mssg.key, nextHop.id, mssg.message)
      if(newMssg.key != null) nextHop.node ! Route(newMssg.key, newMssg.message)
    }
    else self ! mssg
  }
  
  /**
   * Send state tables to node and possibly route to target
   */
  private[pastry] def join(target:BaseNValue, node:Node[ActorRef], hop:Int):Unit = 
  {
    /* Check if the message has arrived at destination. If not, route it. */
    if(ID != target)
    {
      var nextHop = findRoute(target)
      nextHop.node ! Join(target, node, hop + 1)
    }
    
    /* Send requester this nodes tables */
    node.node ! stateTables(hop + 1, ID, leaf, route, neighborhood)
    /* Now add the new node to this nodes tables */
    leaf += node
    route += node
    neighborhood  += node
  }
  
  /**
   * Receives state tables from other nodes and modifies own state tables.
   * @hop			The number of neighbors between the sender and this node
   * @nodeID		The id of the sender
   * @leaf			The leaf set of the sender
   * @route			The routing table of the sender
   * @neighborhood	The neighborhood set of the sender
   */
  private def stateTables(hop:Int, nodeID:BaseNValue, leaf:LeafSet[ActorRef], route:RoutingTable[ActorRef], neighborhood:NeighborhoodSet[ActorRef]):Unit = 
  {
    
  }
  
  /**
   * Called by Pastry when a message is received and the local node's ID is 
   * numerically closest to key, among all live nodes.
   */
  protected def deliver(key:BaseNValue, mssg:Content):Unit
  
  /**
   * Called by Pastry just before a message is forwarded to the node with 
   * ID = nextID. The application may change the contents of the message or the 
   * value of nextID (via pack) by returning the modified PastryMessage. Setting 
   * the nextID to NULL terminates the message at the local node.
   */
  protected def forward(key:BaseNValue, nextID:BaseNValue, message:Content):PastryMessage
  
  /**
   * Called by Pastry whenever there is a change in the local nodes' leaf set. 
   * This provides the application with an opportunity to adjust 
   * application-specific invariants based on the leaf set.
   */
  protected def newLeafs(leaf:LeafSet[ActorRef])
}