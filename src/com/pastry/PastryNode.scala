package com.pastry

import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef

abstract class PastryNode(nodeID:Long, b:Int = 4, l:Int = 16) extends Actor
{
  /* L:ist of this nodes neighbors */
  protected val neighbor = ArrayBuffer.empty[ActorRef]
  protected val route:RoutingTable[ActorRef] = null
  protected val leaf:LeafSet[ActorRef] = null
  protected val neighborhood:NeighborhoodSet[ActorRef] = null
  protected val ID:BaseNValue = new BaseNValue(nodeID, b)
  
  def receive =
  {
    case x =>
  }
  
  /**
   * Causes the local node to join an existing Pastry network (or start a new 
   * one), initialize all relevant state, and return the local node's nodeID.
   */
  private[pastry] def pastryInit():BaseNValue =
  {
    return null
  }
  
  /**
   * Causes pastry to route the given message to the node with ID numerically 
   * closest to the key, among all live Pastry nodes.
   */
  private[pastry] def route(key:BaseNValue, message:Message):Unit =
  {
    /*
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
  }
  
  /**
   * Send state tables to node and possibly route to target
   */
  private[pastry] def join(node:Node[ActorRef]):Unit = 
  {
    
  }
  
  /**
   * Called by Pastry when a message is received and the local node's ID is 
   * numerically closest to key, among all live nodes.
   */
  protected def deliver(key:BaseNValue, message:Message):Unit
  
  /**
   * Called by Pastry just before a message is forwarded to the node with 
   * ID = nextID. The application may change the contents of the message or the 
   * value of nextID. Setting the nextID to NULL terminates the message at the
   * local node.
   */
  protected def forward(key:BaseNValue, nextID:BaseNValue, message:Message):Unit
  
  /**
   * Called by Pastry whenever there is a change in the local nodes' leaf set. 
   * This provides the application with an opportunity to adjust 
   * application-specific invariants based on the leaf set.
   */
  protected def newLeafs(leaf:LeafSet[ActorRef])
}