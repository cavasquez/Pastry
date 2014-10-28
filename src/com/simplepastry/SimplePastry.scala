package com.simplepastry

import com.pastry.PastryNode
import com.pastry.BaseNValue
import com.pastry.PastryMessage
import com.pastry.LeafSetTest
import akka.actor.ActorRef
import com.pastry.Node
import com.pastry.LeafSet
import com.pastry.Route

class SimplePastry(nodeID:BigInt, n:Int, base:Int, b:Int, l:Int, firstNode:BigInt, neighbor:List[Node[ActorRef]], master:ActorRef) extends PastryNode(nodeID, n, base, b, l, firstNode, neighbor) 
{  
  type PMessage = com.pastry.Message
  
  /**
   * Called by Pastry when a message is received and the local node's ID is 
   * numerically closest to key, among all live nodes.
   */
  protected def deliver(key:BaseNValue, mssg:PMessage):Unit =
  {
    mssg match
    {
      case Simple(hop) => master ! ReceivedSimple(nodeID, hop)
      case x => /* do nothing */
    }
  }
  
  /**
   * Called by Pastry just before a message is forwarded to the node with 
   * ID = nextID. The application may change the contents of the message or the 
   * value of nextID (via pack) by returning the modified PastryMessage. Setting 
   * the nextID to NULL terminates the message at the local node.
   */
  protected def forward(key:BaseNValue, nextID:BaseNValue, mssg:PMessage):PastryMessage =
  {
    var next = nextID
    var message:PMessage = null
    
    mssg match
    {
      case Simple(hop) => 
        message = Simple(hop+1)
        master ! Forward(nodeID)
      case x => /* do nothing */
    }
    return PastryMessage(next, message)
  }
  
  /**
   * Called by Pastry whenever there is a change in the local nodes' leaf set. 
   * This provides the application with an opportunity to adjust 
   * application-specific invariants based on the leaf set.
   */
  protected def newLeafs(leaf:LeafSet[ActorRef]):Unit = { /* do nothing */ }
}