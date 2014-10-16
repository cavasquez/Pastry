package com.pastry

import akka.actor.ActorRef
import scala.reflect.ClassTag

/**
 * @param nodeID	The nodeID of the owning Pastry node
 * @param b			The b value that will be used by this Pastry network
 * @param n			The maximum number of nodes in this Pastry network
 * @param owner		The ActorRef of the owning node
 */
class RoutingTable[T:ClassTag](nodeID:BaseNValue, b:Int = 4, n:Int = 10, owner:T = null)
{
  if(b != nodeID.base) throw new IllegalArgumentException("nodeID has a different base than b")
  private val rowSize = Math.ceil(Math.log(n)/Math.log(Math.pow(2, b))).toInt
  private val colSize = Math.pow(2, b).toInt
  private val table = makeRoutingTable(rowSize, colSize, nodeID, owner)
  
  /**
   * Creates a routing table of the provided row and column size that contains
   * the owner.
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @param nodeID	The node ID of the owner
   * @param	owner	The owning node of this table
   * @return		Returns a 2D array that represents an initial Routing Table
   * 				for the Pastry protocol
   */
  def makeRoutingTable(rowSize:Int, colSize:Int, nodeID:BaseNValue, owner:T):Array[Array[T]] =
  {
    var table = makeTable(rowSize, colSize)
    
    return table
  }
  
  /**
   * Returns a 2D array of type T
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @return 		Returns a 2D array of type T
   */
  def makeTable(rowSize:Int, colSize:Int):Array[Array[T]] =
  {
    var rTable = new Array[Array[T]](rowSize)
    for(i:Int <- 0 until rowSize)
    {
      for(j:Int <- 0 until colSize) { rTable(i) = new Array[T](colSize) }
    }
    return rTable
  }
  
  /**
   * Inserts the node with the given id into the Routing Table. The insert will
   * attempt to insert node into the position that matches id with the parent.
   * It will not attempt to insert node into a "lower" matching spot. It will
   * only be inserted into the position who matches the prefix id the closest
   * to nodeID.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param id		The ID of the inserted node.
   * @param node	The node being inserted
   * @returner		Returns whether or not the insert was successful
   */
  def insertNode(id:Int, node:T):Boolean =
  {
    return false
  }
}