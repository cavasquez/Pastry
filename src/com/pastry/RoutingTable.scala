package com.pastry

import akka.actor.ActorRef
import scala.reflect.ClassTag

/**
 * @param nodeID	The nodeID of the owning Pastry node
 * @param b			The b value that will be used by this Pastry network
 * @param n			The maximum number of nodes in this Pastry network
 * @param owner		The ActorRef of the owning node
 */
class RoutingTable[T:ClassTag](val nodeID:BaseNValue, b:Int = 4, n:Int = 10, owner:T = null)
{
  if(b < 1) throw new IllegalArgumentException("b must be greater than 0")
  private val rowSize = Math.ceil(Math.log(n)/Math.log(Math.pow(2, b))).toInt
  if(rowSize < nodeID.numOfDigits()) throw new IllegalArgumentException("n is not big enough to contain the number of digits in nodeID")
  private val colSize = Math.pow(2, b).toInt
  if(colSize > nodeID.base) throw new IllegalArgumentException("cannot have more columns than the size of the base")
  private val table = makeRoutingTable(rowSize, colSize, nodeID, owner)
    
  /**
   * Creates a routing table of the provided row and column size that contains
   * the owner.
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @param nodeID	The node ID of the owner
   * @param	owner	The owning node of this table
   * @param diff	The difference between row size and the number of digits in 
   * 				nodeID.
   * @return		Returns a 2D array that represents an initial Routing Table
   * 				for the Pastry protocol
   */
  def makeRoutingTable(rowSize:Int, colSize:Int, nodeID:BaseNValue, owner:T):Array[Array[Route[T]]] =
  {
    var table = makeTable(rowSize, colSize)
    var offset = diff(nodeID, rowSize) /* the offset cannot be less than 0 */
    var difference = rowSize - nodeID.numOfDigits()
    for(i:Int <- rowSize-1 to offset by -1) { table(i)(nodeID.nextDigit(n = i - difference)) = new Route(nodeID, owner) }
    for(i:Int <- offset - 1 to 0 by -1) { table(i)(0) = new Route(nodeID, owner) }
    return table
  }
  
  /**
   * Returns a 2D array of type T
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @return 		Returns a 2D array of type T
   */
  def makeTable(rowSize:Int, colSize:Int):Array[Array[Route[T]]] =
  {
    var rTable = new Array[Array[Route[T]]](rowSize)
    for(i:Int <- 0 until rowSize) { rTable(i) = new Array[Route[T]](colSize) }
    return rTable
  }
  
  /**
   * Returns the difference between the number of digits in value and n. This 
   * method will never return less than 0
   * @param value	a BaseNValue
   * @param n		the number whose value is being subtracted from
   * @return 		the difference between the number of digits in value and n
   */
  def diff(value:BaseNValue, n:Int):Int = if(n >= value.numOfDigits()) n - value.numOfDigits() else 0
  
  /**
   * Inserts the node with the given id into the Routing Table. The insert will
   * attempt to insert node into the position that matches id with the parent.
   * It will not attempt to insert node into a "lower" matching spot. It will
   * only be inserted into the position who matches the prefix id the closest
   * to nodeID.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param id		The ID of the inserted node.
   * @param ownID	The ID of the node that "owns" this table
   * @param node	The node being inserted
   * @returner		Returns whether or not the insert was successful
   */
  def insertNode(table:Array[Array[Route[T]]] = table, id:BaseNValue, ownID:BaseNValue = nodeID, node:T):Boolean =
  {
    val prefix = nodeID.longestMatchingPrefix(id)
    val rowSize = table.size
    val offset = diff(ownID, rowSize)
    var row = offset + prefix
    var col:Int = 0
    var inserted:Boolean = true
    
    /* Check to see if there are a differing number of digits. If this is the
     * case, "imagine" leading zeroes and match those digits according to the
     * number of rows available. */
    if(ownID.numOfDigits() < id.numOfDigits())
    {
      row = rowSize - id.numOfDigits()
      /* In this case, node cannot be inserted into the table. Give the last 
       * position of nodeID */
      if(row < 0)
      {
        row = rowSize - 1 
    	col = ownID.nextDigit(n = ownID.numOfDigits()-1)
      }
      /* In this case, */
      else col = id.nextDigit(0)
    }
    else if(ownID.numOfDigits() > id.numOfDigits())
    {
      /* node will not fit into the table. Set it to an existing node */
      if(rowSize < ownID.numOfDigits())
      {
        row = rowSize - 1 
    	col = ownID.nextDigit(n = ownID.numOfDigits()-1)
      }
      /* Node fits, set it level with the number of digits in ownID into col 0 */
      else col = 0
    }
    else col = id.nextDigit(n = prefix)
    if(table(row)(col) == null) table(row)(col) = new Route(id, node) 
    else inserted = false
    return inserted
  }
}