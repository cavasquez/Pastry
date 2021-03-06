package com.pastry

import akka.actor.ActorRef
import scala.reflect.ClassTag

/**
 * RoutingTable represents the Routing table used by pastry to route to 
 * neighboring nodes. 
 * @param parentID	The parentID of the owning Pastry node
 * @param b			The b value that will be used by this Pastry network
 * @param n			The maximum number of nodes in this Pastry network
 * @param owner		The ActorRef of the owning node
 */
class RoutingTable[T:ClassTag](val parentID:BaseNValue, b:Int = 4, n:Int = 10, owner:T = null)
{
  if(b < 1) throw new IllegalArgumentException("b must be greater than 0")
  private val rowSize = Math.ceil(Math.log(n)/Math.log(Math.pow(2, b))).toInt
  if(rowSize < parentID.numOfDigits()) throw new IllegalArgumentException("n is not big enough to contain the number of digits in parentID")
  private val colSize = Math.pow(2, b).toInt
  if(colSize > parentID.base) throw new IllegalArgumentException("cannot have more columns than the size of the base")
  private[pastry] val table = makeRoutingTable(rowSize, colSize, parentID, owner)
    
  /**
   * Creates a routing table of the provided row and column size that contains
   * the owner.
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @param parentID	The node ID of the owner
   * @param	owner	The owning node of this table
   * @param diff	The difference between row size and the number of digits in 
   * 				parentID.
   * @return		Returns a 2D array that represents an initial Routing Table
   * 				for the Pastry protocol
   */
  private[pastry] def makeRoutingTable(rowSize:Int, colSize:Int, parentID:BaseNValue, owner:T):Array[Array[Node[T]]] =
  {
    var table = makeTable(rowSize, colSize)
    var offset = diff(parentID, rowSize) /* the offset cannot be less than 0 */
    var difference = rowSize - parentID.numOfDigits()
    for(i:Int <- rowSize-1 to offset by -1) { table(i)(parentID.nextDigit(n = i - difference)) = new Node(parentID, owner) }
    for(i:Int <- offset - 1 to 0 by -1) { table(i)(0) = new Node(parentID, owner) }
    return table
  }
  
  /**
   * Returns a 2D array of type T
   * @param rowSize	The number of rows in the 2D array
   * @param colSize	The number of columns in the 2D array 
   * @return 		Returns a 2D array of type T
   */
  private[pastry] def makeTable(rowSize:Int, colSize:Int):Array[Array[Node[T]]] =
  {
    var rTable = new Array[Array[Node[T]]](rowSize)
    for(i:Int <- 0 until rowSize) { rTable(i) = new Array[Node[T]](colSize) }
    return rTable
  }
  
  /**
   * Returns the difference between the number of digits in value and n. This 
   * method will never return less than 0
   * @param value	a BaseNValue
   * @param n		the number whose value is being subtracted from
   * @return 		the difference between the number of digits in value and n
   */
  private[pastry] def diff(value:BaseNValue, n:Int):Int = if(n >= value.numOfDigits()) n - value.numOfDigits() else 0
  
  /**
   * Inserts the node with the given id into the Routing Table. The insert will
   * attempt to insert node into the position that matches id with the parent.
   * It will not attempt to insert node into a "lower" matching spot. It will
   * only be inserted into the position who matches the prefix id the closest
   * to parentID.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param id		The ID of the inserted node.
   * @param ownID	The ID of the node that "owns" this table
   * @param node	The node being inserted
   * @returner		Returns whether or not the insert was successful
   */
  def insert(table:Array[Array[Node[T]]] = table, id:BaseNValue, ownID:BaseNValue = parentID, node:T):Boolean =
  {
    val prefix = parentID.longestMatchingPrefix(id)
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
       * position of parentID */
      if(row < 0)
      {
        row = rowSize - 1 
    	col = ownID.nextDigit(n = ownID.numOfDigits()-1)
      }
      /* In this case, col can come from the 0th digit of id*/
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
    /* We can't add ourselves to the table */
    else if(id != parentID) col = id.nextDigit(n = prefix)
    else
    {
      /* Attempting to insert the same id. Set row and col to an existing node */
      row = rowSize - 1 
      col = ownID.nextDigit(n = ownID.numOfDigits()-1)
    }
    if(table(row)(col) == null) table(row)(col) = new Node(id, node) 
    else inserted = false
    return inserted
  }
  
  /**
   * Inserts the node into the Routing Table. The insert will attempt to insert 
   * node into the position that matches its id with the parent. It will not 
   * attempt to insert node into a "lower" matching spot. It will only be 
   * inserted into the position which matches the prefix id the closest
   * to parentID.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param node	The node being inserted.
   * @returner		Returns whether or not the insert was successful
   */
  def insert(node:Node[T]):Boolean = insert(id = node.id, node = node.node)
  
  /**
   * Removes the node with the given id from the Routing Table. The remove will
   * attempt to remove  the node from the position that matches id with the 
   * parent. It will not attempt to remove the node from a "lower" matching 
   * spot.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param id		The ID of the node being removed.
   * @param ownID	The ID of the node that "owns" this table
   * @returner		Returns whether or not the remove was successful
   */
  def remove(table:Array[Array[Node[T]]] = table, id:BaseNValue, ownID:BaseNValue = parentID):Boolean =
  {
    val prefix = parentID.longestMatchingPrefix(id)
    val rowSize = table.size
    val offset = diff(ownID, rowSize)
    var row = offset + prefix
    var col:Int = 0
    var removed:Boolean = true
    
    /* Check to see if there are a differing number of digits. If this is the
     * case, "imagine" leading zeroes and match those digits according to the
     * number of rows available. */
    if(ownID.numOfDigits() < id.numOfDigits())
    {
      row = rowSize - id.numOfDigits()
      /* In this case, a node with id cannot exist */
      if(row < 0) removed = false
      /* In this case, col can come from the 0th digit of id */
      else col = id.nextDigit(0)
    }
    else if(ownID.numOfDigits() > id.numOfDigits())
    {
      /* A node with id will not fit into the table. Set it to an existing node */
      if(rowSize < ownID.numOfDigits()) removed = false
      /* A node with id fits and thus may exist. Set it level with the number 
       * of digits in ownID into col 0 */
      else col = 0
    }
    else col = id.nextDigit(n = prefix)
    
    /* Check if it was possible to remove a node with id (ie, it could exist
     * in this table) */
    if(removed)
    {
      if(table(row)(col) != null) table(row)(col) = null
      else removed = false
    }    
    return removed
  }
  
  /**
   * Removes the node with the given id from the Routing Table. The remove will
   * attempt to remove  the node from the position that matches id with the 
   * parent. It will not attempt to remove the node from a "lower" matching 
   * spot.
   * 
   * Note: Insert attempts to match the prefix base b, not base 10
   * @param node	The ID of the node being removed.
   * @returner		Returns whether or not the remove was successful
   */
  def remove(node:Node[T]):Boolean = remove(id = node.id)
  
  /**
   * Searches for a node in the table with an id that is prefixed by the 
   * longest common prefix between id and ownID plus the following digit from
   * id
   * @param id		The ID of a node.
   * @param ownID	The ID of the node that "owns" this table
   * @returner		Returns a node that is close to id
   */
  def findRouteableNode(id:BaseNValue, table:Array[Array[Node[T]]] = table, ownID:BaseNValue = parentID):Node[T] =
  {
    val prefix = parentID.longestMatchingPrefix(id)
    val rowSize = table.size
    val offset = diff(ownID, rowSize)
    var row = offset + prefix
    var col:Int = 0
    var node:Node[T] = null
    var possible:Boolean = true
    
    /* Check to see if there are a differing number of digits. If this is the
     * case, "imagine" leading zeroes and match those digits according to the
     * number of rows available. */
    if(ownID.numOfDigits() < id.numOfDigits())
    {
      row = rowSize - id.numOfDigits()
      /* In this case, a node with id cannot exist */
      if(row < 0) possible = false
      /* In this case, col can come from the 0th digit of id */
      else col = id.nextDigit(0)
    }
    else if(ownID.numOfDigits() > id.numOfDigits())
    {
      /* A node with id will not fit into the table. Set it to an existing node */
      if(rowSize < ownID.numOfDigits()) possible = false
      /* A node with id fits and thus may exist. Set it level with the number 
       * of digits in ownID into col 0 */
      else col = 0
    }
    else col = id.nextDigit(n = prefix)
    
    /* If this id had a chance to be in the table, then it could have a 
     * matching node */
    if(possible) node = table(row)(col)    
    return node
  }
  
  /**
   * Searches table for the node that has the longest common prefix with id
   * @param id			The id being looked for
   * @param parentID	The parent ID
   * @param table		The table being searched
   */
  def findLongestMatchingPrefix(id:BaseNValue, parentID:BaseNValue = parentID, table:Array[Array[Node[T]]] = table):Node[T] =
  {
    var largestDigit = Int.MinValue 
    for(i <- 0 until table.size)
    {
      for(j <- 0 until table(i).size)
      {
        if(table(i) != null && table(i)(j) != null && table(i)(j).id != parentID && table(i)(j).id.numOfDigits(base = id.base) > largestDigit) largestDigit = table(i)(j).id.numOfDigits(base = id.base)
      }
    }
    
    var longestMatchingPrefix = Int.MinValue 
    var curMatchingPrefix = Int.MinValue
    var offset = 0
    var node:Node[T] = null
    for(i <- 0 until table.size)
    {
      for(j <- 0 until table(i).size)
      {
        /* Compensate for varying digit sizes */
        if(table(i) != null && table(i)(j) != null)
        {
          if(table(i)(j).id.numOfDigits(base = id.base) <= id.numOfDigits(base = id.base)) offset = largestDigit - id.numOfDigits(base = id.base)
          else offset = largestDigit - table(i)(j).id.numOfDigits(base = id.base)
          
          curMatchingPrefix = id.longestMatchingPrefix(table(i)(j).id) + offset
          if(curMatchingPrefix > longestMatchingPrefix)
          {
            longestMatchingPrefix = curMatchingPrefix
            node = table(i)(j)
          }
        }
      }
    }
    return node
  }
  
  def +=(that:Node[T]):RoutingTable.this.type = { insert(that); return this }
  
  def -=(that:Node[T]):RoutingTable.this.type = { remove(that); return this }
  
  def -=(that:BaseNValue):RoutingTable.this.type = { remove(id = that); return this }
}