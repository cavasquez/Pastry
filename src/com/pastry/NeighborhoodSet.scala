package com.pastry

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

/**
 * Neighborhood set contains the node IDs and ip addresses of the M nodes that 
 * are closest (according to the proximity metric) to the local node. The
 * neighborhood set is not normally used in routing messages; it is useful in 
 * maintaining locality properties. This will largely be maintained by the
 * user.
 * 
 * NeighborhoodSet will have 2 x 2^b nodes
 */
class NeighborhoodSet[T:ClassTag](parentID:BaseNValue, b:Int = 4)
{
  private lazy val size = 2 *Math.pow(2, b).toInt 
  private lazy val table = new ArrayBuffer[Node[T]](size)
  
  /**
   * Adds node to table if table is not full. Returns whether or not the insert
   * failed
   * @param node	The node to be added to table
   * @param table	The table holding near-by nodes
   * @return		Returns the status of insertion
   */
  def insert(node:Node[T], table:ArrayBuffer[Node[T]]= table):Boolean =
  {
    var inserted = true
    if(table.size < size) table += node
    else inserted = false
    return inserted
  }
  
  /**
   * Removes node from table. Returns whether or not node was removed
   * @param node	The node to be removed from table
   * @param table	The table holding near-by nodes
   * @return		Returns the status of removal
   */
  def remove(node:Node[T], table:ArrayBuffer[Node[T]] = table):Unit = table -= node
  
  /**
   * Removes the node with the given id from table.
   * @param id		The id of the node being removed
   * @param table	The table holding near-by nodes
   * @return		Returns the status of removal
   */
  def remove(id:BaseNValue, table:ArrayBuffer[Node[T]]):Unit =
  {
    var i:Int = 0
    var found = false
    while(i < table.size && !found)
    {
      if(table(i).id == id)
      {
        table -= table(i)
        found = true
      }
      i += 1
    }
  }
  
  /**
   * Returns the node located at the corresponding index i in table
   * @param i		The index
   * @param table	The table holding the near-by nodes
   * @return 		The node located at index i in table
   */
  def get(i:Int, table:ArrayBuffer[Node[T]]):Node[T] = table(i)
  
  /**
   * Returns the length of the table
   */
  def length():Int = size
  
  def +=(that:Node[T]):NeighborhoodSet.this.type = { insert(that); return this }
  
  def -=(that:Node[T]):NeighborhoodSet.this.type = { remove(that); return this } 
  
  def -=(that:BaseNValue):NeighborhoodSet.this.type = { remove(that, this.table); return this }
}