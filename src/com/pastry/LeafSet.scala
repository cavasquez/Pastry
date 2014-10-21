package com.pastry

import scala.reflect.ClassTag

/**
 * The leaf set is the set of nodes with the n/2 numerically closest larger 
 * node ID's, and the n/2 nodes with numerically closest smaller node IDs, 
 * relative to the parent node's ID where n is the size of leaf set.
 * 
 * LeafSet will have 2 x 2^b nodes
 */
class LeafSet[T:ClassTag](parentID:BaseNValue, b:Int = 4)
{
  private lazy val size:Int = Math.pow(2, b).toInt
  private val smaller = new Array[Node[T]](size)
  private val larger = new Array[Node[T]](size)
  
  /**
   * Attempts to insert node into smaller or larger depending on if node is 
   * smaller or larger than parentID (respectively). This method returns false
   * if node is not inserted into larger or smaller. A failure to insert node
   * into larger or smaller can be caused by node have larger difference with
   * parentID than any of the other elements in smaller or larger.
   * @param node		The node being inserted into larger or smaller
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			returns whether or not insertion was successful
   */
  def insert(node:Node[T], parentID:BaseNValue = parentID, smaller:Array[Node[T]] = smaller, larger:Array[Node[T]] = larger):Boolean =
  {
    var arr:Array[Node[T]] = null
    var inserted:Boolean  = false
    var comp: (BaseNValue, BaseNValue) => Boolean = null
    
    if(parentID - node.id > 0)
    {
      arr = smaller
      comp = (x:BaseNValue, y:BaseNValue) => x > y
    }
    else
    {
      arr = smaller
      comp = (x:BaseNValue, y:BaseNValue) => x < y
    }
    
    var i = 0
    var temp = node
    var swapper = node
    while(i < arr.length  && arr(i) != null)
    {
      if(comp(temp.id, arr(i).id))
      {
        inserted = true
        swapper = arr(i)
        arr(i) = temp
        temp = swapper
      }
      i += 1
    }
    
    return inserted
  }
  
  /**
   * Attempts to remove node from smaller or larger depending on if node is 
   * smaller or larger than parentID (respectively). This method returns false
   * if node is not removed from larger or smaller.
   * @param node		The node being inserted into larger or smaller
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			returns whether or not insertion was successful
   */
  def remove(node:Node[T], parentID:BaseNValue = parentID, smaller:Array[Node[T]] = smaller, larger:Array[Node[T]] = larger):Boolean =
  {
    var arr:Array[Node[T]] = null
    var removed:Boolean  = false
    if(parentID - node.id > 0) arr = smaller else arr = smaller
    
    var i = 0
    while(i < arr.length-1 && arr(i) != null)
    {
      if(arr(i).id == node.id) removed = true
      if(removed) arr(i) = arr(i+1)
      i += 1
    }
    if(removed) arr(i) = null
    return removed
  }
  
  def +=(that:Node[T]):Unit = insert(node = that)
  
  def -=(that:Node[T]):Unit = remove(node = that)
}