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
      arr = larger
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
    if(!inserted && i < arr.length) 
    {
      inserted = true
      arr(i) = node
    }
    else if(inserted && i < arr.length) arr(i) = temp
    return inserted
  }
  
  /**
   * Attempts to remove node with id from smaller or larger depending on if id 
   * is smaller or larger than parentID (respectively). This method returns 
   * false if node is not removed from larger or smaller.
   * @param node		The id of the node being removed from larger or smaller
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			returns whether or not insertion was successful
   */
  def remove(id:BaseNValue, parentID:BaseNValue = parentID, smaller:Array[Node[T]] = smaller, larger:Array[Node[T]] = larger):Boolean =
  {
    var arr:Array[Node[T]] = null
    var removed:Boolean  = false
    if(parentID - id > 0) arr = smaller else arr = larger
    
    var i = 0
    while(i < arr.length-1 && arr(i) != null)
    {
      if(arr(i).id == id) removed = true
      if(removed) arr(i) = arr(i+1)
      i += 1
    }
    /* Take care of case where i == arr.length - 1 */
    if(arr(i) != null && arr(i).id == id) removed = true
    if(removed) arr(i) = null
    return removed
  }
  
  /**
   * Attempts to remove node from smaller or larger depending on if node is 
   * smaller or larger than parentID (respectively). This method returns false
   * if node is not removed from larger or smaller.
   * @param node		The node being removed from larger or smaller
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			returns whether or not insertion was successful
   */
  def remove(node:Node[T]):Boolean = remove(id = node.id)
  
  /**
   * Looks for node in smaller or larger.
   * @param id			The node ID being searched for
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			The node with the given id or null if no node was found
   */
  def get(id:BaseNValue, parentID:BaseNValue = parentID, smaller:Array[Node[T]] = smaller, larger:Array[Node[T]] = larger):Node[T] = 
  {
    var arr:Array[Node[T]] = null
    var i = 0
    var node:Node[T] = null
    
    if(parentID - id > 0) arr = smaller else arr = smaller
    while(i < arr.length && arr(i) != null)
    {
      if(arr(i).id == id) node = arr(i)
      i += 1
    }
    return node
  }
  
  /**
   * Looks for node in smaller or larger that is closest to id. This method 
   * returns null if id is not within the range min(smaller) and max(larger.)
   * @param id			The node ID being searched for
   * @param parentID	The parent of larger and smaller
   * @param smaller		The smaller array
   * @param larger		The larger array
   * @return			The node with the given id or null if no node was found
   */
  def findCosest(id:BaseNValue, parentID:BaseNValue = parentID, smaller:Array[Node[T]] = smaller, larger:Array[Node[T]] = larger):Node[T] = 
  {
    var arr:Array[Node[T]] = null
    var comp: (BaseNValue, BaseNValue) => Boolean = null
    
    if(parentID - id > 0)
    {
      arr = smaller
      comp = (x:BaseNValue, y:BaseNValue) => x <= y
    }
    else
    {
      arr = larger
      comp = (x:BaseNValue, y:BaseNValue) => x >= y
    }
    
    var i = 0
    var diff:BigInt = if(arr(0) != null) (arr(0).id - id).abs else null
    var node:Node[T] = null
    var inRange = false
    while(i < arr.length  && arr(i) != null)
    {
      /* Check if id is in range */
      if(comp(id, arr(i).id)) inRange = true
      
      /* Find closest node */
      if((arr(i).id - id).abs <= diff)
      {
        node = arr(i)
        diff = (arr(i).id - id).abs 
      }
      i += 1
    }
    if(!inRange) node = null
    return node
  }
  
  def +=(that:Node[T]):Boolean = insert(node = that)
  
  def -=(that:Node[T]):Boolean = remove(node = that)
  
  def -=(that:BaseNValue):Boolean = remove(id = that)
}