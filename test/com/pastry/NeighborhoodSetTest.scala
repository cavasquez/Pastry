package com.pastry

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import akka.actor.ActorRef
import org.scalatest.junit.AssertionsForJUnit
import java.lang.ArrayIndexOutOfBoundsException
import scala.collection.mutable.ArrayBuffer

/**
 * Test for com.pastry.NeighborhoodSet
 */
class NeighborhoodSetTest 
{
  var test:NeighborhoodSet[String] = null
  var value:BaseNValue = _
  var root:String = _
  var table:ArrayBuffer[Node[String]] = _
  var n:Int = _
  
  @Before
  def initialize() = 
  {
    value = new BaseNValue(100,10)
    root = new String("boss")
    n = 4
    table = new ArrayBuffer[Node[String]]
    test = new NeighborhoodSet[String](parentID = value, b= 1)
  }
  
  @Test
  def insertTest =
  {
    var a = new Node[String](new BaseNValue(1), "a")
    var b = new Node[String](new BaseNValue(2), "b")
    var c = new Node[String](new BaseNValue(3), "c")
    var d = new Node[String](new BaseNValue(4), "d")
    var e = new Node[String](new BaseNValue(5), "e")
    
    assertEquals(true, test.insert(a, table))
    assertEquals(a, table(0))
    
    assertEquals(true, test.insert(b, table))
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    
    assertEquals(true, test.insert(c, table))
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    assertEquals(c, table(2))
    
    assertEquals(true, test.insert(d, table))
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    assertEquals(c, table(2))
    assertEquals(d, table(3))
    
    assertEquals(false, test.insert(e, table))
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    assertEquals(c, table(2))
    assertEquals(d, table(3))
  }
  
  @Test 
  def removeTest =
  {
    var a = new Node[String](new BaseNValue(1), "a")
    var b = new Node[String](new BaseNValue(2), "b")
    var c = new Node[String](new BaseNValue(3), "c")
    var d = new Node[String](new BaseNValue(4), "d")
    var e = new Node[String](new BaseNValue(5), "e")
    table += a += b += c += d
    
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    assertEquals(c, table(2))
    assertEquals(d, table(3))
    
    test.remove(e, table)
    assertEquals(a, table(0))
    assertEquals(b, table(1))
    assertEquals(c, table(2))
    assertEquals(d, table(3))
    
    test.remove(a, table)
    assertEquals(b, table(0))
    assertEquals(c, table(1))
    assertEquals(d, table(2))
    
    test.remove(b.id, table)
    assertEquals(c, table(0))
    assertEquals(d, table(1))
  }
}