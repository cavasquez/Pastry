package com.pastry

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import akka.actor.ActorRef
import org.scalatest.junit.AssertionsForJUnit
import java.lang.ArrayIndexOutOfBoundsException

/**
 * Tests com.bitcoin.ShaHasher
 */
class RoutingTableTest extends AssertionsForJUnit 
{
  var test:RoutingTable[String] = null
  var value:BaseNValue = _
  var root:String = _
  
  @Before
  def initialize() = 
  {
    value = new BaseNValue(19410,4) /* 19410 = 10233102 in base 4 */
    root = new String("boss")
    test = new RoutingTable[String](value, b = 2, owner = root)
  }
  
  @Test
  def makeTableTest =
  {
    var table = test.makeTable(5,10)
    assertEquals(5, table.size)
    assertEquals(10, table(0).size)
    assertEquals(10, table(1).size)
    assertEquals(10, table(2).size)
    assertEquals(10, table(3).size)
    assertEquals(null, table(0)(0))
    
    try 
    { 
      assert(null, table(5))
      fail("Accessing 5 should have thrown an exception")
    }
    catch 
    {
      case e:ArrayIndexOutOfBoundsException =>
      case _:Throwable => fail("This should have thrown an ArrayOutOfBoundsException")
    }
  }
  
  @Test
  def makeRoutingTableTest =
  {
    var temp = new String("not an actorref")
    var table = test.makeRoutingTable(5,10, value, 10, temp)
    assertEquals(null, table)
  }
}