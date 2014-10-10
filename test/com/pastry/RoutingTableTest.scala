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
  @Before
  def initialize() = 
  {
    test = new RoutingTable[String]()
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
  def makeRoutingTable =
  {
    var temp = new String("not an actorref")
    var table = test.makeRoutingTable(5,10, 0, temp)
    assertEquals(null, table)
  }
}