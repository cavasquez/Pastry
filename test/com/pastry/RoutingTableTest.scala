package com.pastry

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import akka.actor.ActorRef
import org.scalatest.junit.AssertionsForJUnit
import java.lang.ArrayIndexOutOfBoundsException

/**
 * Tests com.pastry.RoutingTable
 */
class RoutingTableTest extends AssertionsForJUnit 
{
  var test:RoutingTable[String] = null
  var value:BaseNValue = _
  var root:String = _
  var n:Int = _
  
  @Before
  def initialize() = 
  {
    value = new BaseNValue(19410,4) /* 19410 = 10233102 in base 4 */
    root = new String("boss")
    n = Math.pow(Math.pow(2, 4), 8).toInt
    test = new RoutingTable[String](value, n = n, b = 2, owner = root)
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
    value = new BaseNValue(19410,4) /* 19410 = 10233102 in base 4 (8 digits) */
    var table = test.makeRoutingTable(8,4, value, temp)
    assertEquals(temp, table(0)(1).node)
    assertEquals(temp, table(1)(0).node)
    assertEquals(temp, table(2)(2).node)
    assertEquals(temp, table(3)(3).node)
    assertEquals(temp, table(4)(3).node)
    assertEquals(temp, table(5)(1).node)
    assertEquals(temp, table(6)(0).node)
    assertEquals(temp, table(7)(2).node)
    
    value = new BaseNValue(7122,4) /* 19410 = 1233102 in base 4 (7 digits) */
    table = test.makeRoutingTable(8,4, value, temp)
    assertEquals(temp, table(0)(0).node)
    assertEquals(temp, table(1)(1).node)
    assertEquals(temp, table(2)(2).node)
    assertEquals(temp, table(3)(3).node)
    assertEquals(temp, table(4)(3).node)
    assertEquals(temp, table(5)(1).node)
    assertEquals(temp, table(6)(0).node)
    assertEquals(temp, table(7)(2).node)
    
    value = new BaseNValue(65538,4) /* 65538 = 100000002 in base 4 (9 digits) */
    table = test.makeRoutingTable(8,4, value, temp)    
    assertEquals(temp, table(0)(0).node)
    assertEquals(temp, table(1)(0).node)
    assertEquals(temp, table(2)(0).node)
    assertEquals(temp, table(3)(0).node)
    assertEquals(temp, table(4)(0).node)
    assertEquals(temp, table(5)(0).node)
    assertEquals(temp, table(6)(0).node)
    assertEquals(temp, table(7)(2).node)
  }
  
  @Test
  def insertTest =
  {
    var temp = new String("not an actorref")
    value = new BaseNValue(19410,4) /* 19410 = 10233102 in base 4 (8 digits) */
    var table = test.makeRoutingTable(8,4, value, temp) 
    var id = new BaseNValue(48086,4) /* 48086 = 23233112 in base 4(8 digits) */
    var nodeA = new String("A")
    assertEquals(true,test.insert(table, id, value, nodeA)) 
    assertEquals(nodeA, table(0)(2).node) /* Test insertion into non-null */
    var nodeB = new String("B")
    id = new BaseNValue(35602,4) /* 35602 = 20230102 in base 4(8 digits) */
    assertEquals(false,test.insert(table, id, value, nodeB)) /* Test insertion into existing */
    assertEquals(nodeA, table(0)(2).node) /* Test insertion into non-null */
    id = new BaseNValue(19354,4) /* 19354 = 10232122 in base 4 (8 digits) */
    var nodeC = new String("C")
    assertEquals(true,test.insert(table, id, value, nodeC)) /* Test insertion */
    assertEquals(nodeC, table(4)(2).node) /* Test insertion into non-null */
    id = new BaseNValue(154,4) /* 154 = 2122 in base 4 (4 digits) */
    var nodeD = new String("D")
    assertEquals(true,test.insert(table, id, value, nodeD)) /* Test insertion */
    assertEquals(nodeD, table(0)(0).node) /* insert smaller digit */
    id = new BaseNValue(3026,4) /* 3026 = 233102 in base 4 (6 digits) */
    var nodeE = new String("E")
    assertEquals(false,test.insert(table, id, value, nodeE)) /* Test insertion of subset of original */
    id = new BaseNValue(108994,4) /* 108994 = 122213002 in base 4 (9 digits) */
    var nodeF = new String("E")
    assertEquals(false,test.insert(table, id, value, nodeF)) /* Test insertion of larger digits */
    
    
    value = new BaseNValue(108,4) /* 108 = 1230 in base 4 (4 digits) */
    table = test.makeRoutingTable(8,4, value, temp)
    id = new BaseNValue(1365,4) /* 1365 = 111111 in base 4 (6 digits) */
    assertEquals(true,test.insert(table, id, value, nodeA)) /* Test insertion */
    assertEquals(nodeA, table(2)(1).node) /* Test higher digit */
    id = new BaseNValue(5,4) /* 5 = 11 in base 4 (2 digits) */
    assertEquals(true,test.insert(table, id, value, nodeB)) /* Test insertion */
    assertEquals(nodeB, table(4)(0).node) /* Test lower digit */
    id = new BaseNValue(170,4) /* 170 = 2222 in base 4 (4 digits) */
    assertEquals(true,test.insert(table, id, value, nodeC)) /* Test insertion */
    assertEquals(nodeC, table(4)(2).node) /* Test lower digit */
    id = new BaseNValue(108994,4) /* 108994 = 122213002 in base 4 (9 digits) */
    assertEquals(false,test.insert(table, id, value, nodeD)) /* Test insertion of larger digits */
    
    value = new BaseNValue(449383,4) /* 449383 = 1231231213 in base 4 (10 digits) */
    table = test.makeRoutingTable(8,4, value, temp)    
    id = new BaseNValue(3495255,4) /* 349525 = 1111111111 in base 4 (10 digits) */
    assertEquals(false,test.insert(table, id, value, nodeA)) /* Should not fit */
    id = new BaseNValue(415061,4) /* 415061 = 121111111 in base 4 (10 digits) */
    assertEquals(true,test.insert(table, id, value, nodeB)) /* Should fit with same digit count */
    assertEquals(nodeB, table(0)(1).node) /* Test lower digit */
    id = new BaseNValue(6640981,4) /* 6640981 = 121111111111 in base 4 (12 digits) */
    assertEquals(false,test.insert(table, id, value, nodeC)) /* too big, should not fit */
    id = new BaseNValue(5,4) /* 5 = 11 in base 4 (2 digits) */
    assertEquals(false,test.insert(table, id, value, nodeD)) /* too small, should not fit */
  }
  
  @Test
  def removeTest =
  {
    value = new BaseNValue(91,4) /* 91 = 1123 in base 4 */
    root = new String("boss")
    n = Math.pow(Math.pow(2, 4), 4).toInt
    test = new RoutingTable[String](value, n = n, b = 2, owner = root)
    var parent = new Node[String](value, "parent")
    
    var A = new Node[String](new BaseNValue(1, 4), "A") /* 1 */
    var B = new Node[String](new BaseNValue(172, 4), "B") /* 2230 */
    var C = new Node[String](new BaseNValue(192, 4), "C") /* 3000 */
    var D = null
    var E = null
    var F = null
    var G = new Node[String](new BaseNValue(81, 4), "G") /* 1101 */
    var H = null
    var I = null
    var J = null
    var K = new Node[String](new BaseNValue(89, 4), "K") /* 1121 */
    var L = new Node[String](new BaseNValue(90, 4), "L") /* 1122 */
    
    var arr:Array[Array[Node[String]]] = new Array[Array[Node[String]]](4)
    arr(0) = Array[Node[String]](A, parent, B, C)
    arr(1) = Array[Node[String]](D, parent, E, F)
    arr(2) = Array[Node[String]](G, H, parent, I)
    arr(3) = Array[Node[String]](J, K, L, parent)
    
    var removeMe = new BaseNValue(75) /* 1023 */
    assertEquals(false, test.remove(arr, removeMe, value))
    assertEquals(A, arr(0)(0))
    assertEquals(parent, arr(0)(1))
    assertEquals(B, arr(0)(2))
    assertEquals(C, arr(0)(3))
    assertEquals(D, arr(1)(0))
    assertEquals(parent, arr(1)(1))
    assertEquals(E, arr(1)(2))
    assertEquals(F, arr(1)(3))
    assertEquals(G, arr(2)(0))
    assertEquals(H, arr(2)(1))
    assertEquals(parent, arr(2)(2))
    assertEquals(I, arr(2)(3))
    assertEquals(J, arr(3)(0))
    assertEquals(K, arr(3)(1))
    assertEquals(L, arr(3)(2))
    assertEquals(parent, arr(3)(3))
    
    assertEquals(true, test.remove(arr, A.id, value))
    assertEquals(null, arr(0)(0))
    assertEquals(parent, arr(0)(1))
    assertEquals(B, arr(0)(2))
    assertEquals(C, arr(0)(3))
    assertEquals(D, arr(1)(0))
    assertEquals(parent, arr(1)(1))
    assertEquals(E, arr(1)(2))
    assertEquals(F, arr(1)(3))
    assertEquals(G, arr(2)(0))
    assertEquals(H, arr(2)(1))
    assertEquals(parent, arr(2)(2))
    assertEquals(I, arr(2)(3))
    assertEquals(J, arr(3)(0))
    assertEquals(K, arr(3)(1))
    assertEquals(L, arr(3)(2))
    assertEquals(parent, arr(3)(3))
    
    assertEquals(true, test.remove(arr, L.id, value))
    assertEquals(null, arr(0)(0))
    assertEquals(parent, arr(0)(1))
    assertEquals(B, arr(0)(2))
    assertEquals(C, arr(0)(3))
    assertEquals(D, arr(1)(0))
    assertEquals(parent, arr(1)(1))
    assertEquals(E, arr(1)(2))
    assertEquals(F, arr(1)(3))
    assertEquals(G, arr(2)(0))
    assertEquals(H, arr(2)(1))
    assertEquals(parent, arr(2)(2))
    assertEquals(I, arr(2)(3))
    assertEquals(J, arr(3)(0))
    assertEquals(K, arr(3)(1))
    assertEquals(null, arr(3)(2))
    assertEquals(parent, arr(3)(3))
    
    removeMe = new BaseNValue(1210, 4) /* 102322 */
    assertEquals(false, test.remove(arr, removeMe, value))
    assertEquals(null, arr(0)(0))
    assertEquals(parent, arr(0)(1))
    assertEquals(B, arr(0)(2))
    assertEquals(C, arr(0)(3))
    assertEquals(D, arr(1)(0))
    assertEquals(parent, arr(1)(1))
    assertEquals(E, arr(1)(2))
    assertEquals(F, arr(1)(3))
    assertEquals(G, arr(2)(0))
    assertEquals(H, arr(2)(1))
    assertEquals(parent, arr(2)(2))
    assertEquals(I, arr(2)(3))
    assertEquals(J, arr(3)(0))
    assertEquals(K, arr(3)(1))
    assertEquals(null, arr(3)(2))
    assertEquals(parent, arr(3)(3))
  }
}