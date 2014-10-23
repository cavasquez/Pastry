package com.pastry

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import akka.actor.ActorRef
import org.scalatest.junit.AssertionsForJUnit
import java.lang.ArrayIndexOutOfBoundsException

/**
 * Tests com.pastry.LeafSet
 */
class LeafSetTest extends AssertionsForJUnit 
{
  var test:LeafSet[String] = null
  var value:BaseNValue = _
  var root:String = _
  var smaller:Array[Node[String]] = _
  var larger:Array[Node[String]] = _
  var n:Int = _
  
  @Before
  def initialize() = 
  {
    value = new BaseNValue(100,10)
    root = new String("boss")
    n = 10
    smaller = new Array[Node[String]](5)
    larger = new Array[Node[String]](5)
    test = new LeafSet[String](parentID = value)
  }
  
  @Test
  def insertTest =
  {
    var a = new Node[String](new BaseNValue(9,10), "A")
    assertEquals(true, test.insert(a, value, smaller, larger))
    assertEquals(a, smaller(0))
    assertEquals(null, smaller(1))
    assertEquals(null, larger(0))
    
    var b = new Node[String](new BaseNValue(15,10), "B")
    assertEquals(true, test.insert(b, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(a, smaller(1))
    assertEquals(null, smaller(2))
    assertEquals(null, larger(0))
    
    var c = new Node[String](new BaseNValue(5,10), "C")
    assertEquals(true, test.insert(c, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(a, smaller(1))
    assertEquals(c, smaller(2))
    assertEquals(null, smaller(3))
    assertEquals(null, larger(0))
    
    var d = new Node[String](new BaseNValue(11,10), "D")
    assertEquals(true, test.insert(d, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(d, smaller(1))
    assertEquals(a, smaller(2))
    assertEquals(c, smaller(3))
    assertEquals(null, smaller(4))
    assertEquals(null, larger(0))
    
    var e = new Node[String](new BaseNValue(-15,10), "E")
    assertEquals(true, test.insert(e, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(d, smaller(1))
    assertEquals(a, smaller(2))
    assertEquals(c, smaller(3))
    assertEquals(e, smaller(4))
    assertEquals(null, larger(0))
    
    var f = new Node[String](new BaseNValue(0,10), "F")
    assertEquals(true, test.insert(f, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(d, smaller(1))
    assertEquals(a, smaller(2))
    assertEquals(c, smaller(3))
    assertEquals(f, smaller(4))
    assertEquals(null, larger(0)) 
    
    var g = new Node[String](new BaseNValue(50,10), "G")
    assertEquals(true, test.insert(g, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(c, smaller(4))
    assertEquals(null, larger(0)) 
    
    var h = new Node[String](new BaseNValue(-50,10), "H")
    assertEquals(false, test.insert(h, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(c, smaller(4))
    assertEquals(null, larger(0)) 
    
    var j = new Node[String](new BaseNValue(6,10), "J")
    assertEquals(true, test.insert(j, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(j, smaller(4))
    assertEquals(null, larger(0)) 
    
    var A = new Node[String](new BaseNValue(110,10), "A")
    assertEquals(true, test.insert(A, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(j, smaller(4))
    assertEquals(A, larger(0))
    assertEquals(null, larger(1)) 
    
    var B = new Node[String](new BaseNValue(115,10), "B")
    assertEquals(true, test.insert(B, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(j, smaller(4))
    assertEquals(A, larger(0))
    assertEquals(B, larger(1))
    assertEquals(null, larger(2)) 
    
    var C = new Node[String](new BaseNValue(101,10), "C")
    assertEquals(true, test.insert(C, value, smaller, larger))
    assertEquals(g, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(a, smaller(3))
    assertEquals(j, smaller(4))
    assertEquals(C, larger(0))
    assertEquals(A, larger(1))
    assertEquals(B, larger(2)) 
    assertEquals(null, larger(3)) 
  }
  
  @Test
  def removeTest()
  {
    var a = new Node[String](new BaseNValue(50,10), "a")
    var b = new Node[String](new BaseNValue(45,10), "b")
    var c = new Node[String](new BaseNValue(40,10), "c")
    var d = new Node[String](new BaseNValue(30,10), "d")
    var e = new Node[String](new BaseNValue(10,10), "e")
    
    var A = new Node[String](new BaseNValue(105,10), "A")
    var B = new Node[String](new BaseNValue(115,10), "B")
    var C = new Node[String](new BaseNValue(130,10), "C")
    var D = new Node[String](new BaseNValue(140,10), "D")
    var E = new Node[String](new BaseNValue(160,10), "E")
    
    assertEquals(false, test.remove(a.id, value, smaller, larger))
    assertEquals(false, test.remove(A.id, value, smaller, larger))
    
    var node = new BaseNValue(0, 10)
    smaller = Array[Node[String]](a, b, c , d, e)
    
    assertEquals(false, test.remove(node, value, smaller, larger))
    assertEquals(a, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(c, smaller(2))
    assertEquals(d, smaller(3))
    assertEquals(e, smaller(4))
    assertEquals(null, larger(0))
    
    assertEquals(true, test.remove(a.id, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(c, smaller(1))
    assertEquals(d, smaller(2))
    assertEquals(e, smaller(3))
    assertEquals(null, smaller(4))
    assertEquals(null, larger(0))
    
    assertEquals(true, test.remove(c.id, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(d, smaller(1))
    assertEquals(e, smaller(2))
    assertEquals(null, smaller(3))
    assertEquals(null, smaller(4))
    assertEquals(null, larger(0))
    
    assertEquals(true, test.remove(e.id, value, smaller, larger))
    assertEquals(b, smaller(0))
    assertEquals(d, smaller(1))
    assertEquals(null, smaller(2))
    assertEquals(null, smaller(3))
    assertEquals(null, smaller(4))
    assertEquals(null, larger(0))
    
    smaller = Array[Node[String]](a, b, c , d, e)
    assertEquals(true, test.remove(e.id, value, smaller, larger))
    assertEquals(a, smaller(0))
    assertEquals(b, smaller(1))
    assertEquals(c, smaller(2))
    assertEquals(d, smaller(3))
    assertEquals(null, smaller(4))
    assertEquals(null, larger(0))
    
    smaller = Array[Node[String]](null, null, null, null, null)
    larger = Array[Node[String]](A, B, C, D, E)
    assertEquals(false, test.remove(node, value, smaller, larger))
    assertEquals(A, larger(0))
    assertEquals(B, larger(1))
    assertEquals(C, larger(2))
    assertEquals(D, larger(3))
    assertEquals(E, larger(4))
    assertEquals(null, smaller(0))
    
    assertEquals(true, test.remove(A.id, value, smaller, larger))
    assertEquals(B, larger(0))
    assertEquals(C, larger(1))
    assertEquals(D, larger(2))
    assertEquals(E, larger(3))
    assertEquals(null, larger(4))
    assertEquals(null, smaller(0))
    
    assertEquals(true, test.remove(C.id, value, smaller, larger))
    assertEquals(B, larger(0))
    assertEquals(D, larger(1))
    assertEquals(E, larger(2))
    assertEquals(null, larger(3))
    assertEquals(null, larger(4))
    assertEquals(null, smaller(0))
    
    assertEquals(true, test.remove(E.id, value, smaller, larger))
    assertEquals(B, larger(0))
    assertEquals(D, larger(1))
    assertEquals(null, larger(2))
    assertEquals(null, larger(3))
    assertEquals(null, larger(4))
    assertEquals(null, smaller(0))
    
    larger = Array[Node[String]](A, B, C, D, E)
    assertEquals(true, test.remove(E.id, value, smaller, larger))
    assertEquals(A, larger(0))
    assertEquals(B, larger(1))
    assertEquals(C, larger(2))
    assertEquals(D, larger(3))
    assertEquals(null, larger(4))
    assertEquals(null, smaller(0))
  }
  
  @Test
  def getTest =
  {
    var a = new Node[String](new BaseNValue(50,10), "a")
    var b = new Node[String](new BaseNValue(45,10), "b")
    var c = new Node[String](new BaseNValue(40,10), "c")
    var d = new Node[String](new BaseNValue(30,10), "d")
    var e = new Node[String](new BaseNValue(10,10), "e")
    
    var A = new Node[String](new BaseNValue(105,10), "A")
    var B = new Node[String](new BaseNValue(115,10), "B")
    var C = new Node[String](new BaseNValue(130,10), "C")
    var D = new Node[String](new BaseNValue(140,10), "D")
    var E = new Node[String](new BaseNValue(160,10), "E")
    
    var node = new BaseNValue(0, 10)
    smaller = Array[Node[String]](a, b, c , d, e)
    larger = Array[Node[String]](A, B, C, D, E)
    
    assertEquals(null, test.get(node, value, smaller, larger))
    node = new BaseNValue(103, 10)
    assertEquals(null, test.get(node, value, smaller, larger))
    
    assertEquals(A, test.get(A.id , value, smaller, larger))
    assertEquals(B, test.get(B.id , value, smaller, larger))
    assertEquals(C, test.get(C.id , value, smaller, larger))
    assertEquals(D, test.get(D.id , value, smaller, larger))
    assertEquals(E, test.get(E.id , value, smaller, larger))
    
    assertEquals(a, test.get(a.id , value, smaller, larger))
    assertEquals(b, test.get(b.id , value, smaller, larger))
    assertEquals(c, test.get(c.id , value, smaller, larger))
    assertEquals(d, test.get(d.id , value, smaller, larger))
    assertEquals(e, test.get(e.id , value, smaller, larger))
  }
  
  @Test
  def findClosestTest = 
  {
    var a = new Node[String](new BaseNValue(50,10), "a")
    var b = new Node[String](new BaseNValue(45,10), "b")
    var c = new Node[String](new BaseNValue(40,10), "c")
    var d = new Node[String](new BaseNValue(30,10), "d")
    var e = new Node[String](new BaseNValue(10,10), "e")
    
    var A = new Node[String](new BaseNValue(105,10), "A")
    var B = new Node[String](new BaseNValue(115,10), "B")
    var C = new Node[String](new BaseNValue(130,10), "C")
    var D = new Node[String](new BaseNValue(140,10), "D")
    var E = new Node[String](new BaseNValue(160,10), "E")
    
    smaller = Array[Node[String]](a, b, c , d, e)
    larger = Array[Node[String]](A, B, C, D, E)
    
    var node = new BaseNValue(0, 10)
    assertEquals(null, test.findClosest(node, value, smaller, larger))    
    node = new BaseNValue(200, 10)
    assertEquals(null, test.findClosest(node, value, smaller, larger))
    node = new BaseNValue(160, 10)
    assertEquals(E, test.findClosest(node, value, smaller, larger))
    node = new BaseNValue(100, 10)
    assertEquals(A, test.findClosest(node, value, smaller, larger))
    node = new BaseNValue(141, 10)
    assertEquals(D, test.findClosest(node, value, smaller, larger))
    node = new BaseNValue(25, 10)
    assertEquals(d, test.findClosest(node, value, smaller, larger))
    
    smaller = Array[Node[String]](a, b, c , null, null)
    larger = Array[Node[String]](A, B, C, null, null)
    
    node = new BaseNValue(140)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(0)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(41)
    assertEquals(c, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(106)
    assertEquals(A, test.findClosest(node, value, smaller, larger)) 
    
    smaller = Array[Node[String]](null, null, null , null, null)
    larger = Array[Node[String]](null, null, null , null, null)
    
    node = new BaseNValue(140)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(0)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(41)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
    node = new BaseNValue(106)
    assertEquals(null, test.findClosest(node, value, smaller, larger)) 
  }
}