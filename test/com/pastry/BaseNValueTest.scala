package com.pastry

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

/**
 * Tests com.pastry.BaseNValue
 */
class BaseNValueTest extends AssertionsForJUnit
{
	var test:BaseNValue = null
	
	@Before
	def initialize() =
	{
	  test = new BaseNValue(10)
	}
	
	@Test
	def makeValueTest() =
	{
	  var temp = test.makeValue(base10Val = 150, base = 10, digits = 3)
	  assertEquals(1, temp(2))
	  assertEquals(5, temp(1))
	  assertEquals(0, temp(0))
	  
	  temp = test.makeValue(base10Val = 5, base = 2, digits = 3)
	  assertEquals(1, temp(2))
	  assertEquals(0, temp(1))
	  assertEquals(1, temp(0))
	}
	
	@Test
	def numOfDigitsTest() = 
	{
	  assertEquals(3, test.numOfDigits(100, 10))
	  assertEquals(1, test.numOfDigits(1, 10))
	  assertEquals(4, test.numOfDigits(10, 2))
	}
	
	@Test
	def logBaseTest() =
	{
	  assertEquals(1, test.logBase(10, 10), 0)
	  assertEquals(2, test.logBase(10, 100), 0)
	  try 
	  { 
		  assert(null, test.logBase(0, 10))
		  fail("Taking the log base 0 should throw an exception")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	  
	  try 
	  { 
		  assert(null, test.logBase(10, -100))
		  fail("Taking the log of a negative number should throw an exception")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	}
	
	@Test
	def toStringTest() = 
	{
	  test = new BaseNValue(base10Val = 10, base = 2)
	  assertEquals("2x1|0|1|0", test.toString())
	  
	  test = new BaseNValue(base10Val = 15387, base = 10)
	  assertEquals("10x1|5|3|8|7", test.toString())
	}
	
	@Test
	def longestCommonPrefixTest() =
	{
	  assertEquals(5, test.numOfDigits(23501, 10))
	  assertEquals(3, test.longestMatchingPrefix(23501, 23558, 10))
	  assertEquals(0, test.longestMatchingPrefix(123456, 563215, 10))
	  assertEquals(4, test.longestMatchingPrefix(245, 255, 2))
	  assertEquals(0, test.longestMatchingPrefix(1245, 255, 2))
	}
	
	@Test
	def nextDigitTest() =
	{
	  assertEquals(4, test.nextDigit(3, 1234, 10))
	  assertEquals(4, test.nextDigit(3, 12345678, 10))
	  assertEquals(2, test.nextDigit(1, 12345678, 10))
	  assertEquals(8, test.nextDigit(7, 12345678, 10))
	  assertEquals(1, test.nextDigit(0, 12345678, 10))
	  
	  try 
	  { 
		  assertEquals(8, test.nextDigit(8, 12345678, 10))
		  fail("There cannoot be a digit after the last")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	  
	  try 
	  { 
		  assertEquals(8, test.nextDigit(8, 12345678, 10))
		  fail("There cannoot be a digit after the last")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	  
	  try 
	  { 
		  assertEquals(8, test.nextDigit(-1, 12345678, 10))
		  fail("There is no negative digit")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	  
	  try 
	  { 
		  assertEquals(8, test.nextDigit(5, -12345678, 10))
		  fail("Value cannot be negative")
	  }
	  catch 
	  {
      	case e:IllegalArgumentException =>
      	case _:Throwable => fail("This should have thrown an IllegalArgumentException")
	  }
	}
	
	@Test
	def notEqualTest() =
	{
	  var a = new BaseNValue(5,10)
	  var b = new BaseNValue(10,10)
	  var c = new BaseNValue(5,10)
	  assertEquals(true, a != b)
	  assertEquals(false, a != c)
	  assertEquals(true, a != null)
	}
}