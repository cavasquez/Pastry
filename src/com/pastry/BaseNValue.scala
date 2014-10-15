/**

 */
package com.pastry

class BaseNValue(base10Val:Long, base:Int = 10)
{
	private var value = makeValue()
  
	/**
	 * Makes the base_base value of the provided base10Val in little-endian
	 * notation
	 * @param base10Val 	the base 10 representation of the value
	 * @param base			the base of the value
	 * @param numOfDigits	the number of digits in the base_base value
	 */
	private[pastry] def makeValue(base10Val:Long = base10Val, base:Int = base, digits:Int = numOfDigits()):Array[Int]=
	{
	  var temp = base10Val
	  var value = new Array[Int](digits)
	  var i = 0
	  
	  /* Build the value in little-endian */
	  for(i <- 0 until digits)
	  {
	    value(i) = (temp % base).toInt
	    temp = temp/base
	  }
	  
	  return value
	}
	
	/**
	 * Calculates the number of digits in the given base representation of 
	 * base10Val
	 * @param base10Val	the base 10 representation of the value
	 * @param base		the base of the value
	 * @return			the number of digits in the base representation of 
	 * 					base10Val
	 */
	def numOfDigits(base10Val:Long = base10Val, base:Int = base):Int = Math.floor(logBase(base, base10Val)).toInt + 1
	
	/**
	 * Returns the log of x base n
	 * @param n the base
	 * @param x	the value
	 * @return 	the log of x base n
	 */
	def logBase(n:Long, x:Long):Double =
	{
	  if(n < 1 || x < 1) throw new IllegalArgumentException("Arguments must be greater than 0")
	  else Math.log(x)/Math.log(n)
	}
	
	/**
	 * Returns the string representation of value. Each digit is separated by 
	 * the '|' character and prefixed with "yx" where y is the value of the base
	 * @param value		the value whose string is being returned
	 * @param base		the base of the value
	 * @param length	the length (number of digits) in value
	 * @return			returns the string representation of the current string
	 */
	def toString(value:Array[Int] = value, base:Int = base, length:Int = numOfDigits()):String = 
	{
	  var temp:StringBuilder = new StringBuilder("%sx".format(base));
	  
	  for(i:Int <- (length - 1) until 0 by -1)
	  {
	    temp.append(value(i)).append('|')
	  }
	  temp.append(value(0))
	  return temp.toString
	}
	
	override def toString():String = toString(value, base, numOfDigits())
}