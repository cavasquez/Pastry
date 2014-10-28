package com.simplepastry

/**
 * This class will simulate a pastry network.
 * @param n				The number of nodes in the pastry network
 * @param base			The base of the numeric system used by this pastry
 * 						network
 * @param l				A parameter that affects the size of the leaf set and 
 * 						neighborhood set
 */
class Driver(n:Int = 10, base:Int = 4, l:Int = 16)
{
  /* b should ideally be related to the number of digits in our base. This will
   * be the log[base 2] of our base so that the routing table can have a 
   * number of columns equal to the number of digits in the base. This assumes
   * that the base is a multiple of 2 */
  val b = Math.log(base).toInt
}