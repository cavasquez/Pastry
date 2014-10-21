package com.pastry

import scala.reflect.ClassTag

/**
 * Neighborhood set contains the node IDs and ip addresses of the M nodes that 
 * are closest (according to the proximity metric) to the local node. The
 * neighborhood set is not normally used in routing messages; it is useful in 
 * maintaining locality properties. 
 * 
 * NeighborhoodSet will have 2 x 2^b nodes
 */
class NeighborhoodSet[T:ClassTag]
{
  
}