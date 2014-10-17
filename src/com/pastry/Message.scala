package com.pastry

import scala.reflect.ClassTag

/**
 * The trait by which each actor will communicate
 */
sealed trait Message

/**
 * A Message that will contain contents and the nodeID of the intended 
 * recipient
 * @param message	The content of the PastryMessage that is being communicated
 * @param key		The key of the node (uniqueID) of the node to whom message
 * 					is being delivered. If a node with uniqueID = key is not 
 *      			found, deliver it to a node with the closest uniqueID to key
 */
case class PastryMessage(message:Content, key:Long) extends Message

/**
 * The contents of a PastryMessage. This will be extended to deal with the 
 * actual message
 */
trait Content extends Message

case class PNode[T:ClassTag](id:BaseNValue, node:T) extends Message