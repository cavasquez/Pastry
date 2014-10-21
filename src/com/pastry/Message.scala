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

/**
 * A Route message containing the key of a recipient and a message for the 
 * recipient
 * @param key		the key of the recipient
 * @param message	a message for the recipient
 */
case class Route[T:ClassTag](key:BaseNValue, message:Message) extends Message

/**
 * A forwarded 
 */
case class Deliver[T:ClassTag](key:BaseNValue, message:Message) extends Message

/**
 * Contains a node and it's id.
 * @param id	the id of the node
 * @param node	the node
 */
case class Node[T:ClassTag](id:BaseNValue, node:T) extends Message