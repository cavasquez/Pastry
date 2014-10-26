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
case class PastryMessage(message:Content, key:BigInt) extends Message

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
case class Route(key:BaseNValue, message:Message) extends Message

/**
 * Contains a node and it's id.
 * @param id	the id of the node
 * @param node	the node
 */
case class Node[+T:ClassTag](id:BaseNValue, node:T) extends Message

/**
 * The initializing message sent to a PastryNode
 * @param credentials	The Pastry credentials
 * @param application	the application that Pastry will use
 */
case class PastryInit(credentials:Credentials, application:Application) extends Message

/**
 * A special message sent by the requester to join a network with the receiver
 * in it.
 */
case class Join(node:Node[Any], hop:Int)
/**
 * A special message sent to the Requested of a join message. It contains how
 * far the receiver is from the sender and contains the receivers state tables.
 * @param hop			The distance from the receiver to sender
 * @param leaf			The leaf set of the receiver
 * @param route			The Routing Table of the receiver
 * @param neighborhood	The Neighborhood Set of the receiver
 */
case class StateTables(hop:Int, leaf:LeafSet[Any], route:RoutingTable[Any], neighborhood:NeighborhoodSet[Any])

/**
 * Taken by com.pastry.PastryNode.pastryInit to obtain application-specific 
 * credentials.
 */
trait Application 

/**
 * An initialization message that contains the credentials and application 
 * details of the node
 * @param cred	The nodes credentials
 * @param app	The nodes application data
 */
case class Init(cred:Credentials, app:Application)