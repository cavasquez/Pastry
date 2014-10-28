package com.simplepastry

import com.pastry
import akka.actor.ActorRef

/**
 * The messages for com.simplepastry
 */
trait Message extends pastry.Message

/**
 * Indicates that node should send a message to the node with id key
 */
case class Send(key:BigInt) extends Message

/**
 * Indicates that a node received a message in hop hops
 */
case class Simple(hop:Int) extends Message

/**
 * Indicates that the node received a forwarded message
 */
case class Forward(id:BigInt) extends Message

/**
 * Tells Master to send a message to to and from from
 */
case class StartSend(from:BigInt, to:BigInt) extends Message

/**
 * Instructs master to add node to the map with the given id
 */
case class AddNode(id:BigInt, node:ActorRef) extends Message

/**
 * Indicates that id has received the simple message
 */
case class ReceivedSimple(id:BigInt, hop:Int) extends Message