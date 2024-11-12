package com.axiommd

case class Message(typ:String, value:String)


import upickle.default._


object Message{
  implicit val rw: ReadWriter[Message] = macroRW
}

