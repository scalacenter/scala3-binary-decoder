package ch.epfl.scala.decoder.binary

enum Instruction:
  case Method(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean)
  case Field(opcode: Int, owner: String, name: String, descriptor: String)
  case Variable(name: String, descriptor: String, signature: String)
