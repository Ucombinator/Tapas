package com.ucombinator.dalvik.AST

abstract class Instruction {
  var next: Instruction = null
}

case object End extends Instruction

class Nop extends Instruction

abstract case class MoveInstruction

// wide instructions get expanded to two adjacent instructions
class Move_4_4 extends MoveInstruction
class Move_8_8 extends MoveInstruction
class Move_16_16 extends MoveInstruction

class MoveObject extends MoveInstruction
class MoveResult extends MoveInstruction
class MoveException extends MoveInstruction
// TODO more moves ...

