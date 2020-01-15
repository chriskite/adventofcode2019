package day2

import cats.implicits._

object Computer {
  type Mem = IndexedSeq[Int]

  sealed trait Instruction
  sealed trait IntInstruction extends Instruction {
    val op1: Int
    val op2: Int
    val addr: Int
    val fn: (Int, Int) => Int
  }
  final case class Add(op1: Int, op2: Int, addr: Int) extends IntInstruction {
    val fn = _ + _
  }
  final case class Multiply(op1: Int, op2: Int, addr: Int) extends IntInstruction {
    val fn = _ * _
  }
  final object Halt extends Instruction

  def compute(mem: Mem, pc0: Int = 0): Option[Mem] = {
    val pc = pc0 + 4
    parseInstruction(mem.slice(pc0, pc)).flatMap {
      case Halt => Some(mem)
      case i    => updated(mem, i).flatMap(compute(_, pc))
    }
  }

  def parseInput(input: String): Option[Mem] =
    input.trim.split(",").toList.map(_.toIntOption).sequence.map(_.toIndexedSeq)

  private def parseInstruction(codes: Mem): Option[Instruction] =
    for {
      opcode <- codes.lift(0)
      op1    <- codes.lift(1)
      op2    <- codes.lift(2)
      addr   <- codes.lift(3)
      instruction <- opcode match {
        case 1  => Some(Add(op1, op2, addr))
        case 2  => Some(Multiply(op1, op2, addr))
        case 99 => Some(Halt)
        case _  => None
      }
    } yield instruction

  private def updated(mem: Mem, instruction: Instruction): Option[Mem] =
    instruction match {
      case i: IntInstruction =>
        for {
          a <- mem.lift(i.op1)
          b <- mem.lift(i.op2)
        } yield mem.updated(i.addr, i.fn(a, b))
      case Halt => Some(mem)
    }
}
