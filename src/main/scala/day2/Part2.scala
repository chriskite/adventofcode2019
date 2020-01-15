package day2
import better.files.Resource
import day2.Computer.Mem
import zio.console.putStrLn
import zio.{IO, ZIO}

object Part2 extends zio.App {
  case class Input(noun: Int, verb: Int)

  def computation(initialMem: Mem, noun: Int, verb: Int): Option[Int] = {
    val mem = initialMem.updated(1, noun).updated(2, verb)
    for {
      finalMem <- Computer.compute(mem)
      result   <- finalMem.headOption
    } yield result
  }

  def parseMem: IO[Unit, Mem] = IO.fromOption {
    for {
      input <- Resource.asString("day2/input.txt")
      mem   <- Computer.parseInput(input)
    } yield mem
  }

  def program(desired: Int): IO[Unit, Int] = {
    def go(mem: Mem, inputs: List[Input]): Option[Int] = inputs match {
      case x :: _ if computation(mem, x.noun, x.verb).contains(desired) => Some(x.noun * 100 + x.verb)
      case _ :: xs                                                      => go(mem, xs)
      case Nil                                                          => None
    }

    val inputs: List[Input] = (for {
      noun <- 0 to 99
      verb <- 0 to 99
    } yield Input(noun, verb)).toList

    for {
      mem    <- parseMem
      result <- IO.fromOption(go(mem, inputs))
    } yield result
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    program(19690720).flatMap(head => putStrLn(head.toString)).fold(_ => 1, _ => 0)

}
