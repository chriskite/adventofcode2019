package day2
import better.files.Resource
import zio.console.putStrLn
import zio.{IO, ZIO}

object Part1 extends zio.App {
  val program = IO.fromOption {
    for {
      input <- Resource.asString("day2/input.txt")
      mem   <- Computer.parseInput(input)
      initialMem = mem.updated(1, 12).updated(2, 2)
      finalMem <- Computer.compute(initialMem)
      head     <- finalMem.headOption
    } yield head
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    program.flatMap(head => putStrLn(head.toString)).fold(_ => 1, _ => 0)

}
