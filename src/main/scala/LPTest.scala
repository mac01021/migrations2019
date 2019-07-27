import optimus.optimization.model.MPBinaryVar

object LPTest extends App {

  import optimus.optimization._
  import optimus.optimization.enums.SolverLib

  implicit val model = MPModel(SolverLib.LpSolve)


  val x = MPBinaryVar()
  val y = MPBinaryVar()

  add(x + y + 1 := 2)
  //add(x := 1)

  println("starting")
  val solved = start()
  println(x)
  println(y)
  println(x.value)
  println(y.value)

}
