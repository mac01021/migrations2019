import ch.qos.logback.classic.{Level, Logger}
import optimus.algebra.{Expression, Zero}
import optimus.optimization.model.{MPBinaryVar, MPIntVar}

import scala.collection.{LazyZip4, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

object Migrations {

  type Node = Int
  /**
    * This is just an array, except the range of indices can be 0 to 9, or 1 to 10, or 2 to 11, or x to y, for any x <= y
    * @param rng
    * @tparam E
    */
  class Sequence[E](val rng: Range)(implicit classTag: ClassTag[E]) extends Iterable[E] {
    private val array: Array[E] = if (rng.step != 1) throw new IllegalArgumentException else new Array(rng.length)
    def apply(i: Int): E = array(i-rng.start)
    def update(i: Int, e: E) = array(i-rng.start) = e
    def length = array.length

    override def toString: String = {
      val buf = new StringBuilder
      for { idx <- rng } {
        buf append s"$idx: ${this(idx)}\n"
      }
      buf.toString()
    }

    def copy: Sequence[E] = {
      val clone = new Sequence[E](rng)
      for { i <- rng } {
        clone(i) = this(i)
      }
      clone
    }

    def sortInPlaceBy[B](f: E=>B)(implicit ord: Ordering[B]) = array.sortInPlaceBy(f)

    override def iterator: Iterator[E] = array.iterator
  }

  def time(block: => Schedule): Schedule = {
    val t0 = System.currentTimeMillis()
    try {
      val result = block // call-by-name
      println(s"Found a schedule with ${result.length} stages.")
      result
    } finally {
      val t1 = System.currentTimeMillis()

      println("Elapsed time: " + (t1 - t0) + " ms")
    }
  }

  /**
    * A file has a source and a destination
    * @param src
    * @param dst
    */
  class File(val src: Int, val dst: Int) {
    def incidesWithSrcOf(that: File): Boolean = this.src == that.src || this.dst == that.src
    def incidesWithDstOf(that: File): Boolean = this.dst == that.dst || this.src == that.dst

    override def toString: String = s"${Integer.toHexString(hashCode())}[$src -> $dst]"
  }

  object File {
    def random(nbNodes: Int): File = new File(Random.between(1, nbNodes+1), Random.between(1, nbNodes+1))
  }


  /**
    * An instance is a set of files to migrate, plus the number of network interfaces (nifs) (ie the transmission capacity)
    * of each node.
    * @param files
    * @param nifs
    */
  class Instance(val files: Seq[File], val nifs: Sequence[Int], var space: Sequence[Int] = null) {
    val nodes = nifs.rng

    if (space==null) {
      space = new Sequence[Int](nodes)
      for {node <- nodes} {
        space(node) = 1 + Math.max(files.count(_.src == node),
                                   files.count(_.dst == node))
      }
    }

    def shuffle: Instance = new Instance(Random.shuffle(files), nifs)

    def augment(sent: Set[File]): Instance = {
      val remainingFiles = for { file <- files
                                 if ! sent.contains(file) }
                             yield file
      val newSpace = space
      new Instance(remainingFiles, nifs, newSpace)
    }

    override def toString: String = {
      val buf = new StringBuilder
      for { file <- files } {
        buf.append("\t")
        buf.append(file)
        buf.append("\n")
      }

      "Space:\n"+space+"\n\nNifs:\n"+nifs+"\n\nFiles:\n"+buf
    }
  }

  object Instance {
    def random(N: Int, F: Int, minC: Int = 1, maxC: Int = 10)  = {

      val nifs = new Sequence[Int](1 to N) {
        for { n <- 1 to N } {
          this(n) = Random.between(minC, maxC+1)
        }
      }

      val nbFiles = F
      //val nbFiles = N * Math.log(N).toInt
      //val nbFiles = N * N
      val files = Stream.continually(File.random(N)).filter(f => f.src != f.dst).take(nbFiles)

      new Instance(files, nifs)
    }

  }

  /**
    * A schedule is a sequence of stages, where each stage represents the set of files to be migrated at one time.
    * @param plan
    */
  case class Schedule(plan: IterableOnce[Set[File]]){
    val stages: Seq[Set[File]] = Seq.from(plan)

    def length = stages.size

    def filesForStage(stage: Int) = stages(stage-1)

    def stageForFile(file: File) = stages.find(_.contains(file))

    override def toString: String = {
      val buf = new StringBuilder
      for { (stage, i) <- stages.zipWithIndex } {
        buf.append(i+1)
        buf.append(": ")
        buf.append(stage)
        buf.append("\n")
      }
      return buf.toString
    }

  }

  def check(schedule: Schedule, instance: Instance): Unit = {
    for { file <- instance.files} {
      val timesSent = schedule.stages.count(_ contains file)
      require(timesSent == 1, s"file: $file, times sent: $timesSent != 1 {\n\t $schedule \n}\n\n${instance.files}")
    }

    for { node <- instance.nodes
          stage <- schedule.stages }  require(stage.count(_.src == node) + stage.count(_.dst == node) <= instance.nifs(node))
  }

  def bestof(n: Int, scheduler: Instance => Schedule)(instance: Instance): Schedule = time {
    var minlength = Int.MaxValue
    var minSched: Schedule = null
    for { i <- 1 to n } {
      val schedule = scheduler(instance.shuffle)
      if (schedule.length < minlength) {
        minlength = schedule.length
        minSched = schedule
      }
    }
    minSched
  }

  def computeSchedule(scheduler: Instance=>Schedule)(instance: Instance): Schedule = {
    val schedule = time {
      scheduler(instance)
    }
    check(schedule, instance)
    schedule
  }



  /**
    * Get a schedule for the given instance by greedy matching.
    * @param instance
    * @return
    */
  def scheduleByGreedyMatching(instance: Instance): Schedule = {
    val plan = new ArrayBuffer[Set[File]]()
    val alreadyScheduled = mutable.Set[File]()
    val freeSpace = new Sequence[Int](instance.nodes) {
      for { node <- instance.nodes } {
        this(node) = instance.space(node) - instance.files.count(_.src==node)
      }
    }

    def makeStage = {
      val currentStage = mutable.Set[File]()
      for { file <- instance.files
            if !alreadyScheduled.contains(file)
            if currentStage.count(_ incidesWithSrcOf file) < instance.nifs(file.src)
            if currentStage.count(_ incidesWithDstOf file) < instance.nifs(file.dst)
            if freeSpace(file.dst) > 0} {

        currentStage += file
        freeSpace(file.src) = freeSpace(file.src) + 1
        freeSpace(file.dst) = freeSpace(file.dst) - 1
        alreadyScheduled += file
      }
      plan += Set.from(currentStage)
    }

    while (alreadyScheduled.size < instance.files.size) {
      makeStage
    }
    Schedule(plan)
  }

  def scheduleByGreedyLP(instance: Instance): Schedule = {
    var curInstance = instance
    def solveLp: Set[File] = {
      val nodes = curInstance.nodes
      val space = curInstance.space
      val nifs = curInstance.nifs
      val files = curInstance.files

      import optimus.optimization._
      import optimus.optimization.enums.SolverLib
      implicit val model = MPModel(SolverLib.LpSolve)
      val vars = (for { file <- files} yield file -> MPBinaryVar()).toMap

      def initial(node: Int) = files.count(_.src==node)

      def incoming(node: Int) = for { file <- files
                                      if file.dst==node }
                                  yield vars(file)

      def outgoing(node: Int) = for { file <- files
                                      if file.src==node }
                                  yield vars(file)

      def engaging(node: Int) = outgoing(node) ++ incoming(node)


      def sum(xs: Iterable[Expression]): Expression = xs.fold(Zero)((x, y)=>x+y)


      for { node <- nodes } {
        add(sum(engaging(node)) <:= nifs(node))
        add(initial(node) + sum(incoming(node)) - sum(outgoing(node)) <:= space(node))

      }


      maximize(sum(for { file <- files } yield vars(file)))
      try {
        if (start()) {
          return (for { file <- files
                        if vars(file).value.get > 0 } yield file).toSet
        } else {
          throw new NoSuchElementException
        }
      } finally {
        release()
      }
    }

    val rounds = new ArrayBuffer[Set[File]]
    while (curInstance.files.size > 0) {
      val filesForRound = solveLp
      curInstance = curInstance.augment(filesForRound)
      rounds += filesForRound
    }
    Schedule(rounds)
  }



  def scheduleByDstSpace(instance: Instance): Schedule = {

    val rounds = new ArrayBuffer[Set[File]]

    val freeSpace: Sequence[Int] = instance.space.copy
    for { f <- instance.files } freeSpace(f.src) = freeSpace(f.src) - 1
    val files = ArrayBuffer.from(instance.files)

    var freeCapacity = instance.nifs.copy

    var currentRound = Set[File]()
    while (files.size > 0) {
      files.sortInPlaceBy[Int](f => -freeSpace(f.dst))
      val fileToSchedule = files.find(file => freeCapacity(file.src) > 0 && freeCapacity(file.dst) > 0)
      if (fileToSchedule.isDefined) {
        files -= fileToSchedule.get
        freeCapacity(fileToSchedule.get.src) = freeCapacity(fileToSchedule.get.src) - 1
        freeCapacity(fileToSchedule.get.dst) = freeCapacity(fileToSchedule.get.dst) - 1
        freeSpace(fileToSchedule.get.src) = freeSpace(fileToSchedule.get.src) + 1
        freeSpace(fileToSchedule.get.dst) = freeSpace(fileToSchedule.get.dst) - 1
        currentRound = currentRound + fileToSchedule.get
      } else {
        rounds += currentRound
        currentRound = Set[File]()
        freeCapacity = instance.nifs.copy
      }
    }
    rounds += currentRound
    Schedule(rounds)
  }


  def scheduleBySpaceAndDegree(instance: Instance): Schedule = {
    val rounds = new ArrayBuffer[Set[File]]

    val capacity = instance.nifs
    val freeSpace: Sequence[Int] = instance.space.copy
    for { f <- instance.files } freeSpace(f.src) = freeSpace(f.src) - 1
    val files = ArrayBuffer.from(instance.files)

    //class Connections(val incoming: mutable.Set[File] = mutable.Set(), val outgoing: mutable.Set[File] = mutable.Set())
    val cxns = new Sequence[Connections](instance.nodes)
    for {node <- instance.nodes } cxns(node) = new Connections()
    for { file <- files } {
      cxns(file.src).outgoing += file
      cxns(file.dst).incoming += file
    }

    while (files.size > 0) {
      // Get the degrees of all the nodes
      val degree = new Sequence[Int](instance.nodes)
      for { f <- files } {
        degree(f.src) = degree(f.src) + 1
        degree(f.dst) = degree(f.dst) + 1
      }
      //sort the nodes by space and degree
      val nodes = new Sequence[Int](instance.nodes)
      for { n <- nodes.rng } nodes(n) = n
      nodes.sortInPlaceBy(node => (-freeSpace(node), degree(node) * -1.0 / capacity(node)))


      var currentRound = Set[File]()
      val freeCapacity = capacity.copy
      for { node <- nodes } {
        def tryFile(file: File) = {
          if (freeCapacity(file.src) > 0 && freeCapacity(file.dst) > 0 && freeSpace(file.dst) > 0) {
            files -= file
            cxns(file.src).outgoing -= file
            cxns(file.dst).incoming -= file
            freeCapacity(file.src) = freeCapacity(file.src) - 1
            freeCapacity(file.dst) = freeCapacity(file.dst) - 1
            freeSpace(file.src) = freeSpace(file.src) + 1
            freeSpace(file.dst) = freeSpace(file.dst) - 1
            currentRound += file
          }
        }
        for { file <- cxns(node).incoming } tryFile(file)
        for { file <- cxns(node).outgoing } tryFile(file)
      }
      rounds += currentRound
    }
    Schedule(rounds)
  }

  /**
    *
    */
  def scheduleByMip(instance: Instance): Schedule = {
    print("...")
    import optimus.optimization._
    import optimus.optimization.enums.SolverLib

    def solveLpForSchedule(tf: Int) = {
      print(tf+", ")
      implicit val model = MPModel(SolverLib.LpSolve)
      val vars = (for { t <- 1 to tf
                        file <- instance.files} yield (file, t) -> MPBinaryVar()).toMap

      def initial(node: Int) = instance.files.count(_.src==node)

      def incoming(node: Int, t: Int) = for { (file, stage) <- vars.keys
                                              if file.dst==node && stage==t }
        yield vars((file, stage))

      def outgoing(node: Int, t: Int) = for { (file, stage) <- vars.keys
                                              if file.src==node && stage==t }
        yield vars((file, stage))

      def engaging(node: Int, t: Int) = outgoing(node, t) ++ incoming(node, t)


      def sum(xs: Iterable[Expression]): Expression = xs.fold(Zero)((x, y)=>x+y)

      for { file <- instance.files } {
        add(sum(for { (f, t) <- vars.keys
                      if (f == file) } yield vars((f, t))) := 1)
      }
      for { node <- instance.nodes
            t <- 1 to tf } {
        add(sum(engaging(node, t)) <:= instance.nifs(node))
        add(initial(node) + sum(for { stage <- 1 to t } yield sum(incoming(node, stage)) - sum(outgoing(node, stage))) <:= instance.space(node))

      }

      def filesForStage(t: Int): Seq[File] = for { file <- instance.files
                                                   if vars((file, t)).value.get == 1.0 }
        yield file

      def schedule = Schedule(for { t <- 1 to tf }
                                yield filesForStage(t).toSet)

      try {
        if (start()) {
          schedule
        } else {
          throw new NoSuchElementException
        }
      } finally {
        release()
      }
    }


    /*
    var tf = 1
    var ubSched: Schedule = null
    do {
      try {
        ubSched = solveLpForSchedule(tf)
      } catch {
        case _: NoSuchElementException => tf = tf*2
      }
    } while (ubSched == null)


    var ub = ubSched.length
    var lb = ub/2
    while (ub > lb) {
      val m = (lb + ub)/2
      println(s"$lb $m $ub")
      try {
        val midSched = solveLpForSchedule(m)
        ub = m
        ubSched = midSched
      } catch {
        case _: NoSuchElementException => lb = m+1
      }
    }

    ubSched
     */

    var tf = 1
    while (true) try {
      val sched = solveLpForSchedule(tf)
      return sched
    } catch {
      case _: NoSuchElementException => tf = tf + 1
    }
    ???


  }


  class Connections(val incoming: mutable.Set[File] = mutable.Set(), val outgoing: mutable.Set[File] = mutable.Set()) {
    def copy = new Connections(mutable.Set.from(incoming), mutable.Set.from(outgoing))

    override def toString: String =
      s"""incoming: $incoming
         |outgoing: $outgoing
       """.stripMargin
  }

  def scheduleByStructure(instance: Instance): Schedule = {
    val nodes = instance.nodes
    val files = mutable.Set.from(instance.files)
    val space = instance.space
    val c = instance.nifs

    val freeSpace: Sequence[Int] = space.copy
    for { f <- files } freeSpace(f.src) = freeSpace(f.src) - 1


    val cxns = new Sequence[Connections](nodes)
    for {node <- nodes } cxns(node) = new Connections()
    for { file <- files } {
      cxns(file.src).outgoing += file
      cxns(file.dst).incoming += file
    }


    println("getting cycles...")
    val cycles = getCycles(cxns.copy)
    //println("Got them")
    //println(s"Got cycles: $cycles")
    for { cycle <- cycles
          file <- cycle } files -= file

    println("making dag...")
    val dag = new Sequence[Connections](nodes)
    for {node <- nodes } dag(node) = new Connections()
    for { file <- files } {
      dag(file.src).outgoing += file
      dag(file.dst).incoming += file
    }

    val cycleInstance = instance.augment(files.toSet)
    println("schedulinging cycles...")
    val cycleSchedule = scheduleByGreedyMatching(cycleInstance)




    //val dagInstance = instance.augment(cycles)

    val ranks = nodes
    println("sorting dag...")
    val nodesByRank = toposort(dag)

    println("scheduling dag...")
    val dagRounds = new ArrayBuffer[Set[File]]()
    while (files.size > 0) {
      val lastSz = files.size
      var currentRound = Set[File]()

      val degree = new Sequence[Int](instance.nodes)
      for { f <- files } {
        degree(f.src) = degree(f.src) + 1
        degree(f.dst) = degree(f.dst) + 1
      }

      val remainingCapacity = c.copy

      for { rank <- ranks
            node = nodesByRank(rank) } {
        var incoming: Seq[File] = cxns(node).incoming.toSeq.sortBy(file => -degree(file.src))
        //println("scheduling receptions by node " + node)
        while (remainingCapacity(node) > 0 && freeSpace(node) > 0 && incoming.size > 0)  {
          val file = incoming.head
          if (remainingCapacity(file.src) > 0) {
            remainingCapacity(node) -= 1
            remainingCapacity(file.src) -= 1
            files -= file
            cxns(node).incoming -= file
            cxns(file.src).outgoing -= file
            freeSpace(node) -= 1
            freeSpace(file.src) += 1
            currentRound = currentRound + file
          }
          incoming = incoming.tail
        }
      }
      dagRounds += currentRound

      if (files.size == lastSz) {
        println("Instance:\n"+instance)
        println("Cyclic Instance:\n"+cycleInstance)
        println(s"Space:\n$space Free:\n$freeSpace Dag:\n$dag")
        println(s"\n\n\t$files remaining")
        require(files.size < lastSz)
      }
    }

    println("concattenating schedules...")
    Schedule(cycleSchedule.stages ++ dagRounds)
  }

  private def getCycles(cxns: Migrations.Sequence[Connections]): Iterable[Iterable[File]] = {
    val cycles = new ArrayBuffer[Iterable[File]]()
    val nodes = cxns.rng

    def dfs(node: Int, edge: File = null, seen: Set[Int] = Set.empty[Int]): Option[(Node, List[File], Boolean)] = {
      //for { i <- 1 to seen.size } print("\t")
      //println(s"[$node  $edge    $seen]")
      if (seen contains node) {
        return Some((node, Nil, false))
      }
      for { e <- cxns(node).outgoing } {
        dfs(e.dst, e, seen + node) match {
          case Some((n, es, true)) => return Some((node, es, true))
          case Some((n, es, false)) if n == node => return Some((n, e::es, true))
          case Some((n, es, false)) => return Some((n, e::es, false))
          case None => {}
        }
      }
      None
    }

    def findCycle: Option[Iterable[File]] = {
      for { root <- nodes } {
        //println("dfs from root "+ root)
        dfs(root) match {
          case Some((_, files, _)) => {
            for { file <- files } {
              cxns(file.src).outgoing -= file
              cxns(file.dst).incoming -= file
            }
            //println(s"one cycle is $files")
            return Some(files)
          }
          case None => {}
        }
      }
      None
    }

    var cycle = findCycle
    while (cycle.isDefined) {
      cycles += cycle.get
      cycle = findCycle
    }

    cycles
  }

  private def toposort(cs: Migrations.Sequence[Migrations.Connections]): Sequence[Int] = {
    val nodes = cs.rng
    val cxns = new Sequence[Connections](nodes)
    for { node <- nodes } {
      cxns(node) = cs(node).copy
    }

    val placed = new Sequence[Boolean](nodes)
    val nodesByRank = new Sequence[Int](1 to nodes.size)

    var nbPlaced = 0

    def done = nbPlaced == nodes.size
    def place(n: Int) = {
      require(!placed(n))
      placed(n) = true
      nbPlaced += 1
      nodesByRank(nbPlaced) = n
      for { e <- cxns(n).incoming } {
        cxns(e.src).outgoing -= e
      }
    }

    while (!done) {
      var node = nodes.find(!placed(_)).get
      while (!cxns(node).outgoing.isEmpty) {
        val edge = cxns(node).outgoing.head
        node = edge.dst
      }
      place(node)
    }

    nodesByRank
  }


}



object MigrationsApp extends App {
  import Migrations._

  import org.slf4j.LoggerFactory

  val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
  root.detachAndStopAllAppenders()

  println("let's go!")
  val n = 1200
  val instances = for { i <- 1 to 10 } yield Instance.random(n, n*100, minC=1, maxC=8)

  println("NAIVE\n=============")

  val grd:Instance=>Schedule = computeSchedule(scheduleByGreedyMatching)
  val greedyLengths = for { instance <- instances }
                        yield grd(instance).length

  println("\n\nGREEDY BY EDGE\n=============")
  val gbe: Instance => Schedule = computeSchedule(scheduleByDstSpace)
  val gbeLengths = for {instance <- instances }
                     yield gbe(instance).length


  println("\n\nGREEDY BY NODE\n=============")
  val gbn: Instance => Schedule = computeSchedule(scheduleBySpaceAndDegree)
  val gbnLengths = for {instance <- instances }
                     yield gbn(instance).length

  println("\n\nCYCLE AND DAG\n=============")
  val str: Instance => Schedule = computeSchedule(scheduleByStructure)
  val strLengths = for {instance <- instances }
                     yield str(instance).length





  println("\n\nCompare:\nNAIVE -- GREEDY_BY_EDGE -- GREEDY_BY_NODE -- CYCLE_AND_DAG \n===============")
  for { (w, x, y, z) <- (greedyLengths lazyZip  gbeLengths lazyZip  gbnLengths lazyZip  strLengths ) } {
    println(s"$w -- $x -- $y -- $z")
  }
  println(s"\nAverage schedule length: ${greedyLengths.sum*1.0/greedyLengths.size} <<>> ${gbeLengths.sum*1.0/gbeLengths.size} <<>> ${gbnLengths.sum*1.0/gbnLengths.size} <<>> ${strLengths.sum*1.0/strLengths.size} ")
}


object SillyTest extends App {
  import org.slf4j.LoggerFactory
  val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
  root.detachAndStopAllAppenders()

  import Migrations._

  val instance = Instance.random(10, 2, 2)

  /*
  val nifs = new Sequence[Int](1 to 3)
  for { i <- 1 to 3 } nifs(i) = 2
  val instance = new Instance( List(new File(1, 2),
                                    new File(1, 2),
                                    new File(3, 1),
                                    new File(3, 1),
                                    new File(2, 3),
                                    new File(2, 3),
                                    new File(1, 2)), nifs = nifs)

   */

  println(instance)

  println("\n\nNaive:\n=====================")
  println(computeSchedule(scheduleByGreedyMatching)(instance))

  println("\n\nEdgeRanking:\n=====================")
  println(computeSchedule(scheduleByDstSpace)(instance))

  println("\n\nNodeRanking:\n=====================")
  println(computeSchedule(scheduleBySpaceAndDegree)(instance))

  println("\n\nOpt:\n=====================")
  println(computeSchedule(scheduleByMip)(instance))

  println("\n\nStruct:\n=====================")
  println(computeSchedule(scheduleByStructure)(instance))


}
