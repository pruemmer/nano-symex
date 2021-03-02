
import java.io.{PrintWriter, OutputStreamWriter,
                BufferedReader, InputStreamReader}

import scala.util.matching.Regex

object SMT {

  class SMTException(msg : String) extends Exception(msg)

}

/**
 * Simple interface to SMT solvers.
 */
abstract class SMT {
  val name : String

  /**
   * Declare a new constant of the given type.
   */
  def declareConst(name : String, typ : String) : Unit

  /**
   * Declare a new constant of the given type and return its name.
   */
  def freshConst(typ : String) : String

  /**
   * Add a constraint to the assertion stack.
   */
  def addAssertion(str : String) : Unit

  /**
   * Add a new frame to the assertion stack.
   */
  def push : Unit

  /**
   * Pop the topmost frame from the assertion stack.
   */
  def pop : Unit

  /**
   * Check whether the current combination of constraints is satisfiable.
   */
  def isSat : Boolean

  /**
   * Query the value of a given constant.
   */
  def getSatValue(name : String) : BigInt

  /**
   * Reset the SMT solver to the initial state.
   */
  def reset : Unit

  /**
   * Shut down the SMT solver.
   */
  def shutdown : Unit

  /**
   * Enable or disable logging output.
   */
  def logCommands(flag : Boolean)
}

abstract class SMTProcess(cmd : Array[String]) extends SMT {
  import SMT._

  private val process = Runtime.getRuntime.exec(cmd)
  private val stdin   = process.getOutputStream
  private val stderr  = process.getErrorStream
  private val stdout  = process.getInputStream

  private val stdinWriter  = new PrintWriter (new OutputStreamWriter(stdin))
  private val stdoutReader = new BufferedReader (new InputStreamReader(stdout))

  private var nameCounter = 0
  private var logCmds     = false

  val numberPattern: Regex = "([0-9]+)".r
  
  def logCommands(flag : Boolean) =
    logCmds = flag

  def sendCommand(cmd : String) : Unit = {
    if (logCmds)
      println("> " + cmd)
    stdinWriter.println(cmd)
    stdinWriter.flush
  }

  def readLine : String = stdoutReader.readLine

  def declareConst(name : String, typ : String) : Unit =
    sendCommand("(declare-const " + name + " " + typ + ")")

  def freshConst(typ : String) : String = {
    val name = "const_" + nameCounter
    nameCounter = nameCounter + 1
    declareConst(name, typ)
    name
  }

  def addAssertion(str : String) : Unit =
    sendCommand("(assert " + str + ")")

  def push : Unit =
    sendCommand("(push 1)")

  def pop : Unit =
    sendCommand("(pop 1)")

  def isSat : Boolean = {
    sendCommand("(check-sat)")
    readLine match {
      case null =>
        throw new SMTException("solver crashed")
      case "sat" =>
        true
      case "unsat" =>
        false
      case str =>
        throw new SMTException("unexpected answer from solver: " + str)
    }
  }

  def getSatValue(name : String) : BigInt = {
    sendCommand("(get-value (" + name + "))")
    readLine match {
      case numberPattern.unanchored(assignment) => BigInt(assignment)
      case str => 0
    }
  }

  def reset : Unit = {
    sendCommand("(reset)")
    nameCounter = 0
    logCmds = false
  }

  def shutdown : Unit = {
    sendCommand("(exit)")
    stdinWriter.close
    stdoutReader.close
    stderr.close
  }

}

class Z3SMT
    extends SMTProcess(Array("z3", "-in")) {
  val name = "Z3"
}

class CVC4SMT
    extends SMTProcess(Array("cvc4", "-i", "--lang=smt")) {
  val name = "CVC4"
  sendCommand("(set-logic ALL)")
}

class PrincessSMT
    extends SMTProcess(Array("princess", "+incremental", "+quiet", "+stdin")) {
  val name = "Princess"
}

