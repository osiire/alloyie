package com.falso.alloy.executor

import com.falso.alloy.SignatureInstance
import com.falso.alloy._
import edu.mit.csail.sdg.alloy4.{ConstList, ErrorAPI, A4Reporter}
import edu.mit.csail.sdg.alloy4compiler.ast.{Sig, Command}
import edu.mit.csail.sdg.alloy4compiler.parser.{CompModule, CompUtil}
import edu.mit.csail.sdg.alloy4compiler.translator._

import scala.annotation.tailrec

/**
 * alloy実行機
 */
class AlloyExecutor {
  import collection.JavaConversions._


  /**
   * returns all instances of the specified model.
   * @param model
   * @param commandLabel
   * @return
   */
  def run(model:String, commandLabel:Option[String]):Set[Example] = {

    val reporter:A4Reporter  = new A4Reporter()
    val world:CompModule = CompUtil.parseEverything_fromString(reporter, model)
    val commands:ConstList[Command]  = world.getAllCommands()
    if( commands.size <= 0) {
      throw new ErrorAPI("Model must have atleast one command; no commands found.")
    }
    val cmd:Command = commandLabel.flatMap(label =>
      commands.find( _.label.equals(label) )
    ).getOrElse( commands.head )

    val opt:A4Options = new A4Options()
    opt.solver = A4Options.SatSolver.SAT4J

    // solve
    val sol:A4Solution = TranslateAlloyToKodkod.execute_command(reporter, world.getAllSigs(), cmd, opt)

    @tailrec
    def loop(instances:Set[Example], sol:A4Solution) : Set[Example] = {
      if(sol.satisfiable()) {
        println(sol.toString)
        val example = convertSol2Example(world, sol).resolveName
        println(example.toString)
        val instancesNext = instances + example
        val solNext = sol.next
        if(solNext.eq(sol)) { // pointer equality.
          instancesNext // stop loop.
        } else {
          loop(instancesNext, solNext)
        }
      } else {
        instances
      }
    }
    // correct all solutions as set of Example.
    loop(Set.empty, sol)
  }

  private def convertSol2Example(world:CompModule, sol:A4Solution):Example = {
    Example(world.getAllReachableUserDefinedSigs.map { sig: Sig =>
      val typeName = sig.toString
      sol.eval(sig).map { i : A4Tuple =>
        val instanceName : String = i.atom(0)
        val fields = sig.getFields.map { field: Sig.Field =>
          val fieldName = field.label
          val values : Set[List[Pointer]] =
            sol.eval(field).filter { t: A4Tuple =>
              t.atom(0) == instanceName
            }.map { t: A4Tuple =>
              (for (i <- 1 until t.arity()) yield ( StringPointer(t.atom(i)) )).toList
            }.toSet
          Field(fieldName, values)
        }.toSet
        SignatureInstance(typeName, instanceName, fields)
      }.toSet
    }.toSet.flatten)
  }
}
