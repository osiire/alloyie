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
   *
   * @param model
   * @param commandLabel
   * @return
   */
  def iterator(model:String, commandLabel:Option[String]):Iterator[Example] = {
    val (world, sol) = resolve(model, commandLabel)
    new Iterator[Example]() {
      var currentSol = sol
      var first = true
      override def hasNext: Boolean = {
        if(first) {
          true
        } else {
          val solNext = currentSol.next
          solNext.ne(currentSol) && solNext.satisfiable()
        }
      }
      override def next(): Example = {
        if(first) {
          first = false;
        } else {
          currentSol = currentSol.next
        }
        val x = convertSol2Example(world, currentSol).resolveName
        println("--INSTANCE--")
        println(x.toString)
        x
      }
    }
  }

  /**
   * returns all instances of the specified model.
   * @param model
   * @param commandLabel
   * @return
   */
  def all(model:String, commandLabel:Option[String]):Set[Example] = {
    iterator(model, commandLabel).toSet[Example]
  }

  private def resolve(model:String, commandLabel:Option[String]):(CompModule, A4Solution) = {
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
    (world, TranslateAlloyToKodkod.execute_command(reporter, world.getAllSigs(), cmd, opt))
  }

  private def convertSol2Example(world:CompModule, sol:A4Solution):Example = {
    Example(world.getAllReachableUserDefinedSigs.map { sig: Sig =>
      val typeName = sig.toString
      sol.eval(sig).map { i: A4Tuple =>
        val instanceName: String = i.atom(0)
        val fields = sig.getFields.map { field: Sig.Field =>
          val fieldName = field.label
          val values: Set[List[Pointer]] =
            sol.eval(field).filter { t: A4Tuple =>
              t.atom(0) == instanceName
            }.map { t: A4Tuple =>
              (for (i <- 1 until t.arity()) yield (StringPointer(t.atom(i)))).toList
            }.toSet
          Field(fieldName, values)
        }.toSet
        SignatureInstance(typeName, instanceName, fields)
      }.toSet
    }.toSet.flatten)
  }
}
