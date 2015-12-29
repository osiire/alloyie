package com.falso.alloy.alloyie

import com.falso.alloy.alloyie.AlloyInstanceExtractor.Parameter
import edu.mit.csail.sdg.alloy4.{ConstList, ErrorAPI, A4Reporter}
import edu.mit.csail.sdg.alloy4compiler.ast.Type.ProductType
import edu.mit.csail.sdg.alloy4compiler.ast.{ExprUnary, Sig, Command}
import edu.mit.csail.sdg.alloy4compiler.parser.{CompModule, CompUtil}
import edu.mit.csail.sdg.alloy4compiler.translator._
import com.falso.alloy.alloyie.Tapper._

object AlloyInstanceExtractor {
  case class Parameter(
    model:String,
    commandLabel:Option[String]
  )
}

/**
 * alloy実行機
 */
class AlloyInstanceExtractor(param: Parameter) {
  import collection.JavaConversions._

  // initialize.
  val (world:CompModule, reporter:A4Reporter) = prepair(param.model)

  /**
   * get signatures in the specified model.
   * @return the set of com.falso.alloy/alloyie.Signature
   */
  def signatures:Set[Signature] = {
    world.getAllSigs.map { sig:Sig =>
      val fields:Set[Field] = sig.getFields.map { field:Sig.Field =>
        Field(field.label,
              op2mul(field.decl().expr.mult()),
              for {
                pt: ProductType <- field.`type`().toList
                types <- List.range(1, pt.arity).map(pt.get(_).label) // ignore 0. it's myself.
              } yield(types)
        )
      }.toSet
      Signature(sig.label, fields)
    }.toSet
  }

  /**
   *
   * @return
   */
  def instanceIterator:Iterator[Instance] = {
    val sol = resolve(param.commandLabel)
    new Iterator[Instance]() {
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
      override def next(): Instance = {
        if(first) {
          first = false
        } else {
          currentSol = currentSol.next
        }
        convertSol2Instance(currentSol).resolveName.tap { x =>
          println("--INSTANCE--")
          println(x.toString)
        }
      }
    }
  }

  /**
   * returns all instances of the specified model.
   * @return
   */
  def allInstances:Set[Instance] = {
    instanceIterator.toSet[Instance]
  }

  private def prepair(model:String):(CompModule, A4Reporter) = {
    val reporter:A4Reporter  = new A4Reporter()
    (CompUtil.parseEverything_fromString(reporter, model), reporter)
  }

  private def resolve(commandLabel:Option[String]):A4Solution = {
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
    TranslateAlloyToKodkod.execute_command(reporter, world.getAllSigs(), cmd, opt)
  }

  private def convertSol2Instance(sol:A4Solution):Instance = {
    Instance(world.getAllReachableUserDefinedSigs.flatMap { sig: Sig =>
      val typeName = sig.label
      sol.eval(sig).map { i: A4Tuple =>
        val instanceName: String = i.atom(0)  // signature tuples must be a single element.
        val fields = sig.getFields.map { field: Sig.Field =>
          val fieldName = field.label
          val mult : ExprUnary.Op = field.decl().expr.mult()
          val values: Set[List[Pointer]] =
            sol.eval(field).filter { t: A4Tuple =>
              t.atom(0) == instanceName // field tuple's first element must be signature instance.
            }.map { t: A4Tuple =>
              // there are multiple elements in the a4tuples if relation arity is up to 3.
              (for (i <- 1 until t.arity()) yield (StringPointer(t.sig(i).label, t.atom(i)))).toList
            }.toSet
          FieldInstance(fieldName, op2mul(mult), values)
        }.toSet
        SignatureInstance(typeName, instanceName, fields)
      }.toSet
    }.toSet)
  }

  private def op2mul(op: ExprUnary.Op):Mult = {
    op match {
      case ExprUnary.Op.LONEOF => Mult.Lone
      case ExprUnary.Op.ONEOF => Mult.One
      case ExprUnary.Op.SOMEOF => Mult.Some
      case ExprUnary.Op.SETOF => Mult.Set
      case _ => Mult.Set
    }
  }


}
