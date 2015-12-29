package com.falso.alloy.alloyie.pp

import javax.tools.FileObject

import com.falso.alloy.alloyie.{Signature, Instance}
import com.falso.alloy.alloyie.Tapper._
import com.sun.codemodel._
import java.io._

import com.sun.codemodel.writer.SingleStreamCodeWriter

class InconsistentTypeSignature(name:String)
  extends RuntimeException("could not find class name:%s. this is a bug.".format(name))

object JavaPrityPrinter {
  case class Config (
    packageName:String
    , outputFile:String
  )
}

/**
 * Instance printer for java.
 */
class JavaPrityPrinter(config:JavaPrityPrinter.Config) extends PrityPrinter {


  override def show(signatures:Set[Signature], instances:Set[Instance]):String = {
    ???
  }

  override def show(signatures:Set[Signature], instances:Iterator[Instance]):String = {
    val cm = new JCodeModel()
    val out = new ByteArrayOutputStream()
    val writer = new SingleStreamCodeWriter(out)

    val classes = signatures.map { sig =>
      cm._class("%s.%s".format(config.packageName, typeName2jclassName(sig.typeName))).tap {
        //コンストラクタはpublicで
        _.constructor(JMod.PUBLIC)
      }
    }

    for(sig <- signatures) {
      val clazz:JDefinedClass =
        classes.find( _.name() == typeName2jclassName(sig.typeName)).getOrElse {
          throw new InconsistentTypeSignature(sig.typeName)
        }
      for(field <- sig.fields) {
        val t:JType = resolveType(cm, classes, field.typeNames)
        //インスタンス変数
        val name: JFieldVar = clazz.field(JMod.PUBLIC, t, field.name);
      }
    }
    //ソース生成
    cm.build(writer)
    out.toString
  }

  val removeThis = "this/".r

  private def typeName2jclassName(name:String):String = {
    removeThis.replaceAllIn(name, "")
  }

  private def resolveType(codeModel:JCodeModel, classes:Set[JDefinedClass], names:List[String]):JType = {
    names match {
      case Nil =>
        ???
      case hd :: _ =>
        if(hd == "Int") {
          codeModel.INT
        } else {
          classes.find( _.name() == typeName2jclassName(hd)).getOrElse {
            throw new InconsistentTypeSignature(hd)
          }
        }
    }
  }

}
