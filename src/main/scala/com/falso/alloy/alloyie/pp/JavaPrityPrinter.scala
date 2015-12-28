package com.falso.alloy.alloyie.pp

import javax.tools.FileObject

import com.falso.alloy.alloyie.{Signature, Instance}
import com.sun.codemodel._
import java.io._

import com.sun.codemodel.writer.SingleStreamCodeWriter

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
    //生成するクラス定義
    val clazz:JDefinedClass = cm._class(config.packageName);
    clazz.constructor(JMod.PUBLIC);  //コンストラクタはpublicで
    //privateなインスタンス変数
    val name:JFieldVar = clazz.field(JMod.PUBLIC, classOf[String], "name");
    //ソース生成
    cm.build(writer)
    out.toString
  }
}
