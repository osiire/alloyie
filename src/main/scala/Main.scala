package com.falso.alloy

import scala.io.Source

/**
 *
 */
object Main {

  def main(args: Array[String]): Unit = {
    val instances = (new executor.AlloyExecutor).run(Source.fromFile("sample.als").mkString, Option.empty)
    println(instances.size)
  }

}