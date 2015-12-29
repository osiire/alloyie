package com.falso.alloy.alloyie

import com.falso.alloy.alloyie.pp.JavaPrityPrinter

import scala.io.Source

/**
 *
 */
object Main {

  def main(args: Array[String]): Unit = {
    val param = AlloyInstanceExtractor.Parameter(Source.fromFile("sample.als").mkString, Option.empty)
    val extractor = new AlloyInstanceExtractor(param)
    val signatures = extractor.signatures
    val instances = extractor.instanceIterator
    for(i <- instances) {
      println(i.toString)
    }
    val java = new JavaPrityPrinter(new JavaPrityPrinter.Config("com.example", "tweet.java"))
    println(java.show(signatures, instances))
  }

}