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
    val instances = extractor.allInstances
    val java = new JavaPrityPrinter(new JavaPrityPrinter.Config("java_tweet.conf", "tweet.java"))
    println(java.show(signatures, instances))
  }

}