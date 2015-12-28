package com.falso.alloy.alloyie.pp

import com.falso.alloy.alloyie.{Signature, Instance}

/**
 * ソースコード出力関数
 */
trait PrityPrinter {
  def show(signatures:Set[Signature], instances:Set[Instance]):String
  def show(signatures:Set[Signature], instances:Iterator[Instance]):String
}
