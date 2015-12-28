package com.falso.alloy.alloyie

object Tapper {
  implicit def anyToTapper[A](obj: A) = new Tapper(obj)
}

class Tapper[A](obj: A) {
  def tap(code: A => Unit): A = {
    code(obj)
    obj
  }
}