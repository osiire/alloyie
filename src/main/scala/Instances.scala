package com.falso.alloy

sealed trait Pointer
case class StringPointer(name:String ) extends Pointer {
  override def toString: String = {
    "p(%s)".format(name)
  }
}
case class InstancePointer(instance:SignatureInstance) extends Pointer {
  override def toString: String = {
    "i(%s)".format(instance.instanceName)
  }
}

case class Field (name:String, values:Set[List[Pointer]])
{
  override def toString : String = {
    "[%s,%s]".format(name, values.map(_.toString).mkString)
  }
}

case class SignatureInstance(
   typeName:String
   ,instanceName:String
   ,fields:Set[Field]
)
{
  override def toString : String = {
    "type=%s,instance=%s,fields=%s".format(typeName,instanceName, fields.map(_.toString).mkString)
  }
}

class UnknownInstanceName(name:String)
  extends RuntimeException("could not resolve instance name (%s).".format(name))

/**
 *
 */
case class Example (signatures:Set[SignatureInstance]) {
   def resolveName : Example = {
      Example(signatures.map { sig : SignatureInstance =>
        sig.copy( fields = sig.fields.map { f : Field =>
           f.copy( values = f.values.map { vv : List[Pointer] =>
             vv.map { p : Pointer =>
               p match {
                 case StringPointer(name) =>
                   signatures.find(sig => sig.instanceName == name).map {
                     InstancePointer(_)
                   }.getOrElse({
                     throw new UnknownInstanceName(name)
                   })
                 case _ =>
                   p
               }
             }
           })
        })
      })
   }

  override def toString : String = {
    signatures.map(_.toString).mkString("\n")
  }

}

