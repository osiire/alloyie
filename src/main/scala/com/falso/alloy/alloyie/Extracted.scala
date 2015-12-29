package com.falso.alloy.alloyie

sealed trait Pointer
case class StringPointer(typeName:String, name:String) extends Pointer {
  override def toString: String = {
    "p(%s:%s)".format(name,typeName)
  }
}
case class InstancePointer(typeName:String, instance:SignatureInstance) extends Pointer {
  override def toString: String = {
    "i(%s:%s)".format(instance.instanceName, typeName)
  }
}
case class IntegerPointer(num:Integer) extends Pointer {
  override def toString: String = {
    "i(%d:Int)".format(num)
  }
}

sealed abstract class Mult(val name:String)
object Mult {
  case object One extends Mult("one")
  case object Lone extends Mult("lone")
  case object Some extends Mult("some")
  case object Set extends Mult("set")
}


case class FieldInstance (name:String, mult:Mult, values:Set[List[Pointer]])
{
  override def toString : String = {
    "[%s,%s,%s]".format(name, mult.name, values.map(_.toString).mkString)
  }
}

case class SignatureInstance(
   typeName:String
   ,instanceName:String
   ,fields:Set[FieldInstance]
)
{
  override def toString : String = {
    "type=%s,instance=%s,fields=%s".format(typeName,instanceName, fields.map(_.toString).mkString)
  }
}

case class Field (name:String, mult:Mult, typeNames:List[String])
{
  override def toString : String = {
    "[%s,%s,%s]".format(name, mult.name, typeNames.mkString)
  }
}

case class Signature (
   typeName:String
   ,fields:Set[Field]
)
{
  override def toString: String = {
    "type=%s,fields=%s".format(typeName, fields.map(_.toString).mkString)
  }
}

class UnknownInstanceName(name:String)
  extends RuntimeException("could not resolve instance name (%s).".format(name))

/**
 *
 */
case class Instance (
  instances:Set[SignatureInstance]
)
{
   def resolveName : Instance = {
      Instance(instances.map { sig : SignatureInstance =>
        sig.copy( fields = sig.fields.map { f : FieldInstance =>
           f.copy( values = f.values.map { vv : List[Pointer] =>
             vv.map { p : Pointer =>
               p match {
                 case StringPointer(typeName, name) =>
                   instances.find(sig => sig.instanceName == name).map {
                     InstancePointer(typeName, _)
                   }.getOrElse({
                     try {
                       IntegerPointer(Integer.parseInt(name))
                     } catch {
                       case _:Exception => throw new UnknownInstanceName(name)
                     }
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
    instances.map(_.toString).mkString("\n")
  }

}
