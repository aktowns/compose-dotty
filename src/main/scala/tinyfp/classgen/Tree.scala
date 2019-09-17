package classgen

import tinyfp.transformers.{State, StateT}
import tinyfp.data.Identity
import tinyfp.typeclass._

import given tinyfp.transformers._
import given tinyfp.data.IdentityMonad
import given tinyfp.instance._

enum Type:
  case Concrete(id: String)
  case Array(inner: Type)
  case String
  case Void

enum MethodFlags:
  case Public
  case Static
  
enum Node:
  case Method(flags: Flags, name: String, args: List[Type], returnType: Type, body: List[Node])
  case Class(name: String, superClass: Type, body: List[Node])
  case Call(method: String, args: String)

case class Flags(flags: MethodFlags*)

def test = 
  Node.Class(name = "HelloWorld", superClass = Type.Concrete("Object"),
    body = List(
      Node.Method(Flags(MethodFlags.Public, MethodFlags.Static), "main", 
        List(Type.Array(Type.String)), Type.Void, 
          body = List(Node.Call("system.out.println", "HelloWorld"))
      )
    )
  )

case class ClassCtx(constants: List[Constants], methods: List[MethodInfo]):
  def string(x: String): Int = 
    constants.filter { case y: Constants.Utf8String if y.value => true }
             
    constants.find { (c: Constants) => 
      
    }
    ???
    //constants.find(_.tag )
  def addClass(name: String): ClassCtx = 
    ???

  def addMethod(name: String): ClassCtx =
    ???

type ClassBuilder[A] = StateT[ClassCtx, Identity, A]
given [A] as Monad[ClassBuilder] = StateTMonad[ClassCtx, Identity]

def walk(x: Node): ClassBuilder[Node] =
  x match
    case Node.Method(flags, name, args, ret, body) => ???
    case Node.Class(name, superClass, body)        =>
      StateT.modify[ClassCtx, Identity](_.addClass(name)) #> x
    case Node.Call(method, args)                   => ???
