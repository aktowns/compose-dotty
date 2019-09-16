package classgen

import tinyfp.transformers.{State, StateT}
import given tinyfp.transformers._
import tinyfp.data.Identity
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

case class ClassCtx(constants: List[Constants]):
  def addClass(name: String): ClassCtx = 
    ???

type ClassBuilder[A] = State[ClassCtx, A]

def walk(x: Node): ClassBuilder[Node] = 
  x match
    case Node.Method(flags, name, args, ret, body) => ???
    case Node.Class(name, superClass, body)        => for {
      res <- StateT.modify[ClassCtx, Identity](_.addClass(name))
    } yield res
    case Node.Call(method, args)                   => ???