/* Classgen.scala
 * Ashley Towns <mail@ashleytowns.id.au>
 *
 * This file serves as a DSL and a weak syntax tree for generating java class files.
 *
 * Aims for SE12 of the specification compatability
 *
 * This requires dotty nightly builds as of 2019-09
 * 
 * Theres a few helper typeclasses to help facilitate this, 
 * `SizeOf[A]` returns the size of a structure in bytes
 * `BinaryWriter[A]` serializes a scala type into bytes, also provides some machinery 
 *                   for validating and building up byte arrays dependent on `SizeOf`
 * `Bitwise[A]` provides bitwise operations for A, unlike scalas default bitwise operations
 *              these are closed over A, that is we don't return Int from say Short | Short,
 *              a larger reason for this typeclasses existence is the need to do bitwise operations
 *              on polymorphic values
 * `Default[A]` provides a default for A, used by the helper method `default[A]`
 * 
 * The datatype `Bitset` provides a way to easily apply bitsets on non-primitive types, 
 * `Bitset(A, B)` is equivalent to `A | B`
 * 
 * A basic example of generating a class file for HelloWorld with just a constructor:
 *
 * > val constants = List(/*1*/ utf8Str("HelloWorld"),
 * >                      /*2*/ Constants.ClassRef(1),
 * >                      /*3*/ utf8Str("<init>"),
 * >                      /*4*/ utf8Str("()V"),
 * >                      /*5*/ Constants.NameAndType(3, 4),
 * >                      /*6*/ utf8Str("java/lang/Object"),
 * >                      /*7*/ Constants.ClassRef(6),
 * >                      /*8*/ Constants.MethodRef(7, 5)
 * >                     )
 * >
 * > val methods = List(MethodInfo(accessFlags     = Bitset(),
 * >                               nameIndex       = 1,
 * >                               descriptorIndex = 4,
 * >                               attributesCount = 0,
 * >                               attributes      = List()
 * >                              )
 * >                   )
 * >
 * > ClassFormat(constantPool      = constants, 
 * >             constantPoolCount = (constants.length + 1).toShort,
 * >             accessFlags       = Bitset(ClassAccessFlag.Public),
 * >             thisClass         = 2,
 * >             superClass        = 0,
 * >             interfaceCount    = 0,
 * >             interfaces        = List(), 
 * >             fieldsCount       = 0,
 * >             fields            = List(),
 * >             methodsCount      = methods.length.toShort,
 * >             methods           = methods,
 * >             attributesCount   = 0, 
 * >             attributes        = List()
 * >            ).write("HelloWorld.class")
 */
package classgen

import java.nio.ByteBuffer
import java.io.{BufferedOutputStream, FileOutputStream}

enum Constants(val tag: Byte):
  case Utf8String(len: Short, value: List[Byte])   extends Constants(1)
  case IntRepr(value: Int)                         extends Constants(3)
  case FloatRepr(value: Float)                     extends Constants(4)
  case LongRepr(value: Long)                       extends Constants(5)
  case DoubleRepr(value: Double)                   extends Constants(6)
  case ClassRef(value: Short)                      extends Constants(7)
  case StringRef(value: Short)                     extends Constants(8)
  case FieldRef(klass: Short, nt: Short)           extends Constants(9)
  case MethodRef(klass: Short, nt: Short)          extends Constants(10)
  case InterfaceMethodRef(klass: Short, nt: Short) extends Constants(11)
  case NameAndType(name: Short, typ: Short)        extends Constants(12)

enum ClassAccessFlag(val value: Short):
  /** Declared public; may be accessed from outside its package.*/
  case Public           extends ClassAccessFlag(0x0001)
  /** Declared final; no subclasses allowed.*/
  case Final            extends ClassAccessFlag(0x0010)
  /** Treat superclass methods specially when invoked by the invokespecial instruction.*/
  case Super            extends ClassAccessFlag(0x0020)
  /** Is an interface, not a class.*/
  case Interface        extends ClassAccessFlag(0x0200)
  /** Declared abstract; must not be instantiated.*/
  case Abstract         extends ClassAccessFlag(0x0400)
  /** Declared synthetic; not present in the source code.*/
  case Synthetic        extends ClassAccessFlag(0x1000)
  /** Declared as an annotation type.*/
  case Annotation       extends ClassAccessFlag(0x2000)
  /** Declared as an enum type.*/
  case Enum             extends ClassAccessFlag(0x4000)
  /** Is a module, not a class or interface.*/
  //case Module           extends ClassAccessFlag(0x8000)
  case Custom(x: Short) extends ClassAccessFlag(x)

enum FieldAccessFlag(val value: Short):
  /** Declared public; may be accessed from outside its package.*/
  case Public           extends FieldAccessFlag(0x0001)
  /** Declared private; usable only within the defining class.*/
  case Private          extends FieldAccessFlag(0x0002)
  /** Declared protected; may be accessed within subclasses.*/
  case Protected        extends FieldAccessFlag(0x0004)
  /** Declared static.*/
  case Static           extends FieldAccessFlag(0x0008)
  /** Declared final; never directly assigned to after object construction.*/
  case Final            extends FieldAccessFlag(0x0010)
  /** Declared volatile; cannot be cached.*/
  case Volatile         extends FieldAccessFlag(0x0040)
  /** Declared transient; not written or read by a persistent object manager.*/
  case Transient        extends FieldAccessFlag(0x0080)
  /** Declared synthetic; not present in the source code.*/
  case Synthetic        extends FieldAccessFlag(0x1000)
  /** Declared as an element of an enum.*/
  case Enum             extends FieldAccessFlag(0x4000)
  case Custom(x: Short) extends FieldAccessFlag(x)

enum MethodAccessFlag(val value: Short):
  /** Declared public; may be accessed from outside its package.*/
  case Public           extends MethodAccessFlag(0x0001)
  /** Declared private; accessible only within the defining class.*/
  case Private          extends MethodAccessFlag(0x0002)
  /** Declared protected; may be accessed within subclasses.*/
  case Protected        extends MethodAccessFlag(0x0004)
  /** Declared static.*/
  case Static           extends MethodAccessFlag(0x0008)
  /** Declared final; must not be overridden.*/
  case Final            extends MethodAccessFlag(0x0010)
  /** Declared synchronized; invocation is wrapped by a monitor use.*/
  case Synchronised     extends MethodAccessFlag(0x0020)
  /** A bridge method, generated by the compiler.*/
  case Bridge           extends MethodAccessFlag(0x0040)
  /** Declared with variable number of arguments.*/
  case VarArgs          extends MethodAccessFlag(0x0080)
  /** Declared native; implemented in a language other than Java.*/
  case Native           extends MethodAccessFlag(0x0100)
  /** Declared abstract; no implementation is provided.*/
  case Abstract         extends MethodAccessFlag(0x0400)
  /** Declared strictfp; floating-point mode is FP-strict.*/
  case Strict           extends MethodAccessFlag(0x0800)
  /** Declared synthetic; not present in the source code.*/
  case Synthetic        extends MethodAccessFlag(0x1000)
  case Custom(x: Short) extends MethodAccessFlag(x)

case class Bitset[A](a: A*)

enum Instruction(val opcode: Byte):
  case ALoad0                               extends Instruction(0x2a.toByte)
  case InvokeSpecial(id1: Byte, id2: Byte)  extends Instruction(0xb7.toByte)
  case GetStatic(id1: Byte, id2: Byte)      extends Instruction(0xb2.toByte)
  case LDC(id: Byte)                        extends Instruction(0x12.toByte)
  case InvokeVirtual(id1: Byte, id2: Byte)  extends Instruction(0xb6.toByte)
  case Return                               extends Instruction(0xb1.toByte)

case class ExceptionTable(startPc: Short, endPc: Short, handlerPc: Short, catchType: Short)

enum AttributeInfo:
  case ConstantValue(nameIndex: Short, length: Int, constantValueIndex: Short)
  case Code(attributeNameIndex: Short, 
            attributeLength: Int,
            maxStack: Short,
            maxLocals: Short,
            codeLength: Int,
            code: List[Instruction],
            exceptionTableLength: Short,
            exceptionTable: List[ExceptionTable],
            attributesCount: Short, 
            attributes: List[AttributeInfo]
           )

case class FieldInfo(accessFlags: Bitset[FieldAccessFlag],
                     nameIndex: Short,
                     descriptorIndex: Short,
                     attributesCount: Short, 
                     attributes: List[AttributeInfo])

case class MethodInfo(accessFlags: Bitset[MethodAccessFlag],
                      nameIndex: Short,
                      descriptorIndex: Short,
                      attributesCount: Short, 
                      attributes: List[AttributeInfo])

enum MajorVersion(val version: Short):
  case JavaSE14 extends MajorVersion(0x3A)
  case JavaSE13 extends MajorVersion(0x39)
  case JavaSE12 extends MajorVersion(0x38)
  case JavaSE11 extends MajorVersion(0x37)
  case JavaSE10 extends MajorVersion(0x36)
  case JavaSE9  extends MajorVersion(0x35)
  case JavaSE8  extends MajorVersion(0x34)

case class ClassMagic(magic: Int = 0xCAFEBABE)

case class ClassFormat(
  /** The magic item supplies the magic number identifying the class file format */
  magicNumber: ClassMagic = ClassMagic(),
  /** Together, a major and a minor version number determine the version of the class file format.*/
  minorVersion: Short = 0x0000,
  /** Together, a major and a minor version number determine the version of the class file format.*/
  majorVersion: MajorVersion = MajorVersion.JavaSE8, 
  /** The value of the constant_pool_count item is equal to the number of entries in the constant_pool table plus one.*/
  constantPoolCount: Short,
  constantPool: List[Constants],
  accessFlags: Bitset[ClassAccessFlag],
  thisClass: Short,
  superClass: Short,
  interfaceCount: Short,
  interfaces: List[Short], 
  fieldsCount: Short,
  fields: List[FieldInfo],
  methodsCount: Short, 
  methods: List[MethodInfo],
  attributesCount: Short, 
  attributes: List[AttributeInfo]
)

trait Default[A]:
  def instance: A

given as Default[ClassAccessFlag]:
  def instance: ClassAccessFlag = ClassAccessFlag.Custom(0.toShort)

given as Default[MethodAccessFlag]:
  def instance: MethodAccessFlag = MethodAccessFlag.Custom(0.toShort)

def default[A] given (default: Default[A]): A = default.instance

// sigh
trait Bitwise[A]:
  def (x: A) >| (y: A): A
  def (x: A) >& (y: A): A

given bitwiseShort as Bitwise[Short]:
  def (x: Short) >| (y: Short) = (x | y).toShort
  def (x: Short) >& (y: Short) = (x & y).toShort

given as Bitwise[ClassAccessFlag]:
  def (x: ClassAccessFlag) >| (y: ClassAccessFlag): ClassAccessFlag = 
    ClassAccessFlag.Custom(x.value >| y.value)
  def (x: ClassAccessFlag) >& (y: ClassAccessFlag): ClassAccessFlag = 
    ClassAccessFlag.Custom(x.value >& y.value)

given as Bitwise[MethodAccessFlag]:
  def (x: MethodAccessFlag) >| (y: MethodAccessFlag): MethodAccessFlag = 
    MethodAccessFlag.Custom(x.value >| y.value)
  def (x: MethodAccessFlag) >& (y: MethodAccessFlag): MethodAccessFlag = 
    MethodAccessFlag.Custom(x.value >& y.value)

trait SizeOf[A]:
  def (x: A) sizeOf: Int

given as SizeOf[Short]: 
  def (x: Short) sizeOf: Int = 2

given as SizeOf[Int]: 
  def (x: Int) sizeOf: Int = 4

given as SizeOf[ClassMagic]:
  def (x: ClassMagic) sizeOf: Int = x.magic.sizeOf

given as SizeOf[MajorVersion]:
  def (x: MajorVersion) sizeOf: Int = x.version.sizeOf

given as SizeOf[Constants]:
  def (x: Constants) sizeOf: Int = 
    val reprSize = x match
      case Constants.Utf8String(_, x)         => x.length + 2
      case Constants.IntRepr(_)               => 4
      case Constants.FloatRepr(_)             => 4
      case Constants.LongRepr(_)              => 8
      case Constants.DoubleRepr(_)            => 8
      case Constants.ClassRef(_)              => 2
      case Constants.StringRef(_)             => 2
      case Constants.FieldRef(_, _)           => 4
      case Constants.MethodRef(_, _)          => 4
      case Constants.InterfaceMethodRef(_, _) => 4
      case Constants.NameAndType(_, _)        => 4
    reprSize + 1

given as SizeOf[Instruction]:
  def (x: Instruction) sizeOf: Int =
    val sz = x match
      case Instruction.ALoad0           => 0
      case Instruction.Return           => 0
      case _: Instruction.LDC           => 1
      case _: Instruction.InvokeSpecial => 2
      case _: Instruction.InvokeVirtual => 2
      case _: Instruction.GetStatic     => 2
    sz + 1

given as SizeOf[ExceptionTable]:
  def (x: ExceptionTable) sizeOf: Int =
    x.startPc.sizeOf   + 
    x.endPc.sizeOf     +
    x.handlerPc.sizeOf + 
    x.catchType.sizeOf

given as SizeOf[FieldAccessFlag]:
  def (x: FieldAccessFlag) sizeOf: Int = x.value.sizeOf

given as SizeOf[MethodAccessFlag]:
  def (x: MethodAccessFlag) sizeOf: Int = x.value.sizeOf

given as SizeOf[ClassAccessFlag]:
  def (x: ClassAccessFlag) sizeOf: Int = x.value.sizeOf

given bitSetSizeOf[A: SizeOf: Default] as SizeOf[Bitset[A]]:
  def (x: Bitset[A]) sizeOf: Int = default[A].sizeOf

given listOfSizeOf[A: SizeOf] as SizeOf[List[A]]:
  def (x: List[A]) sizeOf: Int = x.foldLeft(0)((sz, a) => sz + a.sizeOf)

given fieldInfoSizeOf as SizeOf[FieldInfo]:
  def (x: FieldInfo) sizeOf: Int = 0

given attributeInfoSizeOf as SizeOf[AttributeInfo]:
  def (x: AttributeInfo) sizeOf: Int =
    x match
      case k: AttributeInfo.Code =>
        k.attributeNameIndex.sizeOf   +
        k.attributeLength.sizeOf      +
        k.maxStack.sizeOf             +
        k.maxLocals.sizeOf            +
        k.codeLength.sizeOf           +
        k.code.sizeOf                 +
        k.exceptionTableLength.sizeOf +
        k.exceptionTable.sizeOf       +
        k.attributesCount.sizeOf      +
        k.attributes.sizeOf

given methodInfoSizeOf as SizeOf[MethodInfo]:
  def (x: MethodInfo) sizeOf: Int = 
    x.accessFlags.sizeOf     +
    x.nameIndex.sizeOf       +
    x.descriptorIndex.sizeOf +
    x.attributesCount.sizeOf +
    x.attributes.sizeOf

given as SizeOf[ClassFormat]:
  def (x: ClassFormat) sizeOf: Int =
    x.magicNumber.sizeOf       +
    x.minorVersion.sizeOf      +
    x.majorVersion.sizeOf      +
    x.constantPoolCount.sizeOf +
    x.constantPool.sizeOf      + 
    x.accessFlags.sizeOf       +
    x.thisClass.sizeOf         +
    x.superClass.sizeOf        +
    x.interfaceCount.sizeOf    +
    x.interfaces.sizeOf        +
    x.fieldsCount.sizeOf       +
    x.fields.sizeOf            +
    x.methodsCount.sizeOf      +
    x.methods.sizeOf           +
    x.attributesCount.sizeOf   +
    x.attributes.sizeOf

trait BinaryWriter[A: SizeOf]:
  protected def build(k: A)(f: ByteBuffer => ByteBuffer): Array[Byte] = 
    val res = f(ByteBuffer.allocate(k.sizeOf)).array
    assert(res.length == k.sizeOf)
    res

  def (x: A) write (file: String): Unit =  
    val os = new BufferedOutputStream(new FileOutputStream(file))
    os.write(x.decode)
    os.close

  def (x: A) decode: Array[Byte]

given as BinaryWriter[ClassMagic]: 
  def (k: ClassMagic) decode: Array[Byte] = 
    build(k)(_.putInt(k.magic))

given as BinaryWriter[MajorVersion]:
  def (k: MajorVersion) decode: Array[Byte] = 
    build(k)(_.putShort(k.version))

given as BinaryWriter[Constants]:
  def (k: Constants) decode: Array[Byte] =
    build(k) { bfr =>
      val t = bfr.put(k.tag) 
      k match
        case Constants.Utf8String(l, c)          => c.foldLeft(t.putShort(l))((b, a) => b.put(a))
        case Constants.IntRepr(i)                => t.putInt(i)
        case Constants.FloatRepr(f)              => t.putFloat(f)
        case Constants.LongRepr(l)               => t.putLong(l)
        case Constants.DoubleRepr(d)             => t.putDouble(d)
        case Constants.ClassRef(s)               => t.putShort(s)
        case Constants.StringRef(s)              => t.putShort(s)
        case Constants.FieldRef(k, nt)           => t.putShort(k).putShort(nt)
        case Constants.MethodRef(k, nt)          => t.putShort(k).putShort(nt)
        case Constants.InterfaceMethodRef(k, nt) => t.putShort(k).putShort(nt)
        case Constants.NameAndType(n, ty)        => t.putShort(n).putShort(ty)
    }

given as BinaryWriter[ClassAccessFlag]:
  def (af: ClassAccessFlag) decode: Array[Byte] = 
    build(af)(bfr => bfr.putShort(af.value))

given as BinaryWriter[MethodAccessFlag]:
  def (af: MethodAccessFlag) decode: Array[Byte] = 
    build(af)(bfr => bfr.putShort(af.value))

given bitsetWriter[A: BinaryWriter: SizeOf: Bitwise: Default] as BinaryWriter[Bitset[A]]:
  def (bs: Bitset[A]) decode: Array[Byte] =
    bs.a.foldLeft(default[A])((acc, a) => acc >| a).decode

given bitsetShortWriter as BinaryWriter[Short]:
  def (k: Short) decode: Array[Byte] = build(k)(_.putShort(k))

given listConstantWriter as BinaryWriter[List[Constants]]:
  def (k: List[Constants]) decode: Array[Byte] =
    build(k)(bfr => k.foldLeft(bfr)((b, a) => b.put(a.decode)))

given listShortWriter as BinaryWriter[List[Short]]:
  def (k: List[Short]) decode: Array[Byte] = 
    build(k)(bfr => k.foldLeft(bfr)((b, a) => b.putShort(a)))

given listFieldWriter as BinaryWriter[List[FieldInfo]]:
  def (k: List[FieldInfo]) decode: Array[Byte] = Array()

given as BinaryWriter[ExceptionTable]:
  def (k: ExceptionTable) decode: Array[Byte] = 
    build(k)(_.putShort(k.startPc)
              .putShort(k.endPc)
              .putShort(k.handlerPc)
              .putShort(k.catchType)
            )

given as BinaryWriter[Instruction]:
  def (k: Instruction) decode: Array[Byte] = 
    build(k) { bfr => 
      val op = bfr.put(k.opcode)
      k match
        case Instruction.ALoad0                  => op
        case Instruction.Return                  => op
        case Instruction.InvokeSpecial(id1, id2) => op.put(id1).put(id2)
        case Instruction.InvokeVirtual(id1, id2) => op.put(id1).put(id2)
        case Instruction.LDC(id1)                => op.put(id1) 
        case Instruction.GetStatic(id1, id2)     => op.put(id1).put(id2)
    }

given as BinaryWriter[AttributeInfo]:
  def (k: AttributeInfo) decode: Array[Byte] = 
    k match
      case x: AttributeInfo.Code => 
        build(k)(_.putShort(x.attributeNameIndex)
                  .putInt(x.attributeLength)
                  .putShort(x.maxStack)
                  .putShort(x.maxLocals)
                  .putInt(x.codeLength)
                  .put(x.code.decode)
                  .putShort(x.exceptionTableLength)
                  .put(x.exceptionTable.decode)
                  .putShort(x.attributesCount)
                  .put(x.attributes.decode)
                )

given as BinaryWriter[MethodInfo]:
  def (k: MethodInfo) decode: Array[Byte] = 
    build(k)(_.put(k.accessFlags.decode)
              .putShort(k.nameIndex)
              .putShort(k.descriptorIndex)
              .putShort(k.attributesCount)
              .put(k.attributes.decode)
            )

given listBinaryWriterList[A: BinaryWriter: SizeOf] as BinaryWriter[List[A]]:
  def (a: List[A]) decode: Array[Byte] = 
    build(a)(bfr => a.foldLeft(bfr)((acc, a) => acc.put(a.decode)))

given as BinaryWriter[ClassFormat]:
  def (k: ClassFormat) decode: Array[Byte] = 
    build(k)(_.put(k.magicNumber.decode)
              .putShort(k.minorVersion)
              .put(k.majorVersion.decode)
              .putShort(k.constantPoolCount)
              .put(k.constantPool.decode)
              .put(k.accessFlags.decode)
              .putShort(k.thisClass)
              .putShort(k.superClass)
              .putShort(k.interfaceCount)
              .put(k.interfaces.decode)
              .putShort(k.fieldsCount)
              .put(k.fields.decode) 
              .putShort(k.methodsCount)
              .put(k.methods.decode)
             )

/* Just some helpers for recalcing the sizes */
def recalculateMethodInfo(x: MethodInfo): MethodInfo =
  println(s"attributesCount: ${x.attributesCount} ~ ${x.attributes.length}")
  x.copy(attributesCount = x.attributes.length.toShort,
         attributes = x.attributes.map(recalculateAttributeInfoSizes)
        )

def recalculateAttributeInfoSizes(x: AttributeInfo): AttributeInfo =
  x match
    case y: AttributeInfo.Code => 
      println(s"attributeLength: ${y.attributeLength} ~ ${y.sizeOf - 6}")
      println(s"codeLength: ${y.codeLength} ~ ${y.code.sizeOf}")
      y.copy(attributeLength = y.sizeOf - 6 /* proceeding header doesn't count */,
             codeLength = y.code.sizeOf
            )

def recalculateClassFormatSizes(x: ClassFormat): ClassFormat = 
  println(s"interfaceCount: ${x.interfaceCount} ~ ${x.interfaces.length}")
  println(s"methodsCount: ${x.methodsCount} ~ ${x.methods.length}")
  println(s"fieldsCount: ${x.fieldsCount} ~ ${x.fields.length}")
  println(s"attributesCount: ${x.attributesCount} ~ ${x.attributes.length}")
  x.copy(interfaceCount = x.interfaces.length.toShort,
         methodsCount = x.methods.length.toShort,
         methods = x.methods.map(recalculateMethodInfo),
         fieldsCount = x.fields.length.toShort,
         attributesCount = x.attributes.length.toShort
        )

def utf8Str(str: String): Constants =
  Constants.Utf8String(str.length.toShort, str.toCharArray.toList.map(_.toByte))


// Write out a hello world
def helloworld: ClassFormat =
  val constants = List(
/*1 */utf8Str("HelloWorld"),
/*2 */Constants.ClassRef(1),
/*3 */utf8Str("<init>"),
/*4 */utf8Str("()V"),
/*5 */Constants.NameAndType(3, 4),
/*6 */utf8Str("java/lang/Object"),
/*7 */Constants.ClassRef(6),
/*8 */Constants.MethodRef(7, 5),
/*9 */utf8Str("Code"),
/*10*/utf8Str("main"),
/*11*/utf8Str("([Ljava/lang/String;)V"),
/*12*/utf8Str("java/lang/System"),
/*13*/utf8Str("out"),
/*14*/utf8Str("Ljava/io/PrintStream;"),
/*15*/utf8Str("java/io/PrintStream"),
/*16*/utf8Str("println"),
/*17*/utf8Str("(Ljava/lang/String;)V"),
/*18*/Constants.NameAndType(13, 14),
/*19*/Constants.NameAndType(16, 17),
/*20*/Constants.ClassRef(15),
/*21*/Constants.ClassRef(12),
/*22*/Constants.FieldRef(21, 18),
/*23*/Constants.MethodRef(20, 19),
/*24*/Constants.StringRef(1)
  )
  val methods = List(
    MethodInfo(accessFlags = Bitset(),
               nameIndex = 3,
               descriptorIndex = 4,
               attributesCount = 1,
               attributes = List(
                 AttributeInfo.Code(attributeNameIndex = 9,
                                    attributeLength = 17,
                                    maxStack = 1,
                                    maxLocals = 1,
                                    codeLength = 5,
                                    code = List(
                                      Instruction.ALoad0,
                                      Instruction.InvokeSpecial(0x00, 0x08),
                                      Instruction.Return
                                    ),
                                    exceptionTableLength = 0,
                                    exceptionTable = List(),
                                    attributesCount = 0,
                                    attributes = List()
                                   )
                 )
              ),
    MethodInfo(accessFlags = Bitset(MethodAccessFlag.Public, MethodAccessFlag.Static),
               nameIndex = 10,
               descriptorIndex = 11,
               attributesCount = 1,
               attributes = List(
                 AttributeInfo.Code(attributeNameIndex = 9,
                                    attributeLength = 21,
                                    maxStack = 2,
                                    maxLocals = 1,
                                    codeLength = 9,
                                    code = List(
                                      Instruction.GetStatic(0x00, 22),
                                      Instruction.LDC(24),
                                      Instruction.InvokeVirtual(0x00, 23),
                                      Instruction.Return
                                    ),
                                    exceptionTableLength = 0,
                                    exceptionTable = List(),
                                    attributesCount = 0,
                                    attributes = List()
                                   )
               )
              )
  )

  val cf = ClassFormat(constantPool = constants, 
              constantPoolCount = (constants.length + 1).toShort,
              accessFlags = Bitset(ClassAccessFlag.Public),
              thisClass = 2,
              superClass = 7,
              interfaceCount = 0,
              interfaces = List(), 
              fieldsCount = 0,
              fields = List(),
              methodsCount = methods.length.toShort,
              methods = methods,
              attributesCount = 0, 
              attributes = List()
             )
  recalculateClassFormatSizes(cf)