package classgen

import java.nio.ByteBuffer
import java.io.{BufferedOutputStream, FileOutputStream}

enum Constants(val tag: Byte):
  case Utf8String(value: List[Byte]) extends Constants(1)
  case IntRepr(value: Int)           extends Constants(3)
  case FloatRepr(value: Float)       extends Constants(4)
  case LongRepr(value: Long)         extends Constants(5)
  case DoubleRepr(value: Double)     extends Constants(6)
  case ClassRef(value: Short)        extends Constants(7)
  case StringRef(value: Short)       extends Constants(8)
  case FieldRef(value: Int)          extends Constants(9)
  case MethodRef(value: Int)         extends Constants(10)

enum AccessFlags(val value: Short):
  case Public    extends AccessFlags(0x0001)
  case Private   extends AccessFlags(0x0002)
  case Protected extends AccessFlags(0x0004)
  case Static    extends AccessFlags(0x0008)
  case Final     extends AccessFlags(0x0010)
  case Volatile  extends AccessFlags(0x0040)
  case Transient extends AccessFlags(0x0080)
  case Synthetic extends AccessFlags(0x1000)
  case Enum      extends AccessFlags(0x4000)

enum AttributeInfo:
  case ConstantValue(nameIndex: Short, length: Int, constantValueIndex: Short)

case class FieldInfo(accessFlags: Short,
                     nameIndex: Short,
                     descriptorIndex: Short,
                     attributesCount: Short, 
                     attributes: List[AttributeInfo])

case class MethodInfo(accessFlags: Short,
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

case class ClassFormat(magicNumber: ClassMagic = ClassMagic(),
                       minorVersion: Short = 0x0000,
                       majorVersion: MajorVersion = MajorVersion.JavaSE8, 
                       constantPoolCount: Short,
                       constantPool: List[Constants],
                       accessFlags: Short,
                       thisClass: Short,
                       superClass: Short,
                       interfaceCount: Short,
                       interfaces: List[Short], 
                       fieldsCount: Short,
                       fields: List[FieldInfo],
                       methodsCount: Short, 
                       methods: List[MethodInfo],
                       attributesCount: Short, 
                       attributes: List[AttributeInfo])

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
      case Constants.Utf8String(x) => x.length + 2
      case Constants.IntRepr(_)    => 4
      case Constants.FloatRepr(_)  => 4
      case Constants.LongRepr(_)   => 8
      case Constants.DoubleRepr(_) => 8
      case Constants.ClassRef(_)   => 2
      case Constants.StringRef(_)  => 2
      case Constants.FieldRef(_)   => 4
      case Constants.MethodRef(_)  => 4
    reprSize + 1

//given as SizeOf[List[Constants]]:
//  def (x: List[Constants]) sizeOf: Int = x.foldLeft(0)((sz, a) => sz + a.sizeOf)

given listOfSizeOf[A: SizeOf] as SizeOf[List[A]]:
  def (x: List[A]) sizeOf: Int = x.foldLeft(0)((sz, a) => sz + a.sizeOf)

given fieldInfoSizeOf as SizeOf[FieldInfo]:
  def (x: FieldInfo) sizeOf: Int = ???

given as SizeOf[ClassFormat]:
  def (x: ClassFormat) sizeOf: Int =
    x.magicNumber.sizeOf +
    x.minorVersion.sizeOf +
    x.majorVersion.sizeOf +
    x.constantPoolCount.sizeOf +
    x.constantPool.sizeOf

trait BinaryWriter[A: SizeOf]:
  protected def build(k: A)(f: ByteBuffer => ByteBuffer): Array[Byte] = 
    f(ByteBuffer.allocate(k.sizeOf)).array

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
        case Constants.Utf8String(c) => c.foldLeft(t.putShort(c.length.toShort))((b, a) => b.put(a))
        case Constants.IntRepr(i)    => t.putInt(i)
        case Constants.FloatRepr(f)  => t.putFloat(f)
        case Constants.LongRepr(l)   => t.putLong(l)
        case Constants.DoubleRepr(d) => t.putDouble(d)
        case Constants.ClassRef(s)   => t.putShort(s)
        case Constants.StringRef(s)  => t.putShort(s)
        case Constants.FieldRef(i)   => t.putInt(i)
        case Constants.MethodRef(i)  => t.putInt(i)
    }

given listConstantWriter as BinaryWriter[List[Constants]]:
  def (k: List[Constants]) decode: Array[Byte] =
    build(k)(bfr => k.foldLeft(bfr)((b, a) => b.put(a.decode)))

given listShortWriter as BinaryWriter[List[Short]]:
  def (k: List[Short]) decode: Array[Byte] = 
    build(k)(bfr => k.foldLeft(bfr)((b, a) => b.putShort(a)))

given listFieldWriter as BinaryWriter[List[FieldInfo]]:
  def (k: List[FieldInfo]) decode: Array[Byte] = ???

given as BinaryWriter[ClassFormat]:
  def (k: ClassFormat) decode: Array[Byte] = 
    build(k)(_.put(k.magicNumber.decode)
              .putShort(k.minorVersion)
              .put(k.majorVersion.decode)
              .putShort(k.constantPoolCount)
              .put(k.constantPool.decode)
              .putShort(k.accessFlags)
              .putShort(k.thisClass)
              .putShort(k.superClass)
              .putShort(k.interfaceCount)
              .put(k.interfaces.decode)
              .putShort(k.fieldsCount)
              .put(k.fields.decode) 
             )
    