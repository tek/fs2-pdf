package fs2
package pdf

import codec.{Text, Whitespace}
import scodec.Codec
import scodec.codecs.liftF2ToNestedTupleF

case class Obj(index: Obj.Index, data: Prim)

object Obj
extends ObjCodec
{
  import Text.{ascii, str}

  case class Index(number: Long, generation: Int)

  object Index
  {
    def indexRaw: Index => String = {
      case Index(number, generation) =>
        s"$number $generation obj\n"
    }

    implicit def Codec_Index: Codec[Index] =
      ((ascii.long <~ str(" ")) ~ (ascii.int <~ str(" obj")))
        .xmap(Obj.Index(_, _), a => (a.number, a.generation))
  }

  def withDict[A]: Obj => A => Option[(A, Prim.Dict)] = {
    case Obj(_, data @ Prim.Dict(_)) => a => Some((a, data))
    case _ => _ => None
  }

  object tpe
  {
    def unapply(obj: Obj): Option[(String, Prim.Dict)] =
      Prim.tpe.unapply(obj.data)
  }

  object subtype
  {
    def unapply(obj: Obj): Option[(String, Prim.Dict)] =
      Prim.subtype.unapply(obj.data)
  }
}

private[pdf]
trait ObjCodec
{
  import Whitespace.{nlWs, skipWs, ws}
  import Text.str

  val codecPreStream: Codec[Obj] =
    ((skipWs ~> Codec[Obj.Index] <~ ws) :: Prim.Codec_Prim <~ ws)
      .as[Obj]

  implicit val Codec_Obj: Codec[Obj] =
    (codecPreStream <~ nlWs <~ str("endobj") <~ nlWs)
      .as[Obj]
}
