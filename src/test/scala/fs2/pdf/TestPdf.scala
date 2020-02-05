package fs2
package pdf

import cats.Eval
import scodec.Attempt
import scodec.bits.BitVector

object TestPdf
{
  import Prim._

  def regularObjPre: String =
    s"""3 0 obj
    |<<
    |/Length 85
    |/Filter /FlateDecode
    |>>
    |stream
    |""".stripMargin

  def objectEndTags: String =
    s"""
    |endstream
    |endobj
    |""".stripMargin

  val streamDataRaw: String =
    "stream data"

  def streamDataBits: BitVector =
    BitVector(streamDataRaw.getBytes)

  def streamDataCompressed: BitVector =
    BitVector(streamDataRaw.getBytes).deflate()

  def regularObj: BitVector =
    BitVector(regularObjPre.getBytes) ++ streamDataCompressed ++ BitVector(objectEndTags.getBytes)

  def objDictRaw: String =
    """<</Annots[239 0 R 240 0 R]/MediaBox[0 0 595 841]/Resources<</Font<</T1_0 40 0 R>>>>/Type/Page>>"""

  def pageObjRaw: String =
    s"""197 5 obj
    |$objDictRaw
    |endobj
    |""".stripMargin

  def pageObjDict: Prim.Dict =
    Prim.dict(
      "Annots" -> Array(List(Ref(239, 0), Ref(240, 0))),
      "MediaBox" -> Array(List(Prim.num(0), Prim.num(0), Prim.num(595), Prim.num(841))),
      "Resources" -> Dict(Map("Font" -> Dict(Map("T1_0" -> Ref(40, 0))))),
      "Type" -> Name("Page"),
    )

  def pageObjData: Page =
    Page(Obj.Index(197, 5), pageObjDict)

  def pageObj: BitVector =
    BitVector(pageObjRaw.getBytes)

  def pagesObjDict: Prim.Dict =
    Prim.dict("Kids" -> Array(List(Ref(197, 5))))

  def pagesObjData: Page =
    Page(Obj.Index(198, 5), pagesObjDict)

  def pagesObjRaw: String =
    s"""198 0 obj
    |<</Type/Pages/Kids[197 5 R]>>
    |endobj
    |""".stripMargin

  def pagesObj: BitVector =
    BitVector(pagesObjRaw.getBytes)

  def pageObjUpdatedRaw: String =
    """197 5 obj
    |<</Resources <</Font <</T1_0 40 0 R /WmFont 40 0 R>>>> /Contents 1 0 R /MediaBox [0 0 595 841] /Type /Page /Annots [239 0 R 240 0 R]>>
    |endobj
    |""".stripMargin

  def pagesUpdateRaw: String =
    """198 0 obj
    |<</Type /Pages /Kids [197 5 R] /Resources <</Font <</WmFont 40 0 R>>>>>>
    |endobj
    |""".stripMargin

  def imageDictRaw: String =
    "<</Subtype/Image/Type/XObject>>"

  def imageObjString: String =
    s"""197 0 obj
    |$imageDictRaw
    |""".stripMargin

  def imageObjDict: Prim.Dict =
    Prim.dict(
      "Type" -> Name("XObject"),
      "Subtype" -> Name("Image"),
    )

  def imageObjData: Image =
    Image(
      Obj(Obj.Index(197, 0), imageObjDict),
      Image.Codec.Jpg,
      Parsed.Stream(streamDataBits, Eval.now(Attempt.successful(streamDataBits)))
    )

  def imageObj: BitVector =
    BitVector(imageObjString.getBytes) ++ streamDataCompressed ++ BitVector(objectEndTags.getBytes)

  def trailerRaw: String =
    """xref
    |0 2
    |0000000000 65535 f 
    |0000000015 00000 n 
    |trailer
    |<< /Size 123 >>
    |startxref
    |0
    |%%EOF
    |""".stripMargin

  def trailer: BitVector =
    BitVector(trailerRaw.getBytes)
}
