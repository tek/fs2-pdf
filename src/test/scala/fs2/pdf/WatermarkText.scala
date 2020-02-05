package fs2
package pdf

import fs2.pdf.{FontResource, IndirectObj, Obj, Page, PageDir, Prim}
import scodec.{Attempt, Err}
import scodec.bits.BitVector

object WatermarkText
{
  val fontName: String =
    "WmFont"

  def fonts(fontNumber: Long): Prim.Dict =
    Prim.dict("Font" -> Prim.dict(fontName -> Prim.refNum(fontNumber)))

  def fontResources(fontNumber: Long): Prim.Dict =
    Prim.dict("Resources" -> fonts(fontNumber))

  def addFontIfDirect(fontNumber: Long)(data: Prim.Dict): Prim.Dict =
    data("Fonts")
      .flatMap {
        case Prim.Array(refs) =>
          Some(Prim.Dict(data.data.updated("Contents", Prim.Array(Prim.refNum(fontNumber) :: refs))))
        case _ => None
      }
      .getOrElse(data)

  def addFontIfDirectResources(fontNumber: Long)(data: Prim.Dict): Prim.Dict =
    data("Resources")
      .flatMap {
        case Prim.Dict(_) =>
          Prim.Dict.deepMerge(fontResources(fontNumber))(data).toOption
        case _ => None
      }
      .getOrElse(data)

  def addContentIfDirect(streams: Set[Long])(content: Prim)(data: Prim.Dict): Prim.Dict =
    data("Contents")
      .flatMap {
        case Prim.Array(refs) =>
          Some(Prim.Dict(data.data.updated("Contents", Prim.Array(content :: refs))))
        case old @ Prim.Ref(number, _) if streams.contains(number) =>
          Some(Prim.Dict(data.data.updated("Contents", Prim.Array(List(content, old)))))
        case _ =>
          None
      }
      .getOrElse(data)

  def pageObject(streams: Set[Long])(contentNumber: Long, fontNumber: Long): Page => IndirectObj = {
    case Page(Obj.Index(number, generation), data) =>
      val updated = addContentIfDirect(streams)(Prim.refNum(contentNumber))(addFontIfDirectResources(fontNumber)(data))
      IndirectObj.nostream(number, generation, updated)
  }

  def rootDir(fontNumber: Long): PageDir => Attempt[IndirectObj] = {
    case PageDir(Obj.Index(number, generation), data) =>
      Prim.Dict.deepMerge(fontResources(fontNumber))(data)
        .map(IndirectObj.nostream(number, generation, _))
  }

  val fontSize: Int =
    10

  val fontCommand: String =
    s"/$fontName $fontSize Tf"

  def font(number: Long): IndirectObj =
    IndirectObj.nostream(number, 0, Prim.dict(
      "Type" -> Prim.Name("Font"),
      "Subtype" -> Prim.Name("Type1"),
      "Name" -> Prim.Name(fontName),
      "BaseFont" -> Prim.Name("Helvetica"),
      "Encoding" -> Prim.Name("MacRomanEncoding"),
    ))

  def labelRadius(label: String): Double =
    label.length * fontSize * 0.2

  def watermarkXPosition(label: String, pageCenter: BigDecimal): BigDecimal =
    pageCenter - labelRadius(label)

  def watermarkContentStream(label: String, pageCenter: BigDecimal): String =
    s"""q
    |BT
    |0.5 G
    |0.5 g
    |$fontCommand
    |1 0 0 1 ${watermarkXPosition(label, pageCenter)} 10 Tm
    |($label) Tj
    |ET
    |Q""".stripMargin

  def watermarkContentObj(label: String, pageCenter: BigDecimal, objectNumber: Long): IndirectObj =
    IndirectObj.stream(objectNumber, 0, Prim.dict(), BitVector(watermarkContentStream(label, pageCenter).getBytes))

  def pageCenter(page: Page): Attempt[BigDecimal] = {
    Prim.Dict.numbers("MediaBox")(page.data) match {
      case Attempt.Successful(List(_, _, width, _)) =>
        Attempt.successful(width / 2)
      case Attempt.Failure(_) =>
        Attempt.failure(Err(s"no valid MediaBox in first page for width calculation: $page"))
    }
  }

  def fontResource(fontNumber: Long)(res: FontResource): IndirectObj =
    IndirectObj(res.index, addFontIfDirect(fontNumber)(res.data), None)
}
