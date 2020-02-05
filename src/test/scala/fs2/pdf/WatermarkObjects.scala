package fs2
package pdf

import fs2.pdf.{Analyzed, FontResource, Image, IndirectArray, Obj, Page, PageDir, ParsedXref, Xref, XrefStream}
import scodec.bits.ByteVector

case class WatermarkObjects(
  pages: List[Page],
  dir: Option[PageDir],
  pageDirNumbers: List[Long],
  resources: List[FontResource],
  images: List[Image],
  xrefs: List[ParsedXref],
  size: Long,
  xrefStream: Option[XrefStream],
  fonts: List[Long],
  arrays: List[IndirectArray],
  streams: Set[Long],
  linearized: Boolean,
)

object WatermarkObjects
{
  def cons: WatermarkObjects =
    WatermarkObjects(Nil, None, Nil, Nil, Nil, Nil, 0L, None, Nil, Nil, Set.empty, false)

  def collect(old: WatermarkObjects): Analyzed => WatermarkObjects = {
    case Analyzed.Page(page) =>
      old.copy(pages = page :: old.pages)
    case Analyzed.PageDir(dir) =>
      old.copy(dir = Some(dir))
    case Analyzed.PageNumbers(numbers) =>
      old.copy(pageDirNumbers = old.pageDirNumbers ++ numbers)
    case Analyzed.FontResources(res, nums) =>
      old.copy(resources = res :: old.resources, fonts = nums ::: old.fonts)
    case Analyzed.Fonts(nums) =>
      old.copy(fonts = nums ::: old.fonts)
    case Analyzed.Xref(xref, data) =>
      old.copy(xrefs = ParsedXref(xref, data.getOrElse(ByteVector.empty)) :: old.xrefs)
    case Analyzed.Image(image) =>
      old.copy(images = image :: old.images)
    case Analyzed.IndirectArray(array) =>
      old.copy(arrays = array :: old.arrays)
    case Analyzed.XrefStream(xs) =>
      old.copy(xrefStream = Some(xs))
    case Analyzed.StartXref(startxref) =>
      old.xrefStream match {
        case Some(XrefStream(tables, data)) =>
          collect(old)(Analyzed.Xref(Xref(tables, data, startxref), None))
        case None =>
          old
      }
    case Analyzed.Linearized =>
      old.copy(linearized = true)
    case Analyzed.Keep(Obj(Obj.Index(num, _), _), Some(_)) =>
      old.copy(streams = old.streams + num)
    case Analyzed.Keep(_, _) =>
      old
    case Analyzed.KeepUnparsable(_, _) =>
      old
    case Analyzed.Garbage(_) =>
      old
  }
}
