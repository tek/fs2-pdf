package fs2
package pdf

import cats.data.NonEmptyList
import fs2.pdf.{FontResource, IndirectArray, IndirectObj, Page, PageDir, Prim}
import scodec.Attempt

case class Watermarks(
  label: IndirectObj,
  font: IndirectObj,
  pages: NonEmptyList[IndirectObj],
  resources: List[IndirectObj],
  rootDir: IndirectObj,
  arrays: List[IndirectObj],
)

object Watermarks
{
  def pageContentObjects(pages: List[Page]): List[Long] =
    pages.collect { case Page(_, Prim.contents(Prim.Ref(n, _))) => n }

  def updateContentArray(contentNumbers: Set[Long], wmRef: Prim.Ref): IndirectArray => IndirectArray = {
    case original @ IndirectArray(index, contents) =>
    if (contentNumbers.contains(index.number)) IndirectArray(index, Prim.Array(wmRef :: contents.data))
    else original
  }

  def contentArrays(arrays: List[IndirectArray], pages: NonEmptyList[Page], contentNumber: Long): List[IndirectObj] = {
    val contentRef = Prim.Ref(contentNumber, 0)
    arrays
      .map(updateContentArray(Set.from(pageContentObjects(pages.toList)), contentRef))
      .map { case IndirectArray(index, data) => IndirectObj(index, data, None) }
  }

  def label
  (label: String)
  (
    pages: NonEmptyList[Page],
    rootDir: PageDir,
    nextNumber: Long,
    guessedFont: Option[Long],
    res: List[FontResource],
    arrays: List[IndirectArray],
    streams: Set[Long],
    linearized: Boolean,
  )
  : Attempt[Watermarks] = {
    val fontNumber = nextNumber + 1
    val usedFont =
      if (linearized) guessedFont.getOrElse(fontNumber)
      else fontNumber
    val wmPages = pages.map(WatermarkText.pageObject(streams)(nextNumber, usedFont))
    for {
      pageCenter <- WatermarkText.pageCenter(pages.head)
      wmRootDir <- WatermarkText.rootDir(usedFont)(rootDir)
    } yield Watermarks(
      WatermarkText.watermarkContentObj(label, pageCenter, nextNumber),
      WatermarkText.font(fontNumber),
      wmPages,
      res.map(WatermarkText.fontResource(usedFont)),
      wmRootDir,
      contentArrays(arrays, pages, nextNumber),
    )
  }
}
