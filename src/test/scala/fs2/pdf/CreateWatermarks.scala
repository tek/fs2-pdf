package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._
import fs2.pdf.Page
import scodec.{Attempt, Err}

object CreateWatermarks
{
  def nonEmpty[A](desc: String)(items: List[A]): Attempt[NonEmptyList[A]] =
    Attempt.fromOption(NonEmptyList.fromList(items), Err(s"no $desc in the document"))

  /**
   * In some broken pdfs, pages are defined with a duplicate object number, but not included in the pages index.
   * Overriding those pages for the watermark anyway results in a broken pdf, so here we filter out those bad pages.
   */
  def validPages(pages: List[Page])(numbers: List[Long]): List[Page] =
    pages.filter(a => numbers.contains(a.index.number))

  def guessFont(fonts: List[Long]): Option[Long] =
    fonts.groupBy(identity).toList.maxByOption(_._2.size).map(_._1)

  def apply
  (label: String)
  (wmObjects: WatermarkObjects, nextObjNumber: Long)
  : Attempt[Watermarks] =
    for {
      pages <- nonEmpty("pages")(validPages(wmObjects.pages)(wmObjects.pageDirNumbers))
      rootDir <- Attempt.fromOption(wmObjects.dir, Err("no page root directory"))
      encodedWm <- Watermarks.label(label)(
        pages.reverse,
        rootDir,
        nextObjNumber,
        guessFont(wmObjects.fonts),
        wmObjects.resources,
        wmObjects.arrays,
        wmObjects.streams,
        wmObjects.linearized,
      )
    } yield encodedWm
}
