package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull}
import fs2.pdf.{
  AnalyzeObjects,
  Analyzed,
  Image,
  IndirectObj,
  Part,
  Rewrite,
  RewriteState,
  RewriteUpdate,
  StreamParser,
  StreamUtil,
  Trailer,
  Xref,
  XrefStream
}

object CollectWatermarkable
{
  def rewriteObj[A](state: RewriteState[A])(obj: IndirectObj): Pull[IO, Part[Trailer], RewriteState[A]] =
    Pull.output1(Part.Obj(obj)).as(state)

  def passthroughImage[A](state: RewriteState[A])(image: Image): Pull[IO, Part[Trailer], RewriteState[A]] =
    rewriteObj(state)(IndirectObj(image.obj.index, image.obj.data, Some(image.stream.original)))

  def rewriteAnalyzed
  (state: RewriteState[WatermarkObjects])
  : Analyzed => Pull[IO, Part[Trailer], RewriteState[WatermarkObjects]] = {
    case Analyzed.Image(image) =>
      passthroughImage(state)(image)
    case Analyzed.Keep(obj, stream) =>
      rewriteObj(state)(IndirectObj(obj.index, obj.data, stream))
    case Analyzed.KeepUnparsable(index, data) =>
      Pull.output1(Part.Unparsable(index, data)).as(state)
    case Analyzed.Xref(Xref(_, trailer, _), _) =>
      Pull.pure(state.copy(trailers = trailer :: state.trailers))
    case Analyzed.XrefStream(XrefStream(_, trailer)) =>
      Pull.pure(state.copy(trailers = trailer :: state.trailers))
    case Analyzed.Garbage(bytes) =>
      Rewrite.parseVersion(bytes)
        .traverse_(Pull.output1)
        .as(state)
    case _ =>
      Pull.pure(state)
  }

  def collectWmObjects(state: RewriteState[WatermarkObjects], analyzed: Analyzed): RewriteState[WatermarkObjects] =
    state.copy(state = WatermarkObjects.collect(state.state)(analyzed))

  def apply
  (state: RewriteState[WatermarkObjects])
  (analyzed: Analyzed)
  : Pull[IO, Part[Trailer], RewriteState[WatermarkObjects]] =
    rewriteAnalyzed(state)(analyzed).map(collectWmObjects(_, analyzed))
}

object RewriteWatermarked
{
  def apply(label: String)
  : RewriteUpdate[WatermarkObjects] => Pull[IO, Part[Trailer], Unit] = {
    case RewriteUpdate(wmObjects, trailer) =>
      val nextObjNumber = trailer.size.toLong
      val fonts = wmObjects.fonts
      StreamUtil.attemptPull("creating watermark objs")(CreateWatermarks(label)(wmObjects, nextObjNumber)) {
        case Watermarks(label, font, pages, resources, rootDir, arrays) =>
          val objs = List(label, font, rootDir) ++ pages.toList ++ resources.toList ++ arrays
          objs.traverse_(o => Pull.output1(Part.Obj(o)))
      }
  }
}

object WatermarkPdf
{
  def rewrite(log: Log)(label: String): Pipe[IO, Byte, Byte] =
    StreamParser.objects(log)
      .andThen(AnalyzeObjects.analyzed)
      .andThen(Rewrite(WatermarkObjects.cons)(CollectWatermarkable.apply)(RewriteWatermarked(label)))
      .andThen(StreamUtil.bytesPipe)
}
