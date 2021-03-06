package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.bits.ByteVector

case class RewriteState[A](state: A, trailers: List[Trailer])

object RewriteState
{
  def cons[A](state: A): RewriteState[A] =
    RewriteState(state, Nil)
}

case class RewriteUpdate[A](state: A, trailer: Trailer)

object Rewrite
{
  def sanitizeTrailer(trailers: NonEmptyList[Trailer]): Trailer =
    Trailer(
      trailers.toList.maxByOption(_.size).getOrElse(trailers.head).size,
      Prim.Dict(
        trailers.map(_.data.data).reduceLeft((a, b) => a ++ b)
          .removed("Prev")
          .removed("DecodeParms")
      )
    )

  def parseVersion(data: ByteVector): Option[Part[Trailer]] =
    Version.codec.decode(data.bits)
      .toOption
      .map(a => Part.Version(a.value))

  def emitUpdate[A]: RewriteState[A] => Pull[IO, Part[Trailer], RewriteUpdate[A]] = {
    case RewriteState(state, h :: t) =>
      val trailer = sanitizeTrailer(NonEmptyList(h, t))
      Pull.output1(Part.Meta(trailer))
        .as(RewriteUpdate(state, trailer))
    case _ =>
      StreamUtil.failPull("no trailer")
  }

  def rewrite[A]
  (initial: A)
  (collect: RewriteState[A] => Analyzed => Pull[IO, Part[Trailer], RewriteState[A]])
  (in: Stream[IO, Analyzed])
  : Pull[IO, Part[Trailer], RewriteUpdate[A]] =
    StreamUtil.pullState(collect)(in)(RewriteState.cons(initial))
      .flatMap(emitUpdate)

  def rewriteAndUpdate[A]
  (initial: A)
  (collect: RewriteState[A] => Analyzed => Pull[IO, Part[Trailer], RewriteState[A]])
  (update: RewriteUpdate[A] => Pull[IO, Part[Trailer], Unit])
  (in: Stream[IO, Analyzed])
  : Pull[IO, Part[Trailer], Unit] =
    rewrite(initial)(collect)(in)
      .flatMap(update)

  def apply[A]
  (initial: A)
  (collect: RewriteState[A] => Analyzed => Pull[IO, Part[Trailer], RewriteState[A]])
  (update: RewriteUpdate[A] => Pull[IO, Part[Trailer], Unit])
  : Pipe[IO, Analyzed, ByteVector] =
    _
      .through(rewriteAndUpdate(initial)(collect)(update)(_).stream)
      .through(WritePdf.parts)

  def forState[A]
  (initial: A)
  (collect: RewriteState[A] => Analyzed => Pull[IO, Part[Trailer], RewriteState[A]])
  : Pipe[IO, Analyzed, A] =
    in => {
      val p = for {
        result <- rewrite(initial)(collect)(in).mapOutput(Left(_))
        _ <- Pull.output1(Right(result))
      } yield ()
      p.stream.collect { case Right(RewriteUpdate(s, _)) => s }
    }
}
