package fs2
package pdf

import cats.data.ValidatedNel
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.bits.BitVector

object StreamParser
{
  def chunk: Pipe[IO, Byte, BitVector] =
    _
      .chunkN(10000000)
      .map(c => c.toBitVector)

  def objects(log: Log): Pipe[IO, Byte, Parsed] =
    chunk
      .andThen(ChunkObjects.pipe)
      .andThen(StripObjects.pipe(log))
      .andThen(ParseObjects.pipe)

  def analyzed(log: Log): Pipe[IO, Byte, Analyzed] =
    objects(log)
      .andThen(Analyze.analyzed(log))

  def validate(log: Log)(bytes: Stream[IO, Byte]): IO[ValidatedNel[String, Unit]] =
    ValidatePdf.fromParsed(objects(log)(bytes))

  def decode(log: Log): Pipe[IO, Byte, Decoded] =
    Decode.decoded(log)

  def elements(log: Log): Pipe[IO, Byte, Element] =
    decode(log).andThen(Elements.pipe)
}
