package fs2
package pdf

import cats.data.EitherT
import cats.effect.IO
import fs2.{Pipe, Stream}

object ProcessJarPdf
{
  val label: String =
    "watermark@springernature.app"

  def parseAndWrite
  (outfile: String)
  (bytes: Stream[IO, Byte])
  (f: String => Pipe[IO, Byte, Byte])
  : Stream[IO, Unit] =
    WriteFile(outfile)(bytes.through(f(label)))

  def processWith[A]
  (doc: String)
  (f: Log => Pipe[IO, Byte, A])
  : EitherT[IO, JarError, Stream[IO, A]] =
    for {
      log <- Log.ioE
      (bytes, _) <- Jar.resourceStream(s"$doc.pdf")
    } yield f(log)(bytes)

  def processWithIO[A]
  (doc: String)
  (f: Log => Stream[IO, Byte] => IO[A])
  : EitherT[IO, JarError, A] =
    for {
      log <- Log.ioE
      (bytes, _) <- Jar.resourceStream(s"$doc.pdf")
      result <- EitherT.liftF(f(log)(bytes))
    } yield result

  def process
  (doc: String, outname: String)
  (f: Log => String => Pipe[IO, Byte, Byte])
  : EitherT[IO, JarError, Stream[IO, Unit]] =
    processWith(doc)(log => in => parseAndWrite(s"$outname.pdf")(in)(f(log)))

  def apply
  (doc: String, outname: String)
  (f: Log => String => Pipe[IO, Byte, Byte])
  : IO[Unit] =
    process(doc, outname)(f)
      .getOrElse(Stream.empty)
      .flatMap(_.compile.drain)
}
