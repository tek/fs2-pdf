package fs2
package pdf

import cats.implicits._
import org.specs2.mutable.Specification

class RewriteTest
extends Specification
{
  "rewrite pdf" >>
  ProcessJarPdf("books/micro", "books/test-out")(WatermarkPdf.rewrite).as(1 must_== 1).unsafeRunSync

  def dois: List[String] =
    List(
    )

  // "test" >>
  // dois.traverse_(a => run(a, s"test-out-$a")).unsafeRunSync.must_==(())
}
