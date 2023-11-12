object load {
  def fs2Version(version: String): Unit = {
    repl.load.apply(s"""
      import $$ivy.`co.fs2::fs2-core:$version`
      import $$ivy.`co.fs2::fs2-reactive-streams:$version`
      import $$ivy.`co.fs2::fs2-io:$version`

      import cats.syntax.all._
      import cats.effect.{IO, Resource}
      import fs2.io.file.{Files, Path}
      import fs2.{Stream, text}

      // For unsafeRunSync
      implicit val runtime = cats.effect.unsafe.IORuntime.global
    """)
  }

  def fs2: Unit = fs2Version("3.9.3")
}
