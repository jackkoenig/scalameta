package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.io.{AbsolutePath, RelativePath}

import java.net.URI
import java.nio.file.{FileSystems, Files, Paths}
import java.util.{HashMap => JHashMap}

import munit.FunSuite

class SemanticdbPathsJarFsSuite extends FunSuite {

  private def withJarFileSystem[A](f: java.nio.file.FileSystem => A): A = {
    val jar = Files.createTempFile("scalameta-semanticdb-paths", ".jar")
    Files.delete(jar)
    val uri = URI.create("jar:" + jar.toUri.toString)
    val env = new JHashMap[String, String]()
    env.put("create", "true")
    val fs = FileSystems.newFileSystem(uri, env)
    try f(fs)
    finally {
      fs.close()
      Files.deleteIfExists(jar)
    }
  }

  test("toSemanticdb(RelativePath, targetroot) preserves targetroot's FileSystem") {
    withJarFileSystem { fs =>
      val targetroot = AbsolutePath(fs.getPath("/"))
      val source = RelativePath(Paths.get("src").resolve("Hello.scala"))
      val out = SemanticdbPaths.toSemanticdb(source, targetroot)
      assert(
        out.toNIO.getFileSystem eq fs,
        s"expected jar FS, got ${out.toNIO.getFileSystem} for $out",
      )
      assertEquals(out.toNIO.toString, "/META-INF/semanticdb/src/Hello.scala.semanticdb")
    }
  }

  test("toSemanticdb(TextDocument, targetroot) preserves targetroot's FileSystem") {
    withJarFileSystem { fs =>
      val targetroot = AbsolutePath(fs.getPath("/"))
      val doc = s.TextDocument(uri = "src/Hello.scala")
      val out = SemanticdbPaths.toSemanticdb(doc, targetroot)
      assert(
        out.toNIO.getFileSystem eq fs,
        s"expected jar FS, got ${out.toNIO.getFileSystem} for $out",
      )
      assertEquals(out.toNIO.toString, "/META-INF/semanticdb/src/Hello.scala.semanticdb")
    }
  }

  test("toSemanticdb(TextDocument, ...) decodes percent-escaped segments in doc.uri") {
    withJarFileSystem { fs =>
      val targetroot = AbsolutePath(fs.getPath("/"))
      // RelativePath.toURI percent-encodes each segment individually.
      val doc = s.TextDocument(uri = "src/Hello%20World.scala")
      val out = SemanticdbPaths.toSemanticdb(doc, targetroot)
      assertEquals(out.toNIO.toString, "/META-INF/semanticdb/src/Hello World.scala.semanticdb")
    }
  }

  test("write + read round-trip through a writable jar FileSystem") {
    withJarFileSystem { fs =>
      val targetroot = AbsolutePath(fs.getPath("/"))
      val doc = s.TextDocument(uri = "pkg/Foo.scala")
      val out = SemanticdbPaths.toSemanticdb(doc, targetroot)
      Files.createDirectories(out.toNIO.getParent)
      Files.write(out.toNIO, Array[Byte](1, 2, 3))
      assert(Files.exists(out.toNIO))
      assertEquals(Files.readAllBytes(out.toNIO).toSeq, Seq[Byte](1, 2, 3))
    }
  }

}
