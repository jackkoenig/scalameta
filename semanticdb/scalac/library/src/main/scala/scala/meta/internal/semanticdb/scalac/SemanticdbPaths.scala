package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.io.PathIO
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, RelativePath}

import java.net.URI
import java.nio.file.{FileSystem, Path}

object SemanticdbPaths {
  val semanticdbPrefix: RelativePath = RelativePath("META-INF").resolve("semanticdb")
  val semanticdbExtension = "semanticdb"
  private val scalaExtension = "scala"
  private val scalaScriptExtension = "sc"

  /** META-INF/semanticdb directory under `targetroot`, on `targetroot`'s FileSystem. */
  def semanticdbRoot(targetroot: AbsolutePath): AbsolutePath = {
    val fs = targetroot.toNIO.getFileSystem
    AbsolutePath(targetroot.toNIO.resolve(fs.getPath("META-INF", "semanticdb")))(targetroot)
  }

  def isSemanticdb(path: RelativePath): Boolean = path.toNIO.startsWith(semanticdbPrefix.toNIO) &&
    PathIO.extension(path.toNIO) == semanticdbExtension

  def toScala(
      semanticdb: AbsolutePath,
      sourceroot: AbsolutePath,
      targetroot: AbsolutePath,
  ): AbsolutePath = sourceroot.resolve(toScala(semanticdb.toRelative(targetroot)))

  def toScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    val scalaSibling = path.resolveSibling(_.stripSuffix("." + semanticdbExtension))
    semanticdbPrefix.relativize(scalaSibling)
  }

  def isScala(path: RelativePath): Boolean = {
    val extension = PathIO.extension(path.toNIO)
    extension == scalaExtension || extension == scalaScriptExtension
  }

  def fromScala(path: RelativePath): RelativePath = {
    require(isScala(path))
    val semanticdbSibling = path.resolveSibling(_ + "." + semanticdbExtension)
    semanticdbPrefix.resolve(semanticdbSibling)
  }

  def toSemanticdb(path: RelativePath, targetroot: AbsolutePath): AbsolutePath = {
    require(isScala(path))
    val fs = targetroot.toNIO.getFileSystem
    val onFs = onFileSystem(path.toNIO, fs)
    val withExt = onFs.resolveSibling(onFs.getFileName.toString + "." + semanticdbExtension)
    semanticdbRoot(targetroot).resolve(RelativePath(withExt))
  }

  def toSemanticdb(doc: s.TextDocument, targetroot: AbsolutePath): AbsolutePath = {
    val fs = targetroot.toNIO.getFileSystem
    // doc.uri is a relative URI whose segments are individually percent-encoded
    // (see RelativePath.toURI). Resolution must happen in Path space, not URI space:
    // when targetroot is on a ZipFileSystem its toUri() is opaque, and URI.resolve on
    // an opaque base returns the child URI unchanged, silently dropping the jar prefix.
    val decoded = new URI(doc.uri).getPath
    val relPath = fs.getPath(decoded + "." + semanticdbExtension)
    semanticdbRoot(targetroot).resolve(RelativePath(relPath))
  }

  /** Re-creates `p` on `fs` if it was constructed on a different FileSystem. */
  private def onFileSystem(p: Path, fs: FileSystem): Path =
    if (p.getFileSystem eq fs) p
    else {
      val segments = Array.newBuilder[String]
      val it = p.iterator()
      while (it.hasNext) segments += it.next().toString
      val arr = segments.result()
      if (arr.isEmpty) fs.getPath("") else fs.getPath(arr.head, arr.tail: _*)
    }
}
