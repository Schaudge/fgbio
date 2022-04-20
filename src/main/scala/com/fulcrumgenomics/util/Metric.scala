/*
 * The MIT License
 *
 * Copyright (c) 2016 Fulcrum Genomics LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

package com.fulcrumgenomics.util

import com.fulcrumgenomics.cmdline.FgBioMain.FailureException
import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.{Writer => CommonsWriter}
import com.fulcrumgenomics.commons.reflect.{ReflectionUtil, ReflectiveBuilder}
import com.fulcrumgenomics.commons.util.{DelimitedDataParser, LazyLogging}
import enumeratum.EnumEntry
import htsjdk.samtools.util.Iso8601Date

import java.io.{PrintWriter, StringWriter, Writer}
import java.nio.file.Path
import java.text.{DecimalFormat, NumberFormat, SimpleDateFormat}
import java.util.Date
import scala.collection.compat._
import scala.collection.concurrent.TrieMap
import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success}

object Metric extends LazyLogging {
  val Delimiter: Char = '\t'
  val DelimiterAsString: String = s"$Delimiter"

  /** A typedef for [[scala.Long]] to be used when representing counts. */
  type Count = Long

  /** A typedef for [[scala.Double]] to be used when representing values between 0 and 1. */
  type Proportion = Double

  /** A format object for Doubles that outputs with limited precision. Should be synchronized over. */
  private val BigDoubleFormat: NumberFormat = new DecimalFormat("0.######")

  /** A format object for Doubles that are very small and require scientific notation. Should be sync'd over. */
  private val SmallDoubleFormat: NumberFormat = new DecimalFormat("0.#####E0")

  /** A format object for Dates. Should be sync'd over. */
  private val DateFormat = new SimpleDateFormat("yyyy-MM-dd")

  /** Add a cache of names for metric classes. */
  private val nameCache = new TrieMap[Class[_ <: Metric], Seq[String]]()

  /** A class that provides streaming writing capability for metrics. */
  class MetricWriter[T <: Metric] private[Metric](val writer: Writer)(implicit tt: ru.TypeTag[T]) extends CommonsWriter[T] {
    this.writer.write(names[T].mkString(DelimiterAsString))
    this.writer.write("\n")

    /** Writes a metric value to the output. */
    def write(metric: T): Unit = {
      writer.write(metric.values.mkString(DelimiterAsString))
      writer.write("\n")
    }

    /** Writes one or more metric values to the output. */
    def write(metrics: T*): Unit = metrics.foreach { metric => this.write(metric) }

    def flush(): Unit = this.writer.flush()
    override def close(): Unit = this.writer.close()
  }

  /** Get the names of the arguments in the order they were defined for the type [T]. */
  def names[T <: Metric](implicit tt: ru.TypeTag[T]): Seq[String] = {
    names(ReflectionUtil.typeTagToClass[T])
  }

  /** Get the names of the arguments in the order they were defined for the class T. */
  def names[T <: Metric](clazz: Class[T]): Seq[String] = {
    this.nameCache.get(clazz) match {
      case Some(names) => names
      case None        =>
        val reflectiveBuilder = new ReflectiveBuilder(clazz)
        val names = reflectiveBuilder.argumentLookup.ordered.map(_.name)
        this.nameCache.put(clazz, names)
        names
    }
  }

  private[util] def build[T <: Metric](reflectiveBuilder: ReflectiveBuilder[T],
                                       toArg: Int => String,
                                       fail: (String, Option[Throwable]) => Unit
                                      ): T = {
    val names = Metric.names[T]
    forloop(from = 0, until = names.length) { i =>
      reflectiveBuilder.argumentLookup.forField(names(i)) match {
        case Some(arg) =>
          val value = {
            val tmp = toArg(i)
            if (tmp.isEmpty && arg.argumentType == classOf[Option[_]]) ReflectionUtil.SpecialEmptyOrNoneToken else tmp
          }

          val argumentValue = ReflectionUtil.constructFromString(arg.argumentType, arg.unitType, value) match {
            case Success(v) => v
            case Failure(thr) =>
              fail(s"Could not construct value for column '${arg.name}' of type '${arg.typeDescription}' from '$value'", Some(thr))
          }
          arg.value = argumentValue
        case None =>
          fail(s"Did not have a field with name '${names(i)}'.", None)
      }
    }

    // build it.  NB: if arguments are missing values, then an exception will be thrown here
    // Also, we don't use the default "build()" method since if a collection or option is empty, it will be treated as
    // missing.
    reflectiveBuilder.build(reflectiveBuilder.argumentLookup.ordered.map(arg => arg.value getOrElse unreachable(s"Arguments not set: ${arg.name}")))
  }

  private[util] def failReading[T <: Metric](clazz: Class[T],
                                             message: String,
                                             lineNumber: Option[Int] = None,
                                             throwable: Option[Throwable] = None,
                                             source: Option[String] = None): Unit = {
    val sourceMessage = source.map("\nIn source: " + _).getOrElse("")
    val prefix        = lineNumber match {
      case None    => "For metric"
      case Some(n) => s"On line #$n for metric"
    }
    val fullMessage   = s"$prefix '${clazz.getSimpleName}'$sourceMessage\n$message"
    throwable.foreach { thr =>
      val stringWriter = new StringWriter
      thr.printStackTrace(new PrintWriter(stringWriter))
      val banner = "#" * 80
      logger.debug(banner)
      logger.debug(stringWriter.toString)
      logger.debug(banner)
    }
    throw FailureException(message=Some(fullMessage))
  }

  /** Reads metrics from a set of lines.  The first line should be the header with the field names.  Each subsequent
    * line should be a single metric. */
  def iterator[T <: Metric](lines: Iterator[String], source: Option[String] = None)(implicit tt: ru.TypeTag[T]): Iterator[T] = {
    val clazz: Class[T]   = ReflectionUtil.typeTagToClass[T]


    if (lines.isEmpty) failReading(clazz=clazz, message="No header found", lineNumber=Some(1), source=source)
    val parser = new DelimitedDataParser(lines=lines, delimiter=Delimiter, ignoreBlankLines=false, trimFields=true)
    val reflectiveBuilder = new ReflectiveBuilder(clazz)

    parser.zipWithIndex.map { case (row, rowIndex) =>
      build(
        reflectiveBuilder = reflectiveBuilder,
        toArg             = i => row[String](i),
        fail              = (message, throwable) => failReading(clazz=clazz, message=message, lineNumber=Some(rowIndex+2), throwable=throwable, source=source)
      )
    }
  }

  /** Reads metrics from the given path.  The first line should be the header with the field names.  Each subsequent
    * line should be a single metric. */
  def iterator[T <: Metric](path: Path)(implicit tt: ru.TypeTag[T]): Iterator[T] = iterator[T](Io.readLines(path), Some(path.toString))

  /** Reads metrics from a set of lines.  The first line should be the header with the field names.  Each subsequent
    * line should be a single metric. */
  def read[T <: Metric](lines: Iterator[String], source: Option[String] = None)(implicit tt: ru.TypeTag[T]): Seq[T] = {
    iterator(lines, source).toSeq
  }

  /** Reads metrics from the given path.  The first line should be the header with the field names.  Each subsequent
    * line should be a single metric. */
  def read[T <: Metric](path: Path)(implicit tt: ru.TypeTag[T]): Seq[T] = read[T](Io.readLines(path), Some(path.toString))

  /** Writes one or more metrics to the given writer.  The first line will be a header with the field names.  Each subsequent
    * line is a single metric.
    */
  def write[T <: Metric](writer: Writer, metric: T*)(implicit tt: ru.TypeTag[T]): Unit = {
    val out = new MetricWriter[T](writer)
    out.write(metric:_*)
    out.close()
  }

  /** Writes one or more metrics to the given path.  The first line will be a header with the field names.  Each subsequent
    * line is a single metric.
    */
  def write[T <: Metric](path: Path, metric: T*)(implicit tt: ru.TypeTag[T]): Unit = write(Io.toWriter(path), metric:_*)

  /** Writes a metric to the given writer.  The first line will be a header with the field names.  Each subsequent
    * line is a single metric.
    */
  def write[T <: Metric](writer: Writer, metrics: IterableOnce[T])(implicit tt: ru.TypeTag[T]): Unit = {
    val out = new MetricWriter[T](writer)
    out ++= metrics
    out.close()
  }

  /** Writes metrics to the given path.  The first line will be a header with the field names.  Each subsequent
    * line is a single metric.
    */
  def write[T <: Metric](path: Path, metrics: IterableOnce[T])(implicit tt: ru.TypeTag[T]): Unit = {
    val writer = Io.toWriter(path)
    write(writer, metrics)
    writer.close()
  }

  /** Returns a MetricWriter that can be used to stream metrics out to a file. */
  def writer[T <: Metric](path: Path)(implicit tt: ru.TypeTag[T]): MetricWriter[T] = writer(Io.toWriter(path))

  /** Returns a MetricWriter that can be used to stream metrics out to a file. */
  def writer[T <: Metric](writer: Writer)(implicit tt: ru.TypeTag[T]): MetricWriter[T] = new MetricWriter[T](writer)
}


class MetricSorter[Key <: Ordered[Key], T <: Metric](maxObjectsInRam: Int = MetricSorter.MaxInMemory,
                                                     keyfunc: T => Key,
                                                     tmpDir: DirPath = Io.tmpDir
                                                    )(implicit tt: ru.TypeTag[T]) extends Sorter[T, Key](
  maxObjectsInRam = maxObjectsInRam,
  codec           = new MetricSorter.MetricSorterCodec[T](),
  keyfunc         = keyfunc,
  tmpDir          = tmpDir
)

object MetricSorter {
  /** The default maximum # of records to keep and sort in memory. */
  val MaxInMemory: Int = 1e6.toInt

  class MetricSorterCodec[T <: Metric]()(implicit tt: ru.TypeTag[T])
    extends Sorter.Codec[T] with LazyLogging {
    private val clazz: Class[T]   = ReflectionUtil.typeTagToClass[T]
    private val reflectiveBuilder = new ReflectiveBuilder(clazz)

    /** Encode the metric into an array of bytes. */
    def encode(metric: T): Array[Byte] = metric.values.mkString(Metric.DelimiterAsString).getBytes

    /** Decode a metric from an array of bytes. */
    def decode(bs: Array[Byte], start: Int, length: Int): T = {
      val fields = new String(bs.slice(from=start, until=start+length)).split(Metric.DelimiterAsString)
      Metric.build(
        reflectiveBuilder = reflectiveBuilder,
        toArg = i => fields(i),
        fail  = (message, throwable) => Metric.failReading(clazz=clazz, message=message, throwable=throwable)
      )
    }
  }
}

/**
  * Base trait for metrics.
  *
  * All classes extending this class should be a case class.  By convention, all fields should be lower case with
  * words separated by underscores.
  */
trait Metric extends Product with Iterable[(String,String)] {
  /** Get the names of the arguments in the order they were defined. */
  def names: Seq[String] = Metric.names(getClass)

  /** Get the values of the arguments in the order they were defined. */
  def values: Seq[String] = productIterator.map(formatValue).toSeq

  /** Gets the value of the field by name. */
  def apply(name: String): String = get(name).getOrElse {
    throw new NoSuchElementException(s"key not found: $name")
  }

  /** Gets the value of the field by name, returns None if it does not exist. */
  def get(name: String): Option[String] = {
    this.names.indexOf(name) match {
      case -1   => None
      case idx  => Some(this.values(idx))
    }
  }

  /** Gets an iterator over the fields of this metric in the order they were defined.  Returns tuples of names and values */
  override def iterator: Iterator[(String,String)] = this.names.zip(this.values).iterator

  /** @deprecated use [[formatValue]] instead. */
  @deprecated(message="Use formatValue instead.", since="0.5.0")
  protected def formatValues(value: Any): String = formatValue(value)

  /** Override this method to customize how values are formatted. */
  protected def formatValue(value: Any): String = value match {
    case null           => ""
    case None           => ""
    case Some(x)        => formatValue(x)
    case d: Iso8601Date => d.toString
    case d: Date        => Metric.DateFormat.synchronized { Metric.DateFormat.format(d) }
    case f: Float       => formatValue(f.toDouble)
    case d: Double if d != 0.0 && d < 0.00001 && d > -0.00001 =>
      Metric.SmallDoubleFormat.synchronized { Metric.SmallDoubleFormat.format(d) }
    case d: Double if d.isNaN || d.isInfinity => d.toString
    case d: Double      => Metric.BigDoubleFormat.synchronized { Metric.BigDoubleFormat.format(d) }
    case e: EnumEntry   => e.entryName
    case other          => other.toString
  }

  override def toString: String = names.zip(values).toMap.toString
}
