package ch.epfl.scala.decoder

import ch.epfl.scala.decoder.testutils.*
import tastyquery.Exceptions.*

import scala.util.Properties

class Scala3LtsBinaryVariableDecoderTests extends BinaryVariableDecoderTests(ScalaVersion.`3.lts`)
class Scala3NextBinaryVariableDecoderTests extends BinaryVariableDecoderTests(ScalaVersion.`3.next`)

abstract class BinaryVariableDecoderTests(scalaVersion: ScalaVersion) extends BinaryDecoderSuite:
  def isScala33 = scalaVersion.isScala33
  def isScala34 = scalaVersion.isScala34

  test("local variable") {
    val source =
      """|package example
         |
         |class A:
         |  def foo =
         |    val x: Int = 1
         |    x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int foo()", "int x", 6, "x: Int")
  }

  test("local array variable") {
    val source =
      """|package example
         |
         |class A {
         |  def foo() = 
         |    val x = Array(1, 2, 3)
         |    x
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int[] foo()", "int[] x", 6, "x: Array[Int]")
  }

  test("local lazy val".ignore) {
    val source =
      """|package example
         |
         |class A:
         |  def foo() =
         |    lazy val x: Int = 1
         |    x
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int foo()", "int x$1$lzyVal", 6, "x: Int")
  }

  test("local module val".ignore) {
    val source =
      """|package example
         |
         |class A {
         |  def foo() =
         |    object B
         |    B
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "java.lang.Object foo()", " example.A$B$2$ B$1", 6, "B: B.type")
  }

  test("method parameter") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(y: String) =
         |    println(y)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "void foo(java.lang.String y)", "java.lang.String y", 5, "y: String")
  }

  test("constructor parameters and variables") {
    val source =
      """|package example
         |
         |class A(x: Int):
         |  private val y = {
         |    val z = x * x
         |    z
         |  }
         |
         |  def this(x: Int, y: Int) =
         |    this(x + y)
         |    val z = x + y
         |    println(z)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "void <init>(int x)", "int x", 3, "x: Int")
    decoder.assertDecodeVariable("example.A", "void <init>(int x)", "int y", 6, "y: Int")
    decoder.assertDecodeVariable("example.A", "void <init>(int x)", "int z", 7, "z: Int")
    decoder.assertDecodeVariable("example.A", "void <init>(int x, int y)", "int x", 10, "x: Int")
    decoder.assertDecodeVariable("example.A", "void <init>(int x, int y)", "int y", 10, "y: Int")
    decoder.assertDecodeVariable("example.A", "void <init>(int x, int y)", "int z", 12, "z: Int")
  }

  test("contextual parameters") {
    val source =
      """|package example
         |
         |class A:
         |  def m(x: Int): String ?=> String = ???
         |  def m(): (Int, String) ?=> Int = ???
         |  def m(x: String): Int ?=> String ?=> String = ???
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    if isScala33 then
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(int x, java.lang.String evidence$1)",
        "java.lang.String evidence$1",
        4,
        "<anon>: String"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "int m(int evidence$2, java.lang.String evidence$3)",
        "int evidence$2",
        5,
        "<anon>: Int"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "int m(int evidence$2, java.lang.String evidence$3)",
        "java.lang.String evidence$3",
        5,
        "<anon>: String"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(java.lang.String x, int evidence$4, java.lang.String evidence$5)",
        "int evidence$4",
        6,
        "<anon>: Int"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(java.lang.String x, int evidence$4, java.lang.String evidence$5)",
        "java.lang.String evidence$5",
        6,
        "<anon>: String"
      )
    else
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(int x, java.lang.String contextual$1)",
        "java.lang.String contextual$1",
        4,
        "<anon>: String"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "int m(int contextual$2, java.lang.String contextual$3)",
        "int contextual$2",
        5,
        "<anon>: Int"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "int m(int contextual$2, java.lang.String contextual$3)",
        "java.lang.String contextual$3",
        5,
        "<anon>: String"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(java.lang.String x, int contextual$4, java.lang.String contextual$5)",
        "int contextual$4",
        6,
        "<anon>: Int"
      )
      decoder.assertDecodeVariable(
        "example.A",
        "java.lang.String m(java.lang.String x, int contextual$4, java.lang.String contextual$5)",
        "java.lang.String contextual$5",
        6,
        "<anon>: String"
      )
  }

  test("ambiguous local variables") {
    val source =
      """|package example
         |
         |class A:
         |  def foo() =
         |    var i = 0
         |    while i < 10 do
         |      val x = i
         |      i += 1
         |    val x = 17
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    // TODO fix ambiguity somehow
    decoder.assertAmbiguousVariable("example.A", "void foo()", "int x", 8)
    decoder.assertDecodeVariable("example.A", "void foo()", "int x", 9, "x: Int")
  }

  test("ambiguous variables and parameters") {
    val source =
      """|package example
         |
         |class A :
         |  def foo(a: Boolean) =
         |    if a then
         |      val x = 1
         |      x
         |    else
         |      val x = "2"
         |      x
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "java.lang.Object foo(boolean a)", "int x", 7, "x: Int")
    decoder.assertDecodeVariable("example.A", "java.lang.Object foo(boolean a)", "java.lang.String x", 9, "x: String")
  }

  test("failing ambiguous local variables") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(a: Boolean) =
         |    if (a) {val x = 1} else {val x = 2}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertAmbiguousVariable("example.A", "void foo(boolean a)", "int x", 5)
  }

  test("binds for tuple") {
    val source =
      """|package example
         |
         |class A {
         |  def foo: Int = 
         |    val x = (1, 2)
         |    val (c, d) = (3, 4)
         |    x match
         |      case (a, b) => a + b
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int foo()", "scala.Tuple2 x", 5, "x: (Int, Int)")
    decoder.assertDecodeVariable("example.A", "int foo()", "int c", 6, "c: Int")
    decoder.assertDecodeVariable("example.A", "int foo()", "int d", 6, "d: Int")
    decoder.assertDecodeVariable("example.A", "int foo()", "int a", 8, "a: Int")
    decoder.assertDecodeVariable("example.A", "int foo()", "int b", 8, "b: Int")
  }

  test("binds for case classes") {
    val source =
      """|package example
         |
         |class B
         |case class C(x: Int, y: String) extends B
         |case class D(z: String) extends B
         |case class E(v: Int) extends B
         |case class F(w: Int) extends B
         |
         |class A:
         |  private def bar(a: B) =
         |    a match
         |      case F(w) => w
         |      case C(x, y) =>
         |        x
         |      case D(z) => 0
         |      case E(v) => 1
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int bar(example.B a)", "int w", 12, "w: Int")
    decoder.assertDecodeVariable("example.A", "int bar(example.B a)", "int x", 14, "x: Int")
  }

  test("this variable") {
    val source =
      """|package example
         |
         |class A:
         |  def foo: Int = 
         |    4
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int foo()", "example.A this", 5, "this: A.this.type")
  }

  test("value class this") {
    val source =
      """|package example
         |
         |class A(x: Int) extends AnyVal {
         |  def foo: Int = 
         |    Seq(1, 2).map(_ + x).sum
         |    
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A$", "int foo$extension(int $this)", "int $this", 5, "x: Int")
    decoder.assertDecodeVariable("example.A$", "int foo$extension$$anonfun$1(int $this$1, int _$1)", "int $this$1", 5, "x.<capture>: Int")
    decoder.assertDecodeVariable("example.A$", "int foo$extension$$anonfun$1(int $this$1, int _$1)", "int _$1", 5, "<anon>: Int")
  }

  test("parameters of mixin and trait static forwarders") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo(x: Int): Int = x
         |}
         |
         |class B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.B", "int foo(int x)", "int x", 7, "x: Int")
    decoder.assertDecodeVariable(
      "example.A",
      "int foo$(example.A $this, int x)",
      "example.A $this",
      4,
      "this: A.this.type"
    )
  }

  test("captured param in a local def") {
    val source =
      """|package example
         |
         |class A {
         |  def foo(x: Int) = {
         |    def bar() = x
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int bar$1(int x$1)", "int x$1", 5, "x.<capture>: Int")
  }

  test("by-name arg capture") {
    val source =
      """|package example
         |
         |class A {
         |  def foo(x: => Int) = ???
         |
         |  def bar(x: Int) = 
         |    foo(x)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int bar$$anonfun$1(int x$1)", "int x$1", 7, "x.<capture>: Int")
  }

  test("lazy val capture") {
    val source =
      """|package example
         |
         |class A {
         |  def foo =
         |    val y = 4
         |    lazy val z = y + 1
         |    def bar = z
         |    z
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable(
      "example.A",
      "int bar$1(scala.runtime.LazyInt z$lzy1$3, int y$3)",
      "scala.runtime.LazyInt z$lzy1$3",
      7,
      "z.<capture>: Int"
    )
    decoder.assertDecodeVariable(
      "example.A",
      "int z$lzyINIT1$1(scala.runtime.LazyInt z$lzy1$1, int y$1)",
      "scala.runtime.LazyInt z$lzy1$1",
      7,
      "z.<capture>: Int"
    )
    decoder.assertDecodeVariable(
      "example.A",
      "int z$1(scala.runtime.LazyInt z$lzy1$2, int y$2)",
      "scala.runtime.LazyInt z$lzy1$2",
      7,
      "z.<capture>: Int"
    )
  }

  test("bridge parameter") {
    val source =
      """|package example
         |class A
         |class B extends A
         |
         |class C:
         |  def foo(x: Int): A = new A
         |
         |class D extends C:
         |  override def foo(y: Int): B = new B
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertIgnoredVariable("example.D", "example.A foo(int x)", "int x", "Bridge")
  }

  test("inlined param") {
    val source =
      """|package example
         |
         |class A {
         |  inline def foo(x: Int): Int = x + x
         |
         |  def bar(y: Int) = foo(y + 2)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A", "int bar(int y)", "int x$proxy1", 4, "x: Int")
  }

  test("inlined this") {
    val source =
      """|package example
         |
         |class A(x: Int):
         |  inline def foo: Int = x + x
         |
         |class B:
         |  def bar(a: A) = a.foo
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.B", "int bar(example.A a)", "example.A A_this", 7, "this: A.this.type")
  }

  test("partial function variables") {
    val source =
      """|package example
         |
         |class A:
         |  def foo(x: Int) = 
         |    val xs = List(x, x + 1, x + 2)
         |    xs.collect { case z if z % 2 == 0 => z }
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecodeVariable("example.A$$anon$1", "boolean isDefinedAt(int x)", "int x", 6, "x: A")
    decoder.assertDecodeVariable("example.A$$anon$1", "boolean isDefinedAt(int x)", "int z", 6, "z: Int")
    decoder.assertDecodeVariable(
      "example.A$$anon$1",
      "java.lang.Object applyOrElse(int x, scala.Function1 default)",
      "scala.Function1 default",
      6,
      "default: A1 => B1"
    )
  }

  test("tail-local variables".ignore) {
    val source =
      """|package example
         |
         |class A {
         |  @annotation.tailrec
         |  private def factAcc(x: Int, acc: Int): Int =
         |    if x <= 1 then List(1, 2).map(_ * acc).sum
         |    else factAcc(x - 1, x * acc)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.showVariables("example.A", "int factAcc$$anonfun$1(int acc$tailLocal1$1, int _$1)")
    // decoder.assertDecodeVariable("example.A", "int factAcc$$anonfun$1(int acc$tailLocal1$1, int _$1)", "int acc$tailLocal1$1", 6, "acc.<capture>: Int")
  }

  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    /* decoder.assertNotFoundVariable(
      "scala.quoted.runtime.impl.QuotesImpl$reflect$defn$",
      "dotty.tools.dotc.core.Symbols$Symbol TupleClass(int arity)",
      "dotty.tools.dotc.core.Types$TypeRef x$proxy1",
      2816
    ) */
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.QuoteMatcher$",
      "scala.Option treeMatch(dotty.tools.dotc.ast.Trees$Tree scrutineeTree, dotty.tools.dotc.ast.Trees$Tree patternTree, dotty.tools.dotc.core.Contexts$Context x$3)",
      "scala.util.boundary$Break ex",
      128,
      "ex: Break[T]"
    )
    decoder.assertDecodeVariable(
      "org.scalajs.ir.VersionChecks",
      "void <init>(java.lang.String current, java.lang.String binaryEmitted)",
      "java.lang.String current",
      26,
      "current: String"
    )
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.QuoteMatcher$",
      "scala.collection.immutable.Seq $eq$qmark$eq(dotty.tools.dotc.ast.Trees$Tree scrutinee0, dotty.tools.dotc.ast.Trees$Tree pattern0, scala.collection.immutable.Map x$3, dotty.tools.dotc.core.Contexts$Context x$4, scala.util.boundary$Label evidence$4)",
      "dotty.tools.dotc.ast.Trees$Tree s",
      192,
      "s: Tree[Types.Type]"
    )
    decoder.assertDecodeVariable(
      "dotty.tools.dotc.typer.Namer$Completer",
      "scala.collection.immutable.List completerTypeParams(dotty.tools.dotc.core.Symbols$Symbol sym, dotty.tools.dotc.core.Contexts$Context x$2)",
      "dotty.tools.dotc.core.Symbols$Symbol sym",
      829,
      "sym: Symbols.Symbol"
    )
    decoder.assertDecodeVariable(
      "dotty.tools.scripting.Util$",
      "scala.collection.IterableOnce collectMainMethods$1$$anonfun$1(java.net.URLClassLoader cl$1, java.lang.String targetPath$1, java.io.File packageMember)",
      "java.lang.String targetPath$1",
      35,
      "targetPath.<capture>: String"
    )
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.printers.SourceCode$SourceCodePrinter",
      "java.lang.Object printTree$$anonfun$adapted$5(scala.Option elideThis$48, scala.runtime.ObjectRef argsPrefix$2, scala.collection.immutable.List args1$2)",
      "scala.runtime.ObjectRef argsPrefix$2",
      408,
      "argsPrefix.<capture>: String"
    )
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.printers.SourceCode$SourceCodePrinter",
      "scala.quoted.runtime.impl.printers.SourceCode$SourceCodePrinter printTree(java.lang.Object tree, scala.Option elideThis)",
      "scala.runtime.ObjectRef argsPrefix",
      390,
      "argsPrefix: String"
    )
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.printers.SourceCode$SourceCodePrinter",
      "java.lang.String escapedString$$anonfun$adapted$1(java.lang.Object ch)",
      "java.lang.Object ch",
      1435,
      "ch: Char"
    )
    decoder.assertDecodeVariable(
      "dotty.tools.dotc.typer.Implicits$OfTypeImplicits",
      "scala.collection.immutable.List refs()",
      "scala.collection.mutable.ListBuffer buf",
      276,
      "buf: ListBuffer[Types.TermRef]"
    )
    // decode from Java class
    decoder.assertDecodeVariable(
      "dotty.tools.io.JDK9Reflectors",
      "java.util.jar.JarFile newJarFile(java.io.File arg0, boolean arg1, int arg2, java.lang.Object arg3)",
      "java.lang.Object arg3",
      63,
      "x$3: Object"
    )
    decoder.assertDecodeVariable(
      "dotty.tools.repl.package$$anon$1",
      "void dotty$tools$dotc$reporting$UniqueMessagePositions$_setter_$dotty$tools$dotc$reporting$UniqueMessagePositions$$positions_$eq(scala.collection.mutable.HashMap x$0)",
      "scala.collection.mutable.HashMap x$0",
      8,
      "x$0: HashMap[(SourceFile, Integer), Diagnostic]"
    )
    decoder.assertDecodeVariable(
      "scala.quoted.runtime.impl.QuoteMatcher$",
      "scala.collection.immutable.Seq $eq$qmark$eq(dotty.tools.dotc.ast.Trees$Tree scrutinee0, dotty.tools.dotc.ast.Trees$Tree pattern0, scala.collection.immutable.Map x$3, dotty.tools.dotc.core.Contexts$Context x$4, scala.util.boundary$Label evidence$4)",
      "dotty.tools.dotc.ast.Trees$Tree s",
      491,
      "s: Tree[Types.Type]"
    )
    decoder.assertDecodeVariable(
      "dotty.tools.dotc.typer.Namer$TypeDefCompleter",
      "dotty.tools.dotc.core.Contexts$Context given_Context$lzyINIT1$1(scala.runtime.LazyRef given_Context$lzy1$1)",
      "dotty.tools.dotc.core.Contexts$Context x$proxy4",
      965,
      "x$proxy4: Contexts.Context | Null"
    )

