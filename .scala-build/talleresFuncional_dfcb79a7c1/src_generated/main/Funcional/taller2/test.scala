package Funcional.taller2


final class test$_ {
def args = test_sc.args$
def scriptPath = """Funcional/taller2/test.sc"""
/*<script>*/
import ConjuntosDifusos._
def muchoMayorQue(a: Int, m: Int): ConjDifuso = {
  def calcular(n: Int): Double = {
    if (n <= a) 0.0
    else if (n >= a && n <= m) (n - a).toDouble / (m - a).toDouble
    else 1.0
  }
  calcular(_)
}

val mm2 = muchoMayorQue(2, 6)

println(mm2(5)) // 0.0

/*</script>*/ /*<generated>*//*</generated>*/
}

object test_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new test$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export test_sc.script as `test`

