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
