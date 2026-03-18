package ConjuntosDifusos
package object ConjuntosDifusos {
  type ConjDifuso = Int => Double

  def pertenece(x: Int, s: ConjDifuso): Double = s(x)

  import scala.math.pow

//punto 1 def grande

//punto 2 complemento, union e intersección

  def complemento(s: ConjDifuso): ConjDifuso = { // complemento puede usar fn anónima en vez de 'calcular'
    (n: Int) =>
      1 - s(n) // aquí calculamos el grado de pertenencia al complemento de s

  }

  def interseccion(cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = {
    def calcular(n: Int): Double = {
      math.min(cd1(n), cd2(n))
    }
    calcular
  }

  def inclusion(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
    def iteracionCola(n: Int): Boolean = {
      if (n == 1001) true
      else if (cd1(n) <= cd2(n)) iteracionCola(n + 1)
      else false
    }
    iteracionCola(0)
  }
//punto 3 igualdad e inclusion

  def igualdad(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
    inclusion(cd1, cd2) && inclusion(cd2, cd1)
  }

//mis funciones a demostrar:

  def grande(d: Int, e: Int): ConjDifuso = {
    def calcular(n: Int): Double = {
      val quotient = n.toDouble / (n + d)
      val exponent = e
      val result = pow(quotient, exponent).toDouble
      result
    }
    calcular
  }

//otra union jeje

  def union(cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = n =>
    math.max(cd1(n), cd2(n))

}
