import ConjuntosDifusos._

def muchoMayorQue(a: Int, m: Int): ConjDifuso = {
  def mma(x: Int): Double = {
    if (x <= a) 0.0
    else if (x > a && x <= m) (x - a).toDouble / (m - a).toDouble
    else 1.0
  }
  mma
}

def cercanosA(a: Int, k: Int): ConjDifuso = {
  def ca(x: Int): Double = {
    val q = 1.0 + k * (x - a) * (x - a)
    1 / q
  }
  ca
}

val mm2 = muchoMayorQue(2, 6)
mm2(5)
mm2(3)
mm2(1)
mm2(2)
pertenece(3, mm2)
pertenece(8, mm2)

val ca3 = cercanosA(3, 4)
ca3(3)
ca3(4)
ca3(2)
ca3(6)
pertenece(3, ca3)
pertenece(8, ca3)
//casos de prueba grande,

val g1 = grande(1, 10)
val g2 = grande(2, 10)
val g3 = grande(20, 12)
g1(5)
g1(10)
g1(100)
g2(5)
g2(10)
g2(100)
g3(30)
g3(31)
g3(300)

// ============================================================
// CASOS DE PRUEBA: complemento
// ============================================================
// Caso 1: complemento de un elemento que NO pertenece al conjunto
// mm2(1) = 0.0, entonces complemento(mm2)(1) debe ser 1.0
val cmm2 = complemento(mm2)
cmm2(1) // esperado: 1.0

// Caso 2: complemento de un elemento que SÍ pertenece totalmente
// mm2(8) = 1.0, entonces complemento(mm2)(8) debe ser 0.0
cmm2(8) // esperado: 0.0

// Caso 3: complemento de un elemento con pertenencia parcial
// mm2(5) = 0.75, entonces complemento(mm2)(5) debe ser 0.25
cmm2(5) // esperado: 0.25

// Caso 4: complemento de cercanosA, elemento justo en el centro
// ca3(3) = 1.0, entonces complemento(ca3)(3) debe ser 0.0
val cca3 = complemento(ca3)
cca3(3) // esperado: 0.0

// Caso 5: complemento de cercanosA, elemento lejano
// ca3(8) es muy pequeño, entonces su complemento debe ser cercano a 1.0
cca3(8) // esperado: cercano a 1.0

// ============================================================
// CASOS DE PRUEBA: interseccion
// ============================================================
val mm2_y_ca3 = interseccion(mm2, ca3)

// Caso 1: ambos conjuntos tienen grado alto -> se queda con el minimo
// mm2(8)=1.0, ca3(8) es pequeño -> min debe ser ca3(8)
// Verifica que cuando un conjunto domina, el minimo refleja el mas restrictivo
pertenece(8, mm2_y_ca3) // esperado: ca3(8) ~ 0.0099

// Caso 2: uno tiene grado 0 -> interseccion debe ser 0
// mm2(1)=0.0, ca3(1)=0.2 -> min = 0.0
// Verifica que si un elemento no pertenece a uno, no pertenece a la interseccion
pertenece(1, mm2_y_ca3) // esperado: 0.0

// Caso 3: elemento donde ca3 domina completamente
// mm2(3)=0.25, ca3(3)=1.0 -> min = 0.25
// Verifica que el minimo toma el valor del conjunto con menor grado
pertenece(3, mm2_y_ca3) // esperado: 0.25

// Caso 4: elemento con pertenencia media en ambos
// mm2(4)=0.5, ca3(4)=0.2 -> min = 0.2
// Verifica el caso donde ambos tienen grados intermedios distintos
pertenece(4, mm2_y_ca3) // esperado: 0.2

// Caso 5: interseccion de un conjunto con su complemento
// Para cualquier n: min(s(n), 1-s(n)) <= 0.5
// Verifica la propiedad: ningun elemento puede pertenecer mas de 0.5 a S y a su complemento
val mm2_y_cmm2 = interseccion(mm2, complemento(mm2))
pertenece(4, mm2_y_cmm2) // esperado: 0.5 (punto exactamente medio)
pertenece(8, mm2_y_cmm2) // esperado: 0.0 (pertenece totalmente a mm2, nada al complemento)

// Caso 6: interseccion con el universo total (funcion que siempre da 1.0)
// interseccion(s, universo) debe ser igual a s
val universo: ConjDifuso = _ => 1.0
pertenece(5, interseccion(mm2, universo)) // esperado: mm2(5) = 0.75

// Caso 7: interseccion con el conjunto vacio (funcion que siempre da 0.0)
// interseccion(s, vacio) debe ser siempre 0.0
val vacio: ConjDifuso = _ => 0.0
pertenece(5, interseccion(mm2, vacio)) // esperado: 0.0

// ============================================================
// CASOS DE PRUEBA: igualdad
// ============================================================

// Caso 1: un conjunto es igual a si mismo (reflexividad)
// Todo conjunto difuso debe ser igual a si mismo
igualdad(mm2, mm2) // esperado: true

// Caso 2: interseccion es conmutativa -> los resultados deben ser iguales
// min(a,b) == min(b,a) para todo par de valores
igualdad(interseccion(mm2, ca3), interseccion(ca3, mm2)) // esperado: true

// Caso 3: union es conmutativa -> los resultados deben ser iguales
// max(a,b) == max(b,a) para todo par de valores
igualdad(union(mm2, ca3), union(ca3, mm2)) // esperado: true

// Caso 4: dos conjuntos distintos NO son iguales
// mm2 y ca3 modelan conceptos distintos, no pueden ser iguales
igualdad(mm2, ca3) // esperado: false

// Caso 5: complemento del complemento es igual al original
// 1-(1-s(n)) == s(n) para todo n, propiedad fundamental del complemento
igualdad(mm2, complemento(complemento(mm2))) // esperado: true

// Caso 6: interseccion de S con universo es igual a S
// min(s(n), 1.0) == s(n) para todo n
igualdad(mm2, interseccion(mm2, universo)) // esperado: true

// Caso 7: interseccion de S con vacio NO es igual a S (a menos que S sea vacio)
// min(s(n), 0.0) == 0.0 != s(n) en general
igualdad(mm2, interseccion(mm2, vacio)) // esperado: false
