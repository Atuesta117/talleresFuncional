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
//NUESTROS CONJUNTOS DIFUSOS 
val mm2 = muchoMayorQue(2, 6)
val ca3 = cercanosA(3, 4)
val universo: ConjDifuso = _ => 1.0
val vacio: ConjDifuso = _ => 0.0

//COMPLEMENTO 
val cmm2 = complemento(mm2)
val cca3 = complemento(ca3)

// Caso 1: complemento de un elemento que NO pertenece al conjunto
cmm2(1) // esperado: 1.0

// Caso 2: complemento de un elemento que SÍ pertenece totalmente
cmm2(8) // esperado: 0.0

// Caso 3: complemento de un elemento con pertenencia parcial
cmm2(5) // esperado: 0.25

// Caso 4: complemento de un elemento en el centro de cercanos (pertenencia 0.5)
cca3(3) // esperado: 0.0

// Caso 5: complemento de un elemento lejado en cercanos 
cca3(8) // esperado: 0.99


//INTERSECCION

val mm2_y_ca3 = interseccion(mm2, ca3)
val mm2_y_cmm2 = interseccion(mm2, complemento(mm2))

// Caso 1: ambos conjuntos tienen grado alto -> se queda con el minimo
pertenece(8, mm2_y_ca3) // esperado: 0.0099

// Caso 2: uno tiene grado 0 -> interseccion debe ser 0
pertenece(1, mm2_y_ca3) // esperado: 0.0

// Caso 3: elemento donde ca3 domina completamente
pertenece(3, mm2_y_ca3) // esperado: 0.25

// Caso 4: elemento con pertenencia media en ambos
pertenece(4, mm2_y_ca3) // esperado: 0.2

// Caso 5: interseccion de un conjunto con su complemento

pertenece(4, mm2_y_cmm2) // esperado: 0.5 

// Caso 6: Intersección de un conjunto con su propio complemento, con un elemento extremo
pertenece(8, mm2_y_cmm2) // esperado: 0.0 

// Caso 7: interseccion con el universo total (funcion que siempre da 1.0)
pertenece(5, interseccion(mm2, universo)) // esperado: 0.75

// Caso 8: interseccion con el conjunto vacio (funcion que siempre da 0.0)
pertenece(5, interseccion(mm2, vacio)) // esperado: 0.0


//IGUALDAD

// Caso 1: un conjunto es igual a si mismo (reflexividad)
igualdad(mm2, mm2) // esperado: true

// Caso 2: interseccion es conmutativa 
igualdad(interseccion(mm2, ca3), interseccion(ca3, mm2)) // esperado: true

// Caso 3: union es conmutativa
igualdad(union(mm2, ca3), union(ca3, mm2)) // esperado: true

// Caso 4: dos conjuntos distintos NO son iguales
igualdad(mm2, ca3) // esperado: false

// Caso 5: complemento del complemento es igual al original
igualdad(mm2, complemento(complemento(mm2))) // esperado: true

// Caso 6: interseccion con el universo es igual al conjunto original
igualdad(mm2, interseccion(mm2, universo)) // esperado: true

// Caso 7: interseccion con el vacío no es igual al conjunto original
igualdad(mm2, interseccion(mm2, vacio)) // esperado: false
