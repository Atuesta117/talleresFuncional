package Funcional.taller2


final class pruebas$_ {
def args = pruebas_sc.args$
def scriptPath = """Funcional/taller2/pruebas.sc"""
/*<script>*/
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

//GRANDE

// Caso 1: d=1, e=2. (ambos parámetros pequeños)
val g0 = grande(1,2)
g0(5)   // 0.6944444444444445
g0(10)  // 0.8264462809917354
g0(100) // 0.9802960494069208

//Caso 2: d=2, e=10. (número e más grande)
val g1 = grande(2,10)
g1(5)   // 0.03457161303360778
g1(10)  // 0.1615055828898458
g1(100) // 0.8203482998751549

//Caso 3: d=20, e=12 (ambos parámetros grandes)
val g2 = grande(20,12)
g2(5)   // ≈ 0.0 (4.09e-9, prácticamente cero)
g2(10)  // ≈ 0.0 (1.88e-6, prácticamente cero)
g2(100) // 0.11215665478461515

//Caso 4: d=1, e=100 (parámetro e muy grande)
val g3 = grande(1,100)
g3(5)   // ≈ 0.0 (1.20e-8, prácticamente cero)
g3(10)  // 7.256571590148175e-05
g3(100) // 0.3697112123291189

//Caso 5: d=50, e=10. (d muy grande, e grande)
val g4 = grande(50,10)
g4(5)   // ≈ 0.0 (3.85e-11, prácticamente cero)
g4(10)  // ≈ 0.0 (1.65e-8, prácticamente cero)
g4(100) // 0.017341529915832606


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

/*</script>*/ /*<generated>*//*</generated>*/
}

object pruebas_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new pruebas$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export pruebas_sc.script as `pruebas`

