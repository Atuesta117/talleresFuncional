
package objetc Newton{
  trait Expr
  case class Numero (d : Double ) extends Expr
  case class Atomo ( x : Char ) extends Expr
  case class  Suma( e1 : Expr , e2 : Expr ) extends Expr
  case class Prod ( e1 : Expr , e2 : Expr ) extends Expr
  case class Resta ( e1 : Expr , e2 : Expr ) extends Expr
  case class Div ( e1 : Expr , e2 : Expr ) extends Expr
  case class Expo ( e1 : Expr , e2 : Expr ) extends Expr
  case class Logaritmo ( e1 : Expr ) extends Expr

  def mostrar(e: Expr): String = e match {
  case Numero(d)       => d.toString
  case Atomo(x)        => x.toString
  case Suma(e1, e2)    => "(" + mostrar(e1) + " + " + mostrar(e2) + ")"
  case Resta(e1, e2)   => "(" + mostrar(e1) + " - " + mostrar(e2) + ")"
  case Prod(e1, e2)    => "(" + mostrar(e1) + " * " + mostrar(e2) + ")"
  case Div(e1, e2)     => "(" + mostrar(e1) + " / " + mostrar(e2) + ")"
  case Expo(e1, e2)    => "(" + mostrar(e1) + " ^ " + mostrar(e2) + ")"
  case Logaritmo(e1)   => "(lg(" + mostrar(e1) + "))"
}
  
  def derivar(e:Expr, a:Atomo):Expr = {
    
    def derivarAux(e:Expr): Expr = e  match {
      case Atomo(e1) => if(e1 == a.x) Numero(1.0) else Numero(0.0) 
      case Numero(e1) => Numero(0.0)
      case Suma(e1, e2) => Suma(derivarAux(e1), derivarAux(e2))
      case Prod(e1,e2) => Suma(Prod(derivarAux(e1), e2), Prod(e1, derivarAux(e2)))
      case Resta(e1,e2)=>  Resta(derivarAux(e1), derivarAux(e2))
      case Div(e1,e2)=> Div(Resta(Prod(derivarAux(e1),e2), Prod(derivarAux(e2),e1)), Expo(e2, Numero(2)))
      case Logaritmo(e1) => Prod(Div(Numero(1),e1 ), derivarAux(e1))
      case Expo(e1, e2) => Prod(Prod(e2, Expo(e1, Resta(e2, Numero(1.0)))), derivarAux(e1))
    }
    derivarAux(e)


  }
  def limpiar(e: Expr): Expr = e match {
    case Suma(e1,e2) => (limpiar(e1), limpiar(e2)) match{
      case (Numero(0.0), e) => e 
      case (e, Numero(0.0)) => e 
      case (e1,e2) => Suma(e1,e2)
    }
    case Prod(e1,e2) => (limpiar(e1), limpiar(e2)) match{
      case (Numero(0.0), e) => Numero(0.0) 
      case (e, Numero(0.0)) => Numero(0.0)
      case (Numero(1.0), e) => e 
      case (e, Numero(1.0)) => e
      case (e1,e2) => Prod(e1,e2)
    }
    case Div(e1,e2) => (limpiar(e1), limpiar(e2)) match{
      case(Numero(0.0), e) => Numero(0.0)
      case(e1,e2) => Div(e1,e2)
    }
      case Expo(e1,e2) => (limpiar(e1), limpiar(e2)) match{
        case (e, Numero(0.0)) => Numero(1.0)
        case (Numero(0.0), e) => Numero(0.0)
        case (e, Numero(1.0)) => e 
        case (e1,e2) => Expo(e1,e2) 

      }
    case Resta(e1, e2) => (limpiar(e1), limpiar(e2)) match {
      case (e1, Numero(0.0)) => e1       // x - 0 = x
      case (e1, e2)          => Resta(e1, e2)
}
    case Numero(e) => Numero(e)
    case Atomo(x) => Atomo(x)


  }

  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {

  case Numero(d)    => d

  case Atomo(x)     =>
    if (x == a.x) v else 0.0  // otros átomos no deberían aparecer según el enunciado

  case Suma(e1, e2)    => evaluar(e1, a, v) + evaluar(e2, a, v)
  case Resta(e1, e2)   => evaluar(e1, a, v) - evaluar(e2, a, v)
  case Prod(e1, e2)    => evaluar(e1, a, v) * evaluar(e2, a, v)
  case Div(e1, e2)     => evaluar(e1, a, v) / evaluar(e2, a, v)
  case Expo(e1, e2)    => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
  case Logaritmo(e1)   => Math.log(evaluar(e1, a, v))

}

def raizNewton(f: Expr, a: Atomo, x0: Double,
               ba: (Expr, Atomo, Double) => Boolean): Double = {
  if (ba(f, a, x0))
    x0
  else {
    val fx  = evaluar(f, a, x0)                        
    val fpx = evaluar(derivar(f, a), a, x0)            
    val x1  = x0 - (fx / fpx)                          
    raizNewton(f, a, x1, ba)                           
  }
}
}
