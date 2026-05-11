
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
  case Numero(n)      => if (n == n.toInt) n.toInt.toString else n.toString
  case Atomo(a)       => "" + a + ""
  case Suma(e1, e2)   => "(" + mostrar(e1) + " + " + mostrar(e2) + ")"
  case Resta(e1, e2)  => "(" + mostrar(e1) + " - " + mostrar(e2) + ")"
  case Prod(e1, e2)   => "(" + mostrar(e1) + " * " + mostrar(e2) + ")"
  case Div(e1, e2)    => "(" + mostrar(e1) + " / " + mostrar(e2) + ")"
  case Expo(e1, e2)   => "(" + mostrar(e1) + " ^ " + mostrar(e2) + ")"
  case Logaritmo(e1)  => "(lg(" + mostrar(e1) + "))"
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
    case Resta(e1,e2) => (limpiar(e1),limpiar(e2)) match{
      case (Numero(0.0), e) =>
    }


  }
}
