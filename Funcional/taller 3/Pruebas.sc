import Newton . _

//Expresione 
val exprSuma   = Suma(Suma(Numero(0.0), Atomo('x')), Suma(Atomo('y'), Numero(0.0)))
val exprProd   = Prod(Prod(Numero(1.0), Atomo('x')), Prod(Atomo('y'), Numero(1.0)))
val exprDiv    = Div(Div(Numero(0.0), Atomo('x')), Div(Atomo('y'), Numero(1.0)))
val exprExpo   = Suma(Suma(Expo(Atomo('x'), Numero(1.0)), Expo(Numero(0.0), Atomo('y'))), Expo(Atomo('x'), Numero(0.0)))
val exprResta  = Resta(Resta(Atomo('x'), Numero(0.0)), Numero(5.0))
val exprProd2 = Logaritmo(Expo(Atomo('x'), Numero(3)))
val expr1=Suma(Atomo( 'x') , Numero(2))
val expr2=Prod (Atomo('x') , Atomo('x'))
val expr3= Suma( expr1 , Expo(expr2 , Numero(5)) )
val expr4= Logaritmo(Expo(Atomo('x'), Numero(3)))
val expr5=Prod(Div( expr1 , expr2 ) , Resta( expr3 , expr4 ) )
val expr6=Expo(Atomo('x') , Numero(3))
//casos de prueba ejercicio 1:

 mostrar(Numero(4.0))
 mostrar(Atomo('x'))
 mostrar(Suma(Atomo('x'), Numero(2.0)))
 mostrar(Expo(Atomo('x'), Numero(3.0)))
 mostrar(Logaritmo(Atomo('x')))
 mostrar(Suma(Suma(Atomo('x'), Numero(1.0)), Numero(2.0)))
 
//Casos Prueba derivar 
mostrar (derivar( expr6 , Atomo('x')))//Expo
mostrar (derivar( expr2 , Atomo('x')))//Prod 
mostrar (derivar(expr2 ,Atomo('y')))//Prod diferente atomo, lo trata como constante 
mostrar (derivar(expr5 ,Atomo('x')))//Prod, Div y Resta
mostrar (derivar(expr4 ,Atomo('x')))//Logaritmo y Expo 
mostrar (derivar(Suma(Atomo('k') , Prod(Numero (3.0) , Atomo('x'))) , Atomo('x')))//Suma y Prod

//casos de prueba ejercicio 3:

 mostrar(Numero(9.0))
 evaluar(Numero(9.0), Atomo('x'), 99.0)
 mostrar(Atomo('x'))
 evaluar(Atomo('x'), Atomo('x'), 7.0)
 mostrar(Suma(Atomo('x'), Numero(2.0)))
 evaluar(Suma(Atomo('x'), Numero(2.0)), Atomo('x'), 3.0)
 mostrar(Prod(Atomo('x'), Atomo('x')))
 evaluar(Prod(Atomo('x'), Atomo('x')), Atomo('x'), 4.0)
 mostrar(Expo(Atomo('x'), Numero(3.0)))
 evaluar(Expo(Atomo('x'), Numero(3.0)), Atomo('x'), 3.0)
 mostrar(Logaritmo(Suma(Atomo('x'), Numero(2.0))))
 evaluar(Logaritmo(Suma(Atomo('x'), Numero(2.0))), Atomo('x'), 5.0)

 //Casos prueba limpiar 
mostrar(exprSuma)
mostrar(limpiar(exprSuma))

mostrar(exprProd)
mostrar(limpiar(exprProd))

mostrar(exprDiv)
mostrar(limpiar(exprDiv))

mostrar(exprExpo)
mostrar(limpiar(exprExpo))

mostrar(exprResta)
mostrar(limpiar(exprResta))

mostrar(derivar(exprProd2, Atomo('y')))
mostrar(limpiar(derivar(exprProd2, Atomo('y'))))

def buenaAprox ( f : Expr , a : Atomo , d : Double ) : Boolean = {
  Math.abs(evaluar(f, a, d)) < 0.001
}
//Casos algoritmo Newton 

val e1 = Resta(Expo(Atomo('x'), Numero(2.0)), Numero(4.0))
mostrar(e1)
raizNewton(e1, Atomo('x'), 3.0, buenaAprox)

val e2 = Resta(Expo(Atomo('x'), Numero(2.0)), Numero(2.0))
mostrar(e2)
raizNewton(e2, Atomo('x'), 1.0, buenaAprox)

val e3 = Resta(Expo(Atomo('x'), Numero(3.0)), Numero(8.0))
mostrar(e3)
raizNewton(e3, Atomo('x'), 3.0, buenaAprox)

val e4 = Resta(Atomo('x'), Numero(5.0))
mostrar(e4)
raizNewton(e4, Atomo('x'), 10.0, buenaAprox)

val e5 = Resta(Expo(Atomo('x'), Numero(2.0)), Numero(9.0))
mostrar(e5)
raizNewton(e5, Atomo('x'), -1.0, buenaAprox)

val e6 = Resta(Resta(Suma(Expo(Atomo('x'), Numero(3.0)),
              Prod(Numero(-2.0), Expo(Atomo('x'), Numero(2.0)))),
              Atomo('x')), Numero(1.0))
mostrar(e6)
raizNewton(e6, Atomo('x'), 2.0, buenaAprox)


