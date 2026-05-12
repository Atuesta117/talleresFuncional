//Expresione 
val expr1=Suma(Atomo( 'x') , Numero(2))
val expr2=Prod (Atomo('x') , Atomo('x'))
val expr3= Suma( expr1 , Expo(expr2 , Numero(5)) )
val expr4= Logaritmo(Atomo('x') )
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
mostrar (derivar( expr6 , Atomo('x')))
mostrar (derivar( expr2 , Atomo('x')))
mostrar (derivar(expr2 ,Atomo('y')))
mostrar (derivar(Suma(Atomo('k') , Prod(Numero (3.0) , Atomo('x'))) , Atomo('x')))

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
mostrar (limpiar(derivar( expr6 , Atomo('x'))))
mostrar (limpiar(derivar( expr2 , Atomo('x'))))
mostrar (limpiar(derivar(expr2 ,Atomo('y'))))
mostrar (limpiar(derivar(Suma(Atomo('k') , Prod(Numero (3.0) , Atomo('x'))) , Atomo('x'))))
limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))
mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))

