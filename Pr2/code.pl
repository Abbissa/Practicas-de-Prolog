:- module(_,_,[classic, assertions, regtypes]).

:- use_module(library(lists)).

author_data('Pozo','Serrano','Carlos Gaspar','b190234').

:- doc(title,"Segunda practica (Programacion en ISO-Prolog)").
:- doc(author,"Carlos Pozo Serrano, B190234").
:- doc(hide,[author_data/4,compute_length/2]).
:- doc(module,"@section{Pruebas: }

@begin{enumerate}
@item pots/3:
@begin{verbatim}

?- pots(0,100,Ps).

Ps = [1] ? .

no
?- pots(7,4321,Ps).

Ps = [2401,343,49,7,1] ? .

no
?- pots(2,10000,Ps).

Ps = [8192,4096,2048,1024,512,256,128,64,32,16,8,4,2,1] ? .

no
?- 

@end{verbatim}

@item mpart/3:
@begin{verbatim}

?- mpart(0,100,P).

P = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] ? .

no
?- mpart(2,200,P).

P = [128,64,8] ? .

P = [128,64,4,4] ? .

P = [128,32,32,8] ? .

P = [64,64,64,8] ? .

P = [128,64,4,2,2] ? .

P = [128,32,32,4,4] ? 

yes
?- mpart(5,1000,P).

P = [625,125,125,125] ? .

P = [625,125,125,25,25,25,25,25] ? .

P = [125,125,125,125,125,125,125,125] ? .

P = [625,125,125,25,25,25,25,5,5,5,5,5] ? .

P = [625,125,25,25,25,25,25,25,25,25,25,25] ? .

P = [125,125,125,125,125,125,125,25,25,25,25,25] ? .

P = [625,125,125,25,25,25,25,5,5,5,5,1,1,1,1,1] ? 

yes
?- 
@end{verbatim}


@item maria/3:
@begin{verbatim}

?- maria(0,100,NPart).

NPart = 1 ? .

no
?- maria(2,200,NPart).

NPart = 205658 ? .

no
?- maria(5,1000,NPart).

NPart = 14373 ? .

no
?- 

@end{verbatim}
@item guardar_grafo/1:
@begin{verbatim}

?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),arista(c,h)]).

yes
?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),arista(b,h)]).

yes
?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),
   arista(b,h),arista(a,h),arista(f,a),arista(j,b)]).

yes
?-
@end{verbatim}

@item aranya/1:
@begin{verbatim}

?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),arista(c,h)]).

yes
?- aranya.

yes
?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),arista(b,h)]).

yes
?- aranya.

no
?- guardar_grafo([arista(a,b),arista(a,c),
   arista(a,d),arista(a,e),arista(b,f),arista(f,g),
   arista(b,h),arista(a,h),arista(f,a),arista(j,b)]).

yes
?- aranya.

yes
?- 
@end{verbatim}
@end{enumerate}
").

:- pred pots(M,N,Ps) # "Dados @var{M} y @var{N} enteros, devuelve en @var{Ps} una lista con las potencias de @var{M} que son menores o iguales que @var{N}, en orden descendente.".
:-doc(pots/3,"Predicado 1.1:
    Este predicado resulve los casos base, @var{M} 0 o 1, en caso de no poder resolverlo de esta forma delega la resolucion en @var{aux_pots/4}, tras lo cual invierte la lista devuelta para que este en orden decreciente de sumandos. @includedef{pots/3}").
pots(0,_,[1]):-!.
pots(1,_,[1]):-!.
pots(A,B,C):-aux_pots(A,B,1,D),reverse([1|D],C).


:- pred aux_pots(M,N,R,Ps) # "Dados @var{M}, @var{N} y @var{R} enteros, devuelve en @var{Ps} una lista con las potencias de @var{M} que son menores o iguales que @var{N}, en orden ascendednte.".
:-doc(aux_pots/4,"Este predicado resulve los casos base, el resultado de multiplicar @var{R} por @var{M} seria superior a @var{N}, o @var{R} y @var{N} son iguales,en otro caso multiplica @var{R} por @var{M} y llama recursivamente con el resultado obtenido. @includedef{aux_pots/4}").
aux_pots(M,N,R,[]):- C is N//R, C<M,!. 
aux_pots(_,N,N,[N]):-!.
aux_pots(M,N,R,[E|C]):-E is R*M,aux_pots(M,N,E,C).

:-doc(sol/2,"Este predicado sirve para almacenar las distintas soluciones del predicado @var{mpart/3} y guarda el numero de elementos junto con la lista resultado. @includedef{sol/2}").
:-dynamic sol/2.

:- pred mpart(M,N,P) # "Dados M y N enteros, devuelve en P por backtracking todas las particiones @var{M}-arias de @var{N}, representadas como listas de enteros. Las soluciones son devultas con las listas mas cortas primero.".
:-doc(mpart/3,"Predicado 1.2:
    Este predicado borra todos las soluciones almacenadas con anterioridad, a continuacion llama a @var{pots/3} para obtener la lista de las potencias, luego llama a @var{calculate/2} para generar las soluciones y almacenarlas y por ultimo se usa @var{num/3} y @var{sol/2} para recoger todas las soluciones almacenadas.
Comentario: Esta solucion no es la mas optima, ya que para obtener tan solo una solucion el predicado ha de obtener todas las soluciones antes para asi poder devolverlas en orden. A pesar de ello no se ha encontrado otra solucion que cumpla con las condiciones que se piden. @includedef{mpart/3}").
mpart(M,N,P):-retractall(sol(_,_)), pots(M,N,C),calculate(N,C), num(1,N,R),sol(R,P).


:- pred calculate(N,C) # "Dados @var{N} entero y @var{C} una lista de enteros almacena en memoria todas las formas posibles de obtener @var{N} a partir de los elementos de @var{C}.".
:-doc(calculate/2,"Este predicado recoge todas las formas de obtener N a partir de una combinacion de elementos de C a traves del predicado sum/3 y a continuacion lo almacena en memoria a traves de los predicados length y assert, siendo almacenados como proposiciones sol. Se itera a traves de backtracking mientras haya soluciones, una vez se acaban estas termina.  @includedef{calculate/2}").

calculate(N,C):-sum(N,C,P), length(P,Length), assert(sol(Length,P)),fail.
calculate(_,_).


:- pred sum(N,C,P) # "Dados @var{N} entero y @var{C} una lista de enteros, devuelve en @var{P} una lista de enteros cuya suma es @var{N} y esta compuesta por elementos contenidos en @var{C}}.".
:-doc(sum/3,"Este predicado resuelve el caso base de que N sea 0, en cualquier otro caso comprueba que N sea mayor que X, en ese caso almacena X al inicio de la lista que se devuelve como solucion y se llama a sum de forma recursiva. Por ultimo, hay otra regla que permite vaciar la lista C y llamar de forma recursiva, esto se usa tanto para obtener soluciones con numeros mas pequenos por bactracking, como para completar soluciones que requieren de numeros mas pequenos para poder llegar a N.  @includedef{sum/3}").
sum(0,_,[]):-!.
sum(N,[X|C],[D|L]):-N>=X, D=X, R is N-X, sum(R,[X|C],L).
sum(N,[_|C],L):-N>0,sum(N,C,L).


:- pred num(A,N,R) # "Dados @var{A} y  @var{N} enteros, devuelve en @var{R} un numero comprendido entre @var{A} y @var{N}}.".
:-doc(num/3,"Este predicado predicado resuelve el caso base en el que R es A si A es menor o igual que N, si se piden mas soluciones se llama recursivamente con A = A+1.  @includedef{num/3}").
num(A,N,A):-A=<N.
num(A,N,R):-A<N, A1 is A+1,num(A1,N,R). 

compute_length([],0).
compute_length([_|T],N) :- compute_length(T,TN), N is TN+1.

:- pred maria(M,N,NPart) # "@var{NPart} es el numero de particiones @var{M}-arias de @var{N}.".
:-doc(maria/3,"Predicado 1.3:
    Este predicado ejecuta el predicado setof para obtener todas las solucionse que puede dar mpart, a continuacion se ejecuta length para obtener la longitud de la lista obtenida.
Comentario: esta solucion podria ser optimizada utilizando una modificacion de calculate, para asi evitar el uso de memoria que este predicado requiere. @includedef{maria/3}").

maria(M,N,NPart):- setof(X,mpart(M,N,X),L), length(L,NPart).

:-doc(arista/2,"Este predicado sirve para almacenar el grafo en memoria. @includedef{sol/2}").

:-dynamic arista/2.


:- pred guardar_grafo(G) # "Dado @var{G}  un grafo representado como una lista de aristas, deja asertados en la base de datos como hechos del predicado @var{arista/2} los elementos de G. Al llamar a este predicado se borra cualquier hecho que hubiera guardado anteriormente de este predicado.".
:-doc(guardar_grafo/1,"Predicado 2.1:
    Este predicado borra los hechos que haya guardados de arista y a continuacion llama a @var{ggAux/1} para recorrer la lista de aristas y almacenarlas en la base de datos. @includedef{guardar_grafo/1}").
guardar_grafo(G):-retractall(arista(_,_)),ggAux(G).

:- pred ggAux(G) # "Dado @var{G} un grafo representado como una lista de aristas, deja asertados en la base de datos como hechos del predicado arista/2 los elementos de G.".
:-doc(ggAux/1,"Este predicado comprueba que todos los elementos de una lista son aristas.@includedef{ggAux/1}").                     

ggAux([X|G]):-functor(X,F,_),F=arista, assert(X),ggAux(G).
ggAux([]).

:- pred aranya # "Predicado 2.2: 
Dado un grafo @var{G} guardado en la base datos, comprueba que este conntiene una araña de expansion, y en caso contrario falle de forma finita".
:-doc(aranya/0,"Este predicado llama a todos_nodos/1 para almacenar en una lista todos los nodos del grafo, y a continuacion llama a check_aranya/2 para resolver el problema. @includedef{aranya/0}"). 
aranya:-todos_nodos(S),check_aranya(S,S).

:- pred check_aranya(L,F) # "Dados dos conjuntos de nodos @var{L} y @var{F} trata de encontrar una araña de expansion con raiz un nodo de @var{L} que contenga a todos los nodos de @var{F}".
:-doc(check_aranya/2,"Este predicado borrar para eliminar el nodo maestro de la lista de nodos a contener por la araña, ya esta contenido, y a continuacion se intenta hallar una araña de expansion con raiz ese nodo a traves de @var{comprobar/3}, si tiene exito se cortan el resto de ramas, sino se borra el primer elemento de la lista de candidatos a maestro y se vuelve a llamar a este predicado. @includedef{check_aranya/2}"). 

check_aranya([D|_],F):-borrar(F,D,F2),comprobar([D],D,F2),!.
check_aranya([_|L],F):-check_aranya(L,F). 

:- pred comprobar(L,D,F) # "Dados dos conjuntos de nodos @var{L} y @var{F} trata de encontrar una araña de expansion con raiz un nodo de @var{D} ".
:-doc(comprobar/3,"Este predicado elige un elemento de la lista L (nodos añadidos) y uno de la lista F (nodos por visitar) y comprueba que haya una arista que los una, a continuacion borra el nodo escogido de la lista F, y si el nodo escogido de la lista F no es el nodo maestro, D, lo borra de la lista L, solo puede tener dos aristas cada nodo, finalmente añade el nodo sacado de la lista F a la lista L y se llama de forma recursiva. El predicado finaliza cuando la lista de nodos por visitar esta vacia. @includedef{comprobar/3}"). 

comprobar(L,D,F):-member(X,F),member(A,L),aristo(X,A),borrar(F,X,R),(A=D->L1=L;borrar(L,A,L1)),anadir(L1,X,L2),comprobar(L2,D,R).
comprobar(_,_,[]).

:- pred todos_nodos(S) # "Este predicado retorna en @var{S} una lista que contiene todos los nodos contenidos en el grafo almacenado en memoria".
:-doc(todos_nodos/1,"Este predicado llama a set of para hallar todas las soluciones de arst. @includedef{todos_nodos/1}"). 

todos_nodos(S):-setof(A,(arst(A)),S).

:- pred arst(A) # "Tiene exito si @var{A} es un nodo del grafo almacenado en memoria".
:-doc(arst/1,"Este predicado comprueba si este nodo es parte de una arista (a la izquierda o a la derecha) . @includedef{arst/1}"). 
arst(A):-arista(A,_);arista(_,A).

:- pred aristo(A,B) # "Tiene exito si @var{A} y @var{B} forman una arista del grafo almacenado en memoria".
:-doc(aristo/2,"Este predicado comprueba si estos nodos forman una arista (en cualquiera de los dos sentidos) . @includedef{aristo/2}"). 

aristo(A,B):-arista(A,B);arista(B,A).

:- pred anadir(L,X,R) # "Este predicado añade @var{X} a la lista @var{L} y lo devuelve en @var{R}. Si ya pertenecia a @var{L}, no lo añade".
:-doc(anadir/3,"Este predicado resuelve los casos base de que X ya este contenido en la lista o que la lista desde la que añadir ya este vacia, en caso contrario se llama de forma recursiva . @includedef{anadir/3}"). 

anadir([X|L],X,[X|L]):-!.
anadir([A|L],X,[A|R]):-anadir(L,X,R).
anadir([],X,[X]).

:- pred borrar(L,X,R) # "Este predicado borra @var{X} de la lista @var{L} y lo devuelve en @var{R}. Se presupone que esta contenido en ella".
:-doc(borrar/3,"Este predicado resuelve el caso base de hallar X  lista  y devuelve la lista sin incluir el elemento, en caso contrario se llama de forma recursiva . @includedef{anadir/3}"). 


borrar([X|L],X,L).
borrar([A|L],X,[A|R]):-borrar(L,X,R).
