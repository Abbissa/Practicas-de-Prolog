%:-module(_,_,['bf/bfall']).
:- module(_,_,[assertions,regtypes]).
author_data('Pozo','Serrano','Carlos Gaspar','b190234').

:- doc(title,"Primera practica (Programacion Logica Pura)").
:- doc(author,"Carlos Pozo Serrano, B190234").
:- doc(hide,[author_data/4]).
:- doc(module,"@section{Pruebas: }
@begin{enumerate}
@item byte_list/1:
@begin{verbatim}
?- byte_list([[bind(0),bind(0),bind(0),bind(0),
        bind(0),bind(0),bind(0),bind(0)]
,[hexd(a),hexd(1)]]).

yes
?- byte_list([bind(0)]).

no
?- byte_list(L).

L = [] ? ;

L = [[bind(0),bind(0),bind(0),bind(0),
     bind(0),bind(0),bind(0),bind(0)]] ? ;

L = [[bind(0),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(0),bind(0)],
    [bind(0),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(0),bind(0)]] ?

yes
?-

@end{verbatim}
@item byte_conversion/2:
@begin{verbatim}
?- byte_conversion([hexd(a),hexd(2)],R).

R = [bind(1),bind(0),bind(1),bind(0),
    bind(0),bind(0),bind(1),bind(0)] ? ;

no
?- byte_conversion([hexd(a)],R).

no
?- byte_conversion(H,
    [bind(1),bind(1),bind(0),bind(0),
    bind(1),bind(1),bind(1),bind(0)]).

H = [hexd(c),hexd(e)] ? ;

no
?- 


@end{verbatim}
@item byte_list_conversion/2
@begin{verbatim}
?- byte_list_conversion([[hexd(1),hexd(a)],
    [hexd(0),hexd(c)]],B).

B = [[bind(0),bind(0),bind(0),bind(1),
    bind(1),bind(0),bind(1),bind(0)],
    [bind(0),bind(0),bind(0),bind(0),
    bind(1),bind(1),bind(0),bind(0)]] ? ;

no
?- byte_list_conversion(H,
    [[bind(1),bind(0),bind(1),bind(1),
    bind(1),bind(0),bind(0),bind(0)],
    [bind(0),bind(1),bind(1),bind(0),
    bind(0),bind(1),bind(0),bind(1)]]).

H = [[hexd(b),hexd(8)],[hexd(6),hexd(5)]] ? ;

no
?- byte_list_conversion([[hexd(1),hexd(a)],
    [hexd(0)]],B).

no
?-

@end{verbatim}
@item get_nth_bit_from_byte/3:
@begin{verbatim}
?- get_nth_bit_from_byte(N,
    [bind(1),bind(0),bind(1),bind(0),
    bind(1),bind(0),bind(1),bind(1)],NB).

N = s(s(s(s(s(s(s(0))))))),
NB = bind(1) ? ;

N = s(s(s(s(s(s(0)))))),
NB = bind(1) ? ;

N = s(s(s(s(s(0))))),
NB = bind(0) ? ;

N = s(s(s(s(0)))),
NB = bind(1) ? 

yes

?- get_nth_bit_from_byte(0,B,bind(1)).

B = [bind(1),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(0),bind(0)] ? ;

B = [bind(1),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(0),bind(1)] ? ;

B = [bind(1),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(1),bind(0)] ? ;

B = [bind(1),bind(0),bind(0),bind(0),
    bind(0),bind(0),bind(1),bind(1)] ? 

yes
?-

@end{verbatim}
@item  byte_list_clsh/2
@begin{verbatim}
?- byte_list_clsh([[hexd(a),hexd(1)],
    [hexd(b),hexd(5)]],CLShL).

CLShL = [[hexd(4),hexd(3)],[hexd(6),hexd(b)]] ? ;

no

?- byte_list_clsh([[hexd(3),hexd(2)],
    [hexd(e),hexd(6)]],
    [[hexd(6),hexd(5)],
    [hexd(c),hexd(c)]]).

yes
?- 

@end{verbatim}
@item byte_list_crsh/2
@begin{verbatim}
?- byte_list_crsh([[hexd(4),hexd(3)],
    [hexd(6),hexd(b)]],CRShL).

CRShL = [[hexd(a),hexd(1)],[hexd(b),hexd(5)]] ? ;

no

?- byte_list_crsh([[hexd(3),hexd(1)],
    [hexd(4),hexd(2)]],
    [[hexd(1),hexd(8)],
    [hexd(a),hexd(1)]]).

yes
?-

@end{verbatim} 
@item  byte_xor/3
@begin{verbatim}
?- byte_xor([hexd(a),hexd(1)],[hexd(0),hexd(7)],C).

C = [hexd(a),hexd(6)] ? ;

no
?- byte_xor([bind(1),bind(1),bind(0),bind(1),
    bind(0),bind(0),bind(1),bind(1)],
    [bind(1),bind(1),bind(0),bind(1),
    bind(1),bind(0),bind(1),bind(1)],C).

C = [bind(0),bind(0),bind(0),bind(0),
    bind(1),bind(0),bind(0),bind(0)] ? 

yes
?- 
@end{verbatim}
@end{enumerate}

").
% Define a binary digit type.
:- prop bind(B) # "Dado un elemento @var{B} determina si es un digito binario.".
:-doc(bind/1," @includedef{bind/1}").
bind(0).
bind(1).



% Define a hex digit (nibble) type.
:- prop hexd(H) # "Dado un elemento @var{H} determina si es un digito hexadecimal.".
:-doc(hexd/1," @includedef{hexd/1}").
hexd(0).
hexd(1).
hexd(2).
hexd(3).
hexd(4).
hexd(5).
hexd(6).
hexd(7).
hexd(8).
hexd(9).
hexd(a).
hexd(b).
hexd(c).
hexd(d).
hexd(e).
hexd(f).


% Define a binary byte as a list of 8 binary digits.
:- pred binary_byte(B) # "@var{B}: lista a comprobar si forma un byte binario.".
:-doc(binary_byte/1," @includedef{binary_byte/1}").

binary_byte([bind(A), bind(B), bind(C), bind(D), bind(E),
bind(F), bind(G), bind(H)]):-
    bind(A),
    bind(B),
    bind(C),
    bind(D),
    bind(E),
    bind(F),
    bind(G),
    bind(H).

% Define a hex byte as a list of 2 hex nibbles.

:- pred hex_byte(H) # "@var{H}: lista a comprobar si forma un byte hexadecimal.".
:-doc(hex_byte/1," @includedef{hex_byte/1}").
hex_byte([hexd(H1), hexd(H0)]) :-
    hexd(H1),
    hexd(H0).

:- prop hextobind(H,R) #"Dado un valor hexadecimal @var{H} devuelve su valor @var{R} binario asociado.".
:- doc(hextobind/2,"@includedef{hextobind/2}").

hextobind(hexd(0),[bind(0),bind(0),bind(0),bind(0)]).
hextobind(hexd(1),[bind(0),bind(0),bind(0),bind(1)]).
hextobind(hexd(2),[bind(0),bind(0),bind(1),bind(0)]).
hextobind(hexd(3),[bind(0),bind(0),bind(1),bind(1)]).
hextobind(hexd(4),[bind(0),bind(1),bind(0),bind(0)]).
hextobind(hexd(5),[bind(0),bind(1),bind(0),bind(1)]).
hextobind(hexd(6),[bind(0),bind(1),bind(1),bind(0)]).
hextobind(hexd(7),[bind(0),bind(1),bind(1),bind(1)]).
hextobind(hexd(8),[bind(1),bind(0),bind(0),bind(0)]).
hextobind(hexd(9),[bind(1),bind(0),bind(0),bind(1)]).
hextobind(hexd(a),[bind(1),bind(0),bind(1),bind(0)]).
hextobind(hexd(b),[bind(1),bind(0),bind(1),bind(1)]).
hextobind(hexd(c),[bind(1),bind(1),bind(0),bind(0)]).
hextobind(hexd(d),[bind(1),bind(1),bind(0),bind(1)]).
hextobind(hexd(e),[bind(1),bind(1),bind(1),bind(0)]).
hextobind(hexd(f),[bind(1),bind(1),bind(1),bind(1)]).


%Ej 1

:- pred byte_list(L) # "@var{L}: lista a comprobar si esta formada por bytes binarios o hexadecimales.".
:-doc(byte_list/1,"Predicado 1:
    Extrae elementos de la lista, comprueba si son bytes binarios o hexadecimales y se hace una llamada recursiva al predicado con el resto de la lista. El caso base es que la lista este vacia.@includedef{byte_list/1}").

byte_list([]).
byte_list([A|L]) :- binary_byte(A),byte_list(L).
byte_list([A|L]) :- hex_byte(A),byte_list(L).




%Ej 2


:- pred byte_conversion(H,R) #"@var{H}: byte en hexadecimal, @var{R}: byte en binario.".
:-doc(byte_conversion/2,"Predicado 2:
    Comprueba que el primer argumento es un byte hexadecimal y despues lo convierte a un byte binario.@includedef{byte_conversion/2}").

byte_conversion([X,Y],R):- hextobind(X,L1), hextobind(Y,L2), append(L1,L2,R). 






%Ej 3
:-pred byte_list_conversion(HL,BL) #"@var{HL}: lista de bytes hexadecimales, @var{BL}: lista  de bytes binarios.".
:-doc(byte_list_conversion/2,"Predicado 3:
    Recorre los elementos de la lista convirtiendo el primer elemento de hexadecimal a binario y llama recursivamente al predicado con el resto de la lista hasta que esta sea vacia.@includedef{byte_list_conversion/2}").

byte_list_conversion([],[]). 
byte_list_conversion([X|Xs],[L|R]):- byte_conversion(X,L), byte_list_conversion(Xs,R).

%Ej 4
:-pred get_nth_bit_from_byte(N,B,BN) #"@var{N}: posicion del bit, @var{B}; byte del que extraer el bit, @var{L}: valor del bit.".
:-doc(get_nth_bit_from_byte/3,"Predicado 4:
    Comprueba que el segundo argumento es un byte, si es hexadecimal lo convierte a binario llamando al predicado 2, a continuacion invierte la lista de bits y llama al predicado aux4/3 para buscar el elemento de la posicion que se indica en el primer argumento en la lista.@includedef{get_nth_bit_from_byte/3}").

get_nth_bit_from_byte(N,B,NB):- binary_byte(B), aux4(N,B,NB).
get_nth_bit_from_byte(N,B,NB):- hex_byte(B), byte_conversion(B,L), invert(L,R),aux4(N,R,NB).


%Ej 5
:-pred byte_list_clsh(L,CLShL) #"@var{L}: lista de bytes, @var{CLShL}: lista con un bit rotado a la izquierda.".
:-doc(byte_list_clsh/2,"Predicado 5: 
    Comprueba que el primer argumento es una lista de bytes binarios o hexadecimales, si es una lista de hexadecimales los convierte a binario llamando al predicado 3. A continuacion une las listas de bytes en una lista de bits, llamando a byte_to_bit/2. Posteriormente se mueve el primer elemento de la lista a la ultima posicion.Por ultimo transforman los bits en bytes usando de nuevo el predicado byte_to_bit/2 y si es necesario se convierte el resultado a hexadecimal.@includedef{byte_list_clsh/2}").

byte_list_clsh(L,CLShL) :- hex_byte_list(L), byte_list_conversion(L,R),byte_to_bit(R,[B1|Bits]), append(Bits,[B1],R1), byte_to_bit(Bytes,R1),byte_list_conversion(CLShL,Bytes).
byte_list_clsh(L,CLShL) :- binary_byte_list(L), byte_to_bit(L,[B1|Bits]), append(Bits,[B1],D), byte_to_bit(CLShL,D).




%Ej 6
:-pred byte_list_crsh(L,CRShL) #"@var{L}: lista de bytes, @var{CRShL}: lista con un bit rotado a la derecha.".
:-doc(byte_list_crsh/2,"Predicado 6:
    Comprueba que el primer argumento es una lista de bytes binarios o hexadecimales, si es una lista de hexadecimales los convierte a binario llamando al predicado 3. A continuacion une las listas de bytes en una lista de bits, llamando a byte_to_bit/2. Se invierte la lista llmando a invert/2 y posteriormente se mueve el primer elemento de la lista a la ultima posicion. Por ultimo se invierte la lista de nuevo y se transforman los bits en bytes usando de nuevo el predicado byte_to_bit/2 y si es necesario se convierte el resultado a hexadecimal.@includedef{byte_list_crsh/2}").

byte_list_crsh(L,CRShL) :- hex_byte_list(L), byte_list_conversion(L,R),byte_to_bit(R,I),invert(I,[B1|Bits]), append(Bits,[B1],I1),invert(I1,R1), byte_to_bit(Bytes,R1),byte_list_conversion(CRShL,Bytes).
byte_list_crsh(L,CRShL) :- binary_byte_list(L), byte_to_bit(L,I), invert(I,[B1|Bits]), append(Bits,[B1],I1),invert(I1,D), byte_to_bit(CRShL,D).


%Ej 7
:-pred byte_xor(B1,B2,B3) #"@var{B1}: primer operando, @var{B2}: segundo operando, @var{B3}: resultado del xor.".
:-doc(byte_xor/3,"Predicado 7: 
    Comprueba que los dos primeros operados son bytes, si son hexadecimales los transforma a binario llamando al predicado 2. Llama al predicadoxor_list/3 para calcular la operacion xor, por ultimo, en caso de ser necesario, transforma el resultado binario en hexadecimal.@includedef{byte_xor/3}").
byte_xor(A,B,C) :- binary_byte(A), binary_byte(B), binary_byte(C),xor_list(A,B,C).
byte_xor(A,B,C) :- hex_byte(A), hex_byte(B), byte_conversion(A,AB),byte_conversion(B,BB), xor_list(AB,BB,CB),byte_conversion(C,CB).




%append: append/3
% Concatena dos listas
append([],Ys ,Ys) :- list(Ys).
append([X|Xs],Ys ,[X|Zs]) :- append(Xs ,Ys ,Zs).

:-pred append(X,Y,Z) #"@var{Z} es el resultado de concatenar @var{X} y @var{Y}".
:-doc(append/3," Anade el primer elemento de la primera lista y lo pone al inicio de la lista resultado, a continuacion se llama de forma recursiva a la misma funcion con parametros el resto de la primera lista, la segunda lista entera y la lista resultado. El caso base es que la primera lista este vacia, en ese caso la lista resultado es la segunda lista.@includedef{append/3}").

%invert: invert/2
% Invierte una lista
invert([],[]).
invert([X|L],R):-invert(L,L1),append(L1,[X],R).
:-pred invert(L,R) #"@var{R} es el resultado de invertir @var{L}".
:-doc(invert/2,"Se extrae el primer elemento de la lista y se hace una llamada recursiva, a continuacion la lista obtenida como resultadose concatena con el elemento extraido. El caso base son las dos listas vacias.@includedef{invert/2}  ").


:-pred aux4(N,B,BN) #"@var{N}: posicion del bit, @var{B}: byte del que extraer el bit, @var{L}: valor del bit.".
:-doc(aux4/3,"Reduce en uno el valor del numero del primer argumento, descarta el primer bit de la lista y a continuacion se vuelve a llamr de forma recursiva. El caso base es el primer argumento con valor 0, en esta situacion se equipara el primer elemento de la lista de bits con el tercer argumento.@includedef{aux4/3} ").
aux4(s(N),[_|A],B):- aux4(N,A,B).
aux4(0,[X|_],X).
:-pred hex_byte_list(L) #"@var{L}: lista a comprobar si esta formada por bytes hexadecimales.".
:-doc(hex_byte_list/1,"Extrae elementos de la lista, comprueba si son bytes hexadecimales y se hace una llamada recursiva al predicado con el resto de la lista. El caso base es que la lista este vacia.@includedef{hex_byte_list/1} ").
hex_byte_list([]).
hex_byte_list([A|L]) :- hex_byte(A),hex_byte_list(L).

:-pred binary_byte_list(L) #"@var{L}: lista a comprobar si esta formada por bytes binarios.".
:-doc(binary_byte_list/1,"Extrae elementos de la lista, comprueba si son bytes binarios y se hace una llamada recursiva al predicado con el resto de la lista. El caso base es que la lista este vacia.@includedef{binary_byte_list/1} ").
binary_byte_list([]).
binary_byte_list([A|L]) :- binary_byte(A),binary_byte_list(L).


:-pred byte_to_bit(Bytes,Bits) #"@var{Bytes}: Lista de bytes, @var{Bits}: Lista de bits.".
:-doc(byte_to_bit/2,"Transforma una lista de bytes en una unica lista de bits o viceversa.@includedef{byte_to_bit/2} ").

byte_to_bit([],[]).
byte_to_bit([[A,B,C,D,E,F,G,H]|By],[A,B,C,D,E,F,G,H|Bi]) :-
    byte_to_bit(By,Bi).

:-pred xor_list(A,B,C) #"@var{A}: Lista de bytes del primer operando, @var{B}: Lista de bytes del segundo operando, @var{C}: lista de bytes del resultado.".
:-doc(xor_list/3,"Extrae elementos de las dos primeras listas, realiza la operacion xor sobre ellos, se almacena el resultado en el tercer argumento y se llama recursivamente a la funcion. El caso base es las una llamada con las tres listas vacias.@includedef{xor_list/3} ").
xor_list([E1|A],[E2|B],C) :- xor(E1, E2, R), xor_list(A,B,Acc), append([R],Acc,C).

xor_list([],[],[]).

:- prop xor(A,B,C) #"Dado un bit  @var{A} y un bit @var{B} el resultado de la operacion xor en @var{C}.".
:- doc(xor/3,"@includedef{xor/3}").
xor(bind(1),bind(1),bind(0)).
xor(bind(1),bind(0),bind(1)).
xor(bind(0),bind(1),bind(1)). 
xor(bind(0),bind(0),bind(0)).
