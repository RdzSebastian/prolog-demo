% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% Your program goes here

% Libreria

escribio (elsaBornemann, socorro).
escribio (neilGaiman, sandman).
escribio (alanMoore, watchmen).
escribio (neilGaiman, americanGods).
escribio (neilGaiman, buensoPresagios).
escribio (terryPratchett, buensoPresagios).
escribio (brianAzarello, cienBalas).
escribio (warenElis, planetary).
escribio (frankMiller, elCaballeroOscuroRegresa).
escribio (frankMiller, batmanAnioUno).
escribio(isaacAsimov, fundacion).
escribio(isaacAsimov, yoRobot).
escribio(isaacAsimov, elFinDeLaEternidad).
escribio(isaacAsimov, laBusquedaDeLosElementos).
escribio(joseHernandez, martinFierro).
escribio(stephenKing, it).
escribio(stephenKing, misery).
escribio(stephenKing, carrie).
escribio(stephenKing, elJuegoDeGerald).
escribio(julioCortazar, rayuela).
escribio(jorgeLuisBorges, ficciones).
escribio(jorgeLuisBorges, elAleph).
escribio(horacioQuiroga, cuentosDeLaSelva).
escribio(horacioQuiroga, cuentosDeLocuraAmorYMuerte)


%¿Es cierto que alguien escribió una determinada obra?
escribio (alanMoore , watchmen).    🡪 True
escribio (alanMoore, elPadrino).    🡪 False

%Quién o quienes escribieron una obra
escribio (Artista, watchmen).   🡪 alanMoore.
escribio (Artista, buenosPresagios). 🡪 neilGaiman (r ó ;) terryPratchett.

%Qué obra escribió cierta persona
escribio (isaacAsimov, Obra).  🡪 {Varias respuestas}.

%Si es cierto que cierta persona escribió alguna obra, sin importar cual
escribio(jorgeLuisBorges, _).

%Si es cierto que cierta obra existe
escribio(_, it).

%Se puede extender la base de datos
%Es Comic?
esComic(sandman)
esComic(cienBalas)
esComic(watchman)
esComic(planetary)
esComic(elCaballeroOscuroRegresa)
esComic(batmanAnioUno)

%Se trabaja con consultas
esComic(rayuela) 🡪 False (No esta en la BdC)
esComic(_) 🡪 True  (Porque hay comics en la BdC)
esComic(Obra) 🡪 Devuelve una obra.


suma(3,2,5).

numero(40).
numero(_).

siguiente(N,S):- numero(N), S is N+1.

% ejercicio 1
programaEn(maria,cobol).
programaEn(maria,java).
programaEn(mario,cobol).
programaEn(mario,python).
programaEn(jose,cobol).
programaEn(jorge,java).
programaEn(jorge,python).

sonColegas(Persona,Persona2):-
	programaEn(Persona,Lenguajes),
	programaEn(Persona2,Lenguajes),
	Persona \= Persona2.
% Respuestas
% programaEn(_,cobol)
%programaEn(Personas,cobol)
%programaEn(maria,Lenguajes)
%sonColegas(maria,mario)
%sonColegas(P1,P2)

%Ejerecicio 2
esHijoDe(carlosIII,isabelIII).
esHijoDe(ana,isabelIII).
esHijoDe(andrew,isabelIII).
esHijoDe(edward,isabelIII).
esHijoDe(henry,carlosII).
esHijoDe(carlosII,carlosIII).
esHijoDe(oscar,carlosIII).
esHijoDe(sam,ana).

esNietoDe(Nieto,Abuelo) :- 
    esHijoDe(Nieto,Padre),
    esHijoDe(Padre, Abuelo).

sonHermanos(Hermano1,Hermano2):-
    esHijoDe(Hermano1,Padre),
    esHijoDe(Hermano2,Padre),
    Hermano1 \= Hermano2.

esPrimoDe(Primo1,Primo2):-
    esNietoDe(Primo1,Abuelo),
    esNietoDe(Primo2,Abuelo),
    Primo1 \= Primo2,
    not(sonHermanos(Primo1,Primo2)).
    
    

%esHijoDe(_,isabelIII)
%esHijoDe(Hijo,isabelIII)
%Primo1 = sam,  Primo2 = oscar
%Primo1 = sam,Primo2 = carlosII Primo1 = sam,Primo2 = oscar

%ejercicio 3

puedeUsar(num,sum).
puedeUsar(num,resta).
puedeUsar(num,mult).
puedeUsar(fractional,div).
puedeUsar(show,mostrar).

esTypeClass(int,num).
esTypeClass(int,show).
esTypeClass(float,fractional).
esTypeClass(float,show).
esTypeClass(double,fractional).
esTypeClass(double,show).
esTypeClass(bool,show).
esTypeClass(fractional,num).

operacionPermitida(ClaseTipo, Operacion):-
    puedeUsar(ClaseTipo, Operacion);
    esTypeClass(ClaseTipo, TypeClass),
    puedeUsar(TypeClass, Operacion).
	

pertenece(Clase, ClaseTipo):-
    esTypeClass(Clase,ClaseTipo).

%ejercicio 4

amigo(nico, fernando).

%amigo(Uno, Otro).
% Este predicado es completamente inversible porque no hay 
% restricciones en las variables. Puede utilizarse tanto para 
% consultar como para generar soluciones.

%amigo(axel, Persona) :- 
    %Amigo(Persona, nico).
%Este predicado establece que Axel es amigo de cualquier persona 
%que sea amigo de Nico. Esto significa que, dada una persona, 
%Prolog puede generar a Axel como amigo de esa persona, pero no puede 
%generar a esa persona a partir de Axel como amigo. Por lo tanto, este 
%predicado no es completamente inversible.

amigo(alf, _).
%Este predicado establece que Alf es amigo de cualquier 
%cosa (el guion bajo _ en Prolog representa una variable anónima). 
% Aun asi este predicado no es inversible 
% ya que no puede generar soluciones, ni responder a la consulta (alf, Otro)
%solo responde true si le colocas un valor que corresponda a la base de datos

id(X, X).
%Este predicado es completamente inversible,
% ya que puede ser utilizado tanto para consultar como para generar soluciones.
% Cuando se consulta con id(Algo, LoMismo), 
% Prolog unificará Algo y LoMismo con el mismo valor

edad(pedro,20).
edad(juan, 5).
edad(juian, 59).

mayorDeEdad(Persona):- 
    edad(Persona, Edad),
    Edad > 18.
 %Este predicado es inversible porque podes consultar y generar soluciones
% aunque tenga una sola variable 
    
%ejercicio 5

%El siguiente predicado hermano es inversible?

hermano(Uno, Otro) :- 
    padre(Alguien,Uno), 
    padre(Alguien,Otro),
	Uno \= Otro.

%Este predicado es inversible porque puede ser utilizado tanto 
%para consultar como para generar soluciones.

%Consulta: Si se consulta hermano(Uno, Otro), 
%Prolog buscará dos personas Uno y Otro que cumplan con 
%la condición especificada 
%(compartir el mismo padre pero no ser la misma persona).

%Generación de Soluciones: Si se quiere generar soluciones, 
%Prolog utilizará el predicado para encontrar todas las 
%combinaciones de Uno y Otro que cumplan con la condición establecida.

padre(pepe,luis).

es_padre(Padre, Hijo) :-
    esHijjoDe(Hijo,Padre).

%la función es_padre/2 no es completamente 
%inversible, ya que no puede ser utilizada para generar 
%todas las posibles soluciones

%en el caso de que pongas " es_padre(luis, pepe) dara false

%Consulta: es_padre(juan, luis) dará como resultado true, ya que el hecho padre(juan, luis) existe.
%Consulta: es_padre(Padre, luis) dará como resultado Padre = juan, ya que padre(juan, luis) es un hecho en la base de conocimiento y Padre se unificará con juan.
%Consulta: es_padre(juan, Hijo) dará como resultado Hijo = luis, ya que padre(juan, luis) es un hecho en la base de conocimiento y Hijo se unificará con luis.
%Generación de soluciones: Si quieres generar todas las posibles combinaciones de padre e hijo, Prolog podría instanciar Padre con juan y Hijo con luis, ya que es la única combinación que hace que la consulta sea verdadera.

%Como serían los predicados de Primo? 
%Primo seria lo mismo que hermanos y tio a padre


%Ejercicio 6: Dado los siguientes partidos responder:
%Los Equipos africanos hacen muchos goles (goles>2).
%Hubo una goleada (>3) entre dos Equipos?.

partido(islandia,5,croacia,1).
partido(nigeria,3,argentina,1).
partido(croacia,0,argentina,6).
partido(brasil,5,costaRica,0).
partido(brasil,2,croacia,2).
continente(brasil,america).
continente(croacia,europa).
continente(argentina,america).
continente(costarica,america).
continente(islandia,europa).
continente(nigeria,africa).
descalificado(brasil).
descalificado(alemania).

africaHaceMuchosGoles(Equipo):-
    continente(Equipo,africa),
    partido(Equipo,Goles,_,_),
    Goles > 2.

huboUnaGoleada(Equipo1,Equipo2):-
    partido(Equipo1,Goles1,Equipo2,Goles2),
    abs(Goles1 - Goles2) > 3.


    