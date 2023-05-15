% Parte 2

% Joao Vitor e Henrique

% ----------------------- FATOS -----------------------

% forte_contra(X,Y) onde Y eh forte contra X
forte_contra(darius, [fiora,teemo]).
forte_contra(ashe, [janna,twitch]).
forte_contra(ekko, [maokai,darius]).
forte_contra(talon, [cassiopeia,ekko]).
forte_contra(janna, [twitch,milio]).
forte_contra(sett, [darius,teemo]).
forte_contra(kayn, [hecarim,vi]).
forte_contra(ahri, [cassiopeia,zed]).
forte_contra(jinx, [jhin,ezreal]).
forte_contra(nautilus, [lulu,janna]).

% sinergia(X,Y) onde X tem sinergia com Y
sinergia_top(kayn, [fiora,darius]).
sinergia_top(ahri, [fiora,teemo]).
sinergia_top(jhin, [darius,teemo]).
sinergia_top(janna, [fiora,teemo]).

sinergia_jg(darius, [hecarim, ekko]).
sinergia_jg(ahri, [maokai, ekko]).
sinergia_jg(jhin, [maokai, hecarim]).
sinergia_jg(janna, [hecarim, ekko]).

sinergia_mid(darius, [talon, cassiopeia]).
sinergia_mid(kayn, [zed, yasuo]).
sinergia_mid(jhin, [talon, cassiopeia]).
sinergia_mid(janna, [talon, yasuo]).

sinergia_adc(darius, [jinx, twitch]).
sinergia_adc(ahri, [ashe, jinx]).
sinergia_adc(kayn, [jhin, twitch]).
sinergia_adc(janna, [ashe, jinx]).

sinergia_sup(darius, [milio, thresh]).
sinergia_sup(ahri, [thresh, janna]).
sinergia_sup(jhin, [milio, thresh]).
sinergia_sup(kayn, [nautilus, janna]).

% campeao(Campeao, Lane, Alcance, TipoDeDano, Build, OrdemSkill)
campeao(darius, top, corpo, ad, [trindade, danca, couraca, natureza, sterak],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(fiora, top, corpo, ad, [ruptor, hidra, danca, sterak, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(teemo, top, distancia, ap, [liandry, abraco, dente, morello, zhonya],
[e,q,w,e,e,r,e,q,e,q,r,q,q,w,w,r,w,w]).
campeao(gnar, top, distancia, ad, [trindade, cutelo, natureza, randuin, espinhos],
[q,w,e,q,q,r,q,w,q,w,r,w,w,e,e,r,e,e]).
campeao(sett, top, corpo, ad, [ruptor, hidra, danca, sterak, shiojin],
[q,w,e,q,q,r,q,w,q,w,r,w,w,e,e,r,e,e]).

campeao(kayn, jg, corpo, ad, [eclipse, cutelo, shiojin, manamune, sterak],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(hecarim, jg, corpo, ad, [eclipse, cutelo, shiojin, manamune, sterak],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(vi, jg, corpo, ad, [eclipse, cutelo, shiojin, manamune, sterak],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(ekko, jg, corpo, ap, [liandry, abraco, dente, morello, zhonya],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(maokai, jg, corpo, ap, [liandry, abraco, dente, morello, zhonya],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).

campeao(ahri, mid, distancia, ap, [liandry, abraco, dente, morello, zhonya],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(yasuo, mid, corpo, ad, [ruptor, hidra, danca, sterak, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(talon, mid, corpo, ad, [trindade, danca, couraca, natureza, sterak],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(cassiopeia, mid, distancia, ap, [liandry, abraco, dente, morello, zhonya],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(zed, mid, corpo, ad, [ruptor, hidra, danca, sterak, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).

campeao(ashe, adc, distancia, ad, [vendaval, runaan, gume, shiojin, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(twitch, adc, distancia, ad, [vendaval, runaan, gume, shiojin, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(jinx, adc, distancia, ad, [vendaval, runaan, gume, shiojin, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(ezreal, adc, distancia, ad, [vendaval, runaan, gume, shiojin, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(jhin, adc, distancia, ad, [vendaval, runaan, gume, shiojin, shiojin],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).

campeao(milio, sup, distancia, ap, [medalhao, convergencia, espinhos, juramento, natureza],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(janna, sup, distancia, ap, [medalhao, convergencia, espinhos, juramento, natureza],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(thresh, sup, distancia, ap, [medalhao, convergencia, espinhos, juramento, natureza],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(lulu, sup, distancia, ap, [medalhao, convergencia, espinhos, juramento, natureza],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).
campeao(nautilus, sup, corpo, ap, [medalhao, convergencia, espinhos, juramento, natureza],
[q,w,e,q,q,r,q,e,q,e,r,e,e,w,w,r,w,w]).

item(eclipse) :- write('Eclipse').
item(manamune) :- write('Manamune').
item(vendaval) :- write('Forca do Vendaval').
item(runaan) :- write('Furacao de Runaan').
item(gume) :- write('Gume do Infinito').
item(medalhao) :- write('Medalha dos Solari de Ferro').
item(juramento) :- write('Juramento do Cavaleiro').
item(convergencia) :- write('Convergencia de Zeke').
item(ruptor) :- write('Ruptor Divino').
item(espinhos) :- write('Armadura de Espinhos').
item(randuin) :- write('Pressagio de Randuin').
item(cutelo) :- write('Cutelo Negro').
item(zhonya) :- write('Ampulheta de Zhonya').
item(morello) :- write('Morellonomicon').
item(dente) :- write('Dente de Nashor').
item(abraco) :- write('Abraco Demoniaco').
item(liandry) :- write('Angustia de Liandry').
item(shiojin) :- write('Lanca de Shiojin').
item(hidra) :- write('Hidra Raivosa').
item(danca) :- write('Danca da Morte').
item(trindade) :- write('Forca da Trindade').
item(cimitarra) :- write('Cimitarra Mercurial').
item(couraca) :- write('Couraca do Defunto').
item(semblante) :- write('Semblante Espiritual').
item(natureza) :- write('Forca da Natureza').
item(sterak) :- write('Sinal de Sterak').
item(jaksho) :- write('JakSho').

itens(eclipse).
itens(manamune).
itens(vendaval).
itens(runaan).
itens(gume).
itens(medalhao).
itens(juramento).
itens(convergencia).
itens(ruptor).
itens(espinhos).
itens(randuin).
itens(cutelo).
itens(zhonya).
itens(morello).
itens(dente).
itens(abraco).
itens(liandry).
itens(shiojin).
itens(hidra).
itens(danca).
itens(trindade).
itens(cimitarra).
itens(couraca).
itens(semblante).
itens(natureza).
itens(sterak).
itens(jaksho).

% -----------------------------------------------------

% ----------------------- REGRAS -----------------------

% Retorna a lane de um campeao X
lane_campeao(X) :- campeao(X,Y,_,_,_,_), write(Y), nl.

% Retorna o alcance de um campeao X
alcance_campeao(X) :- campeao(X,_,Y,_,_,_), write(Y), nl.

% Retorna o tipo de dano de um campeao X
tipo_campeao(X) :- campeao(X,_,_,Y,_,_), write(Y), nl.

% Retorna a build de um campeao X
itens_campeao(X) :- campeao(X,_,_,_,Y,_), informaItemLista(Y), nl.

% Retorna a ordem de habilidades de um campeao X
habilidades_campeao(X) :- campeao(X,_,_,_,_,Y), write(Y), nl.

% Mostra os dados principais do campeao contidos no fato campeao()
dados_campeao(X) :-
write('---------------'), nl,
write('Campeao: '),
write(X), nl,
write('Lane: '), lane_campeao(X),
write('Alcance: '), alcance_campeao(X),
write('Tipo de dano: '), tipo_campeao(X),
write('Itens recomendados: '), itens_campeao(X),
write('---------------').

% Recebe uma lista de campeoes e mostra os dados de cada campeao
informacaoCampeaoLista([]).
informacaoCampeaoLista([H|T]) :- dados_campeao(H), nl, informacaoCampeaoLista(T).

% Recebe uma lista de itens e formata essa lista para mostrar cada item para o usuario
informaItemLista([]).
informaItemLista([H|T]) :- length(T,Valor),
(Valor >= 1 -> item(H), write(', '), informaItemLista(T) ;
item(H), informaItemLista(T)).

% Recebe uma lista de campeoes e formata essa lista para mostrar cada campeao para o usuario
nomeCampeaoLista([]).
nomeCampeaoLista([H|T]) :- write(H), length(T,Valor),
(Valor >= 1 -> write(', '), nomeCampeaoLista(T) ;
nomeCampeaoLista(T)).

% Mostra todos os dados contidos na base de dados relacionados a um campeao X
dadosCompletosCampeao(X) :-
dados_campeao(X), nl, nl, nl,
mostra_forte_contra(X),
campeao(X,Lane,_,_,_,_), nl, nl,
sinergia_total(X, Lane).

% Recebe um capeao X e mostra campeoes fortes contra o campeao X
mostra_forte_contra(X) :-
write('Campeoes forte contra '),
write(X), nl,
forte_contra(X,Y),
informacaoCampeaoLista(Y).

% Mostra os campeoes da lane top que tem alguma vantagem com um campeao X
mostra_sinergia_top(X) :-
write('Campeoes top que tem sinergia com '),
write(X), nl,
sinergia_top(X,A),
nomeCampeaoLista(A), nl.

% Mostra os campeoes da lane jg que tem alguma vantagem com um campeao X
mostra_sinergia_jg(X) :-
write('Campeoes jungle que tem sinergia com '),
write(X), nl,
sinergia_jg(X,A),
nomeCampeaoLista(A), nl.

% Mostra os campeoes da lane mid que tem alguma vantagem com um campeao X
mostra_sinergia_mid(X) :-
write('Campeoes mid que tem sinergia com '),
write(X), nl,
sinergia_mid(X,A),
nomeCampeaoLista(A), nl.

% Mostra os campeoes da lane adc que tem alguma vantagem com um campeao X
mostra_sinergia_adc(X) :-
write('Campeoes adc que tem sinergia com '),
write(X), nl,
sinergia_adc(X,A),
nomeCampeaoLista(A), nl.

% Mostra os campeoes da lane sup que tem alguma vantagem com um campeao X
mostra_sinergia_sup(X) :-
write('Campeoes sup que tem sinergia com '),
write(X), nl,
sinergia_sup(X,A),
nomeCampeaoLista(A), nl.

% Filtra todos os campeoes baseados em um determinado tipo de dano
filtra_tipo(X,Y) :- campeao(X,_,_,Y,_,_).

% Filtra todos os campeoes baseados em uma determinada lane
filtra_lane(X,Y) :- campeao(X,Y,_,_,_,_).

% Filtra todos os campeoes baseados em seu alcance
filtra_alcance(X,Y) :- campeao(X,_,Y,_,_,_).

% Mostra os campeoes de todas as lanes que oferencem algum tipo de vantagem a
% um campeao X
sinergia_total(X, Lane) :-
    write('Campeoes com sinergia com '),
    write(X), nl,
    ((Lane = top,
    findall(Y, sinergia_jg(X,Y), Jg),
    findall(Y, sinergia_mid(X,Y), Mid),
    findall(Y, sinergia_adc(X,Y), Adc),
    findall(Y, sinergia_sup(X,Y), Sup),
    append(Jg, Mid, Aux1),
    append(Adc, Sup, Aux2),
    append(Aux1, Aux2, Matriz),
    append(Matriz, Lista),
    list_to_set(Lista, ListaSemRepeticao),
    informacaoCampeaoLista(ListaSemRepeticao));
    (Lane = jg,
    findall(Y, sinergia_top(X,Y), Top),
    findall(Y, sinergia_mid(X,Y), Mid),
    findall(Y, sinergia_adc(X,Y), Adc),
    findall(Y, sinergia_sup(X,Y), Sup),
    append(Top, Mid, Aux1),
    append(Adc, Sup, Aux2),
    append(Aux1, Aux2, Matriz),
    append(Matriz, Lista),
    list_to_set(Lista, ListaSemRepeticao),
    informacaoCampeaoLista(ListaSemRepeticao));
    (Lane = mid,
    findall(Y, sinergia_top(X,Y), Top),
    findall(Y, sinergia_jg(X,Y), Jg),
    findall(Y, sinergia_adc(X,Y), Adc),
    findall(Y, sinergia_sup(X,Y), Sup),
    append(Top, Jg, Aux1),
    append(Adc, Sup, Aux2),
    append(Aux1, Aux2, Matriz),
    append(Matriz, Lista),
    list_to_set(Lista, ListaSemRepeticao),
    informacaoCampeaoLista(ListaSemRepeticao));
    (Lane = adc,
    findall(Y, sinergia_top(X,Y), Top),
    findall(Y, sinergia_jg(X,Y), Jg),
    findall(Y, sinergia_mid(X,Y), Mid),
    findall(Y, sinergia_sup(X,Y), Sup),
    append(Top, Jg, Aux1),
    append(Mid, Sup, Aux2),
    append(Aux1, Aux2, Matriz),
    append(Matriz, Lista),
    list_to_set(Lista, ListaSemRepeticao),
    informacaoCampeaoLista(ListaSemRepeticao));
    (Lane = sup,
    findall(Y, sinergia_top(X,Y), Top),
    findall(Y, sinergia_jg(X,Y), Jg),
    findall(Y, sinergia_mid(X,Y), Mid),
    findall(Y, sinergia_adc(X,Y), Adc),
    append(Top, Jg, Aux1),
    append(Mid, Adc, Aux2),
    append(Aux1, Aux2, Matriz),
    append(Matriz, Lista),
    list_to_set(Lista, ListaSemRepeticao),
    informacaoCampeaoLista(ListaSemRepeticao))).
    
% Filtra os campeoes de uma determinada lane com um determinado alcance
filtra_lane_alcance(X,Y,Resultado) :- findall(Z, filtra_lane(Z,X), LISTA_TOP),
                            findall(Z, filtra_alcance(Z,Y), LISTA_ALCANCE),
                            intersection(LISTA_TOP, LISTA_ALCANCE, Resultado).

% Filtra os campeoes de uma determinada lane com um determinado tipo de dano
filtra_lane_tipo(X,Y,Resultado) :- findall(Z, filtra_lane(Z,X), LISTA_TOP),
                         findall(Z, filtra_tipo(Z,Y), LISTA_TIPO),
                         intersection(LISTA_TOP, LISTA_TIPO, Resultado).

% Filtra os campeoes com um determinado alcance e um determinado tipo de dano
filtra_alcance_tipo(X,Y,Resultado) :- findall(Z, filtra_alcance(Z,X), LISTA_ALCANCE),
                         findall(Z, filtra_tipo(Z,Y), LISTA_TIPO),
                         intersection(LISTA_ALCANCE, LISTA_TIPO, Resultado).

% Filtra os campeoes de uma determinada lane com um determinado alcance e tipo de dano
filtra_lane_alcance_tipo(X,Y,Z,Resultado) :- filtra_lane_alcance(X,Y,LISTA_ALCANCE_LANE),
                                             filtra_lane_tipo(X,Z,LISTA_TIPO_LANE),
                                             intersection(LISTA_ALCANCE_LANE, LISTA_TIPO_LANE, Resultado).

% Mostra o item mitico a ser feito com um campeao
itemMitico(Campeao) :-
    campeao(Campeao, _, _, _, [PrimeiroItem | _], _),
    !,
    item(PrimeiroItem).

% Mostra os items principais a serem feitos com um campeao
buildprincipal(Campeao) :-
    campeao(Campeao, _, _, _, Itens, _),
    mostrarItens(Itens, 1).

mostrarItens([], _).
mostrarItens([Item | Resto], Contador) :-
    Contador =< 3,
    write('Item '), write(Contador), write(': '), item(Item), nl,
    NovoContador is Contador + 1,
    !,
    mostrarItens(Resto, NovoContador).

% Busca todos os campeoes que fazem um determinado item especifico
campeoes_com_item(_, []).
campeoes_com_item(Item) :- verifica_campeoes_com_item(Item,Campeoes),
                        nomeCampeaoLista(Campeoes).

verifica_campeoes_com_item(Item,Campeoes) :-
  findall(Campeao, (campeao(Campeao, _, _, _, Build, _), member(Item, Build)), Campeoes).

todos_campeoes :-
    findall(Nome, campeao(Nome, _, _, _, _, _), Campeoes),
    write(Campeoes).
    
todos_itens :-
    findall(Nome, itens(Nome), Itens),
    write(Itens).

% -----------------------------------------------------

% COMANDOS:
% dadosCompletosCampeao(darius).
% filtra_lane_alcance(top,distancia,Resultado).
% filtra_lane_tipo(jg,ad,Resultado).
% filtra_alcance_tipo(distancia,ap,Resultado).
% filtra_lane_alcance_tipo(top,distancia,ap,Resultado).
% itemMitico(darius).
% buildprincipal(darius).
% campeoes_com_item(trindade).
