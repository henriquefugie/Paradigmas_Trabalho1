saltosCavalo(Tamanho, Linha, Coluna) :-
    get_time(Inicio),
    obterSolucao(Tamanho, [Linha, Coluna], [[Linha, Coluna]], Caminho, []),
    imprimirCaminho(Tamanho, Caminho),
    get_time(Fim),
    TempoSegundos is Fim - Inicio,
    format('Tempo decorrido: ~3f segundos', [TempoSegundos]).

obterSolucao(Tamanho, [X, Y], Caminho, Solucao, Visitados) :-
    length(Caminho, L),
    L =:= (Tamanho * Tamanho),
    reverse(Caminho, Solucao).

obterSolucao(Tamanho, [X, Y], Caminho, Solucao, Visitados) :-
    length(Caminho, L),
    L < (Tamanho * Tamanho),
    fazerMovimento(Tamanho, [X, Y], [ProximoX, ProximoY]),
    \+ member([ProximoX, ProximoY], Visitados),
    obterSolucao(Tamanho, [ProximoX, ProximoY], [[ProximoX, ProximoY] | Caminho], Solucao, [[ProximoX, ProximoY] | Visitados]).

fazerMovimento(Tamanho, [X, Y], [ProximoX, ProximoY]) :-
    movimentoPossivel([X, Y], [ProximoX, ProximoY]),
    ProximoX >= 1,
    ProximoX =< Tamanho,
    ProximoY >= 1,
    ProximoY =< Tamanho.

imprimirCaminho(Tamanho, Caminho) :-
    criarMatrizVazia(Tamanho, Matriz),
    preencherMatriz(Caminho, Matriz, 1),
    imprimirMatriz(Matriz).

imprimirMatriz([]).
imprimirMatriz([Linha | Resto]) :-
    write('| '),
    imprimirLinha(Linha),
    nl,
    imprimirMatriz(Resto).

imprimirLinha([]) :- write('|').
imprimirLinha([Celula | Resto]) :-
    format('~|~t~d~4+ ', [Celula]),
    imprimirLinha(Resto).

criarMatriz(Tamanho, Caminho, Matriz) :-
    criarMatrizVazia(Tamanho, Matriz),
    preencherMatriz(Caminho, Matriz, 1).

criarMatrizVazia(Tamanho, Matriz) :-
    length(Matriz, Tamanho),
    criarLinhasVazias(Tamanho, Matriz).

criarLinhasVazias(_, []).
criarLinhasVazias(Tamanho, [Linha|Resto]) :-
    length(Linha, Tamanho),
    criarLinhasVazias(Tamanho, Resto).

preencherMatriz([], _, _).
preencherMatriz([[X, Y]|Resto], Matriz, N) :-
    nth1(X, Matriz, Linha),
    nth1(Y, Linha, N),
    N1 is N + 1,
    preencherMatriz(Resto, Matriz, N1).

movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X + 2, ProximoY is Y + 1.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X + 1, ProximoY is Y + 2.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X - 1, ProximoY is Y + 2.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X - 2, ProximoY is Y + 1.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X - 2, ProximoY is Y - 1.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X - 1, ProximoY is Y - 2.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X + 1, ProximoY is Y - 2.
movimentoPossivel([X, Y], [ProximoX, ProximoY]) :- ProximoX is X + 2, ProximoY is Y - 1.
