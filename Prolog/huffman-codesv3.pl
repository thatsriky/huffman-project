/* Riccardo Piatti 909687
   -*- Mode: Prolog -*-
   huffman-codes.pl
*/

% -------------------------------
% DECODIFICA UNA SEQUENZA DI BIT
% -------------------------------

hucodec_decode([], leaf(Symbol, _), [Symbol]).
hucodec_decode([], _, _) :- fail.
hucodec_decode(Bits, Tree, Message) :-
    decode_bits(Bits, Tree, Tree, Message).

decode_bits([], leaf(Symbol, _), _, [Symbol]).
decode_bits([], _, _, _) :- fail.  % Fallisce se ci sono bit in eccesso
decode_bits([Bit|Bits], Tree, node(L, R, _, _), Message) :-
    (Bit = 0 -> NewNode = L ; NewNode = R),
    (NewNode = leaf(Symbol, _) ->
        Message = [Symbol|RestMessage],
        decode_bits(Bits, Tree, Tree, RestMessage)
    ;
        decode_bits(Bits, Tree, NewNode, Message)
    ).

% -------------------------------
% CODIFICA UNA SEQUENZA DI SIMBOLI
% -------------------------------

hucodec_encode([], _, []).
hucodec_encode([Symbol], leaf(Symbol, _), []).
hucodec_encode([Symbol|Message], Tree, Bits) :-
    hucodec_generate_symbol_bits_table(Tree, SymbolBitsTable),
    member(sb(Symbol, SymbolBits), SymbolBitsTable),
    hucodec_encode(Message, Tree, RestBits),
    append(SymbolBits, RestBits, Bits).

% -------------------------------
% GENERA UN ALBERO DI HUFFMAN
% -------------------------------

hucodec_generate_huffman_tree([], _) :- fail.
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    make_leaf_set(SymbolsWeights, LeafSet),
    sort_by_weight(LeafSet, SortedLeafSet),
    successive_merge(SortedLeafSet, HuffmanTree).

make_leaf_set([], []).
make_leaf_set([sw(S, W)|Rest], [leaf(S, W)|LeafSet]) :-
    make_leaf_set(Rest, LeafSet).

sort_by_weight(List, Sorted) :-
    predsort(compare_weights, List, Sorted).

compare_weights(Order, T1, T2) :-
    weight(T1, W1),
    weight(T2, W2),
    (W1 < W2 -> Order = '<' ;
     W1 > W2 -> Order = '>' ;
                Order = '=').

successive_merge([Tree], Tree).
successive_merge([T1, T2|Rest], HuffmanTree) :-
    merge_nodes(T1, T2, MergedNode),
    insert_sorted(MergedNode, Rest, NewSet),
    successive_merge(NewSet, HuffmanTree).

merge_nodes(Node1, Node2, node(Node1, Node2, MergedSymbols, MergedWeight)) :-
    weight(Node1, W1),
    weight(Node2, W2),
    MergedWeight is W1 + W2,
    symbols(Node1, S1),
    symbols(Node2, S2),
    append(S1, S2, MergedSymbols).

insert_sorted(Tree, [], [Tree]).
insert_sorted(Tree, [T|Ts], [Tree, T|Ts]) :-
    weight(Tree, W1),
    weight(T, W2),
    W1 =< W2, !.
insert_sorted(Tree, [T|Ts], [T|NewTs]) :-
    insert_sorted(Tree, Ts, NewTs).

% -------------------------------
% GENERA UNA TABELLA SIMBOLO-BIT
% -------------------------------

hucodec_generate_symbol_bits_table(Tree, Table) :-
    traverse_tree(Tree, [], Table).

traverse_tree(leaf(S, _), Code, [sb(S, Code)]).
traverse_tree(node(L, R, _, _), Code, Table) :-
    append(Code, [0], LeftCode),
    append(Code, [1], RightCode),
    traverse_tree(L, LeftCode, LeftTable),
    traverse_tree(R, RightCode, RightTable),
    append(LeftTable, RightTable, Table).

% -------------------------------
% STAMPA L'ALBERO DI HUFFMAN
% -------------------------------

hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, 0).

print_tree(leaf(S, W), Indent) :-
    tab(Indent), write('Leaf: '), write(S), write(' ('), write(W), write(')'), nl.
print_tree(node(L, R, Symbols, W), Indent) :-
    tab(Indent), write('[NODE] ('), write(W), write(')'), nl,
    tab(Indent), write('Symbols: '), write(Symbols), nl,
    Indent1 is Indent + 2,
    print_tree(L, Indent1),
    print_tree(R, Indent1).

% -------------------------------
% LETTURA DI UN FILE E CODIFICA
% -------------------------------

hucodec_encode_file(Filename, Tree, Bits) :-
    open(Filename, read, Stream),
    read_file_symbols(Stream, Message),
    close(Stream),
    hucodec_encode(Message, Tree, Bits).

read_file_symbols(Stream, []) :-
    at_end_of_stream(Stream).
read_file_symbols(Stream, [Symbol|Symbols]) :-
    read(Stream, Symbol),  % Legge simboli interi, inclusi spazi e newline
    read_file_symbols(Stream, Symbols).

% -------------------------------
% FUNZIONI AUSILIARIE
% -------------------------------

weight(leaf(_, W), W).
weight(node(_, _, _, W), W).

symbols(leaf(S, _), [S]).
symbols(node(_, _, Symbols, _), Symbols).
