/* Riccardo Piatti 909687

   -*- Mode: Prolog -*-

   huffman-codes.pl
*/

% Decodes a sequence of bits using a Huffman tree
hucodec_decode([], _, []).
hucodec_decode(Bits, Tree, Message) :-
    decode_bits(Bits, Tree, Tree, Message).

decode_bits([], _, _, []).
decode_bits([Bit|Bits], Tree, node(L, R, _, _), Message) :-
    (Bit = 0 -> NextBranch = L ; NextBranch = R),
    decode_bits(Bits, Tree, NextBranch, Message).
decode_bits(Bits, Tree, leaf(Symbol, _), [Symbol|Rest]) :-
    decode_bits(Bits, Tree, Tree, Rest).

% Encodes a message using a Huffman tree
hucodec_encode([], _, []).
hucodec_encode([Symbol|Message], Tree, Bits) :-
    encode_symbol(Symbol, Tree, SymbolBits),
    hucodec_encode(Message, Tree, RestBits),
    append(SymbolBits, RestBits, Bits).

encode_symbol(Symbol, leaf(Symbol, _), []).
encode_symbol(Symbol, node(L, _, _, _), [0|Bits]) :-
    encode_symbol(Symbol, L, Bits).
encode_symbol(Symbol, node(_, R, _, _), [1|Bits]) :-
    encode_symbol(Symbol, R, Bits).

% Generates a Huffman tree from a list of symbol-weight pairs
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    make_leaf_set(SymbolsWeights, LeafSet),
    successive_merge(LeafSet, HuffmanTree).

make_leaf_set([], []).
make_leaf_set([sw(S, W)|Rest], [leaf(S, W)|LeafSet]) :-
    make_leaf_set(Rest, LeafSet).

successive_merge([Tree], Tree).
successive_merge([T1, T2|Rest], HuffmanTree) :-
    merge_nodes(T1, T2, MergedNode),
    insert_sorted(MergedNode, Rest, NewSet),
    successive_merge(NewSet, HuffmanTree).

merge_nodes(leaf(S1, W1), leaf(S2, W2), node(leaf(S1, W1), leaf(S2, W2), [S1, S2], W)) :-
    W is W1 + W2.
merge_nodes(Tree1, Tree2, node(Tree1, Tree2, Symbols, W)) :-
    Symbols = [Tree1, Tree2],
    W is W1 + W2,
    weight(Tree1, W1),
    weight(Tree2, W2).

insert_sorted(Tree, [], [Tree]).
insert_sorted(Tree, [T|Ts], [Tree, T|Ts]) :-
    weight(Tree, W1),
    weight(T, W2),
    W1 =< W2, !.
insert_sorted(Tree, [T|Ts], [T|NewTs]) :-
    insert_sorted(Tree, Ts, NewTs).

% Generates a symbol-to-bits table from a Huffman tree
hucodec_generate_symbol_bits_table(Tree, Table) :-
    traverse(Tree, [], Table).

traverse(leaf(S, _), Path, [sb(S, Path)]).
traverse(node(L, R, _, _), Path, Table) :-
    traverse(L, [0|Path], TableL),
    traverse(R, [1|Path], TableR),
    append(TableL, TableR, Table).

% Prints the Huffman tree structure for debugging
hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, 0).

print_tree(leaf(S, W), Indent) :-
    tab(Indent), write(S), write(' ('), write(W), write(')'), nl.
print_tree(node(L, R, _, W), Indent) :-
    tab(Indent), write('[NODE] ('), write(W), write(')'), nl,
    Indent1 is Indent + 2,
    print_tree(L, Indent1),
    print_tree(R, Indent1).

% Reads a file and encodes its content using a Huffman tree
hucodec_encode_file(Filename, Tree, Bits) :-
    open(Filename, read, Stream),
    read(Stream, Message),
    close(Stream),
    hucodec_encode(Message, Tree, Bits).

% huffman-codes.pl ends here
