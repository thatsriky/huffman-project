Huffman Encoding in Prolog

Questo progetto implementa l'algoritmo di codifica di Huffman utilizzando Prolog. La codifica di Huffman è un metodo di compressione senza perdita di dati che permette di rappresentare simboli frequenti con codici brevi e simboli rari con codici più lunghi, ottimizzando così lo spazio di archiviazione.

L'implementazione include le principali operazioni di codifica e decodifica, insieme alla generazione dell'albero di Huffman e delle relative tabelle di simboli e bit.

Funzioni Implementate:
1. hucodec_decode/3
  - Descrizione: Decodifica una sequenza di bit utilizzando un albero di Huffman.
  - Sintassi: (hucodec_decode(<bits>, <tree>, <message>))
  - Funzionamento: Quando una sequenza di bit è decodificata, viene navigato l'albero di Huffman. Ogni bit dirige l'algoritmo verso il ramo sinistro o destro, e quando si raggiunge una foglia, il simbolo viene aggiunto al messaggio decodificato.

2. decode_bits/4
  - Descrizione: Funzione ausiliaria che esegue la decodifica bit per bit dell'albero di Huffman.
  - Sintassi: (decode_bits(<bits>, <tree>, <node>, <message>))
  - Funzionamento: Analizza ogni bit della sequenza, esplorando il ramo dell'albero corrispondente. Quando si raggiunge una foglia, il simbolo viene aggiunto al messaggio, e la decodifica continua ricorsivamente per i bit restanti.
3. hucodec_encode/3
  - Descrizione: Codifica un messaggio utilizzando un albero di Huffman.
  - Sintassi: (hucodec_encode(<message>, <tree>, <bits>))
  - Funzionamento: La funzione esplora la tabella dei simboli e bit generata dall'albero di Huffman e, per ogni simbolo nel messaggio, trova la sua sequenza di bit corrispondente. Restituisce la sequenza di bit per l'intero messaggio.

4. hucodec_generate_huffman_tree/2
  - Descrizione: Genera un albero di Huffman a partire da una lista di coppie simbolo-peso.
  - Sintassi: (hucodec_generate_huffman_tree(<symbols_weights>, <huffman_tree>))
  - Funzionamento: Crea un insieme di foglie a partire dai simboli e pesi, ordina le foglie in base al peso e poi unisce ricorsivamente le foglie in nodi interni per costruire l'albero di Huffman.

5. make_leaf_set/2
  - Descrizione: Crea un insieme di foglie a partire dalla lista di coppie simbolo-peso.
  - Sintassi: (make_leaf_set(<symbols_weights>, <leaf_set>))
  - Funzionamento: Converte ogni coppia simbolo-peso in una foglia del tipo leaf(S, W).

6. successive_merge/2
  - Descrizione: Unisce i nodi in un albero di Huffman.
  - Sintassi: (successive_merge(<nodes>, <huffman_tree>))
  - Funzionamento: Unisce i nodi a due a due in base al peso, fino a formare un unico nodo che rappresenta l'albero di Huffman completo.

7. merge_nodes/3
  - Descrizione: Combina due nodi in un singolo nodo con simboli uniti e peso combinato.
  - Sintassi: (merge_nodes(<node1>, <node2>, <merged_node>))
  - Funzionamento: Unisce due nodi, sommandone i pesi e concatenando i simboli, creando così un nuovo nodo interno.

8. insert_sorted/3
  - Descrizione: Inserisce un nodo in un insieme ordinato.
  - Sintassi: (insert_sorted(<tree>, <sorted_trees>, <new_sorted_trees>))
  - Funzionamento: Aggiunge un nodo all'insieme di nodi in ordine crescente di peso.

9. hucodec_generate_symbol_bits_table/2
  - Descrizione: Genera una tabella di simboli e bit dall'albero di Huffman.
  - Sintassi: (hucodec_generate_symbol_bits_table(<tree>, <table>))
  - Funzionamento: Utilizza una traversata dell'albero per creare una tabella che associa ogni simbolo alla sua sequenza di bit.

10. traverse_tree/3
  - Descrizione: Esegue una traversata dell'albero per generare la tabella simbolo-bit.
  - Sintassi: (traverse_tree(<tree>, <code>, <table>))
  - Funzionamento: Percorre l'albero di Huffman, aggiungendo 0 al codice per il ramo sinistro e 1 per il ramo destro, raccogliendo la codifica di ciascun simbolo.

11. hucodec_print_huffman_tree/1
  - Descrizione: Stampa la struttura dell'albero di Huffman per il debug.
  - Sintassi: (hucodec_print_huffman_tree(<tree>))
  - Funzionamento: La funzione stampa l'albero di Huffman, mostrando il simbolo e il peso per ogni foglia e nodo interno, utile per il debug e per visualizzare l'albero.

13. print_tree/2
  - Descrizione: Stampa ricorsivamente un albero di Huffman.
  - Sintassi: (print_tree(<tree>, <indent>))
  - Funzionamento: La funzione stampa un nodo dell'albero, indentando i rami in modo da visualizzare la struttura gerarchica dell'albero.

14. hucodec_encode_file/3
  - Descrizione: Legge un file e codifica il suo contenuto usando un albero di Huffman.
  - Sintassi: (hucodec_encode_file(<filename>, <tree>, <bits>))
  - Funzionamento: Legge il contenuto di un file e lo codifica utilizzando l'albero di Huffman, restituendo la sequenza di bit.

15. read_file_content/2
  - Descrizione: Legge il contenuto di un file e lo converte in una lista di caratteri.
  - Sintassi: (read_file_content(<stream>, <content>))
  - Funzionamento: Legge i caratteri da un file e li converte in una lista, uno per uno.

16. weight/2
  - Descrizione: Restituisce il peso di un nodo.
  - Sintassi: (weight(<node>, <weight>))
  - Funzionamento: Se il nodo è una foglia, restituisce il suo peso. Se è un nodo interno, restituisce la somma dei pesi dei suoi sottoalberi.

17. symbols/2
  - Descrizione: Restituisce i simboli contenuti in un nodo.
  - Sintassi: (symbols(<node>, <symbols>))
  - Funzionamento: Se il nodo è una foglia, restituisce il simbolo. Se è un nodo interno, restituisce la lista di simboli dei sottoalberi.
