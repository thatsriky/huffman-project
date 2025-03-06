Huffman Encoding in Lisp.

Descrizione:
Questo progetto implementa l'algoritmo di codifica di Huffman utilizzando Common Lisp. La codifica di Huffman è un metodo di compressione senza perdita di dati, che consente di rappresentare simboli frequenti con codici brevi e simboli rari con codici più lunghi, ottimizzando così lo spazio di archiviazione.

L'implementazione fornisce le funzioni principali per la codifica e la decodifica di messaggi, la generazione dell'albero di Huffman, e l'encoding/decoding di file. L'algoritmo è strutturato attraverso l'uso di strutture dati (nodi e foglie) e funzioni ricorsive per costruire e navigare nell'albero di Huffman.

Funzioni implementate:

1. make-leaf
   - Crea una foglia dell'albero di Huffman, rappresentando un simbolo e il suo peso.
   - Sintassi: `(make-leaf <symbol> <weight>)`
   - Descrizione: Restituisce una lista che rappresenta una foglia dell'albero.

2. leaf-p
   - Verifica se un nodo è una foglia dell'albero di Huffman.
   - Sintassi: `(leaf-p <node>)`
   - Descrizione: Restituisce `T` se il nodo è una foglia, `NIL` altrimenti.

3. leaf-symbol
   - Estrae il simbolo da una foglia.
   - Sintassi: `(leaf-symbol <leaf>)`
   - Descrizione: Restituisce il simbolo contenuto in una foglia.

4. weight
   - Restituisce il peso di un nodo.
   - Sintassi: `(weight <node>)`
   - Descrizione: Se il nodo è una foglia, restituisce il peso associato alla foglia; se il nodo è un nodo interno, restituisce la somma dei pesi dei suoi sottoalberi.

5. symbols
   - Restituisce la lista dei simboli contenuti in un nodo.
   - Sintassi: `(symbols <node>)`
   - Descrizione: Restituisce la lista dei simboli dei sottoalberi.

6. make-code-tree
   - Crea un nodo interno dell'albero di Huffman.
   - Sintassi: `(make-code-tree <left> <right>)`
   - Descrizione: Crea un nodo combinando due sottoalberi e calcola il peso totale.

7. node-left / node-right
   - Estrae il sottoalbero sinistro/destro di un nodo.
   - Sintassi: `(node-left <node>)` / `(node-right <node>)`
   - Descrizione: Restituisce il sottoalbero corrispondente.

8. hucodec-decode
   - Decodifica una sequenza di bit usando un albero di Huffman.
   - Sintassi: `(hucodec-decode <bits> <huffman-tree>)`
   - Descrizione: Decodifica una sequenza di bit in un messaggio.

9. choose-branch
   - Seleziona il ramo successivo nell'albero di Huffman basato su un bit.
   - Sintassi: `(choose-branch <bit> <branch>)`

10. hucodec-generate-symbol-bits-table
    - Genera una tabella simbolo-bit da un albero di Huffman.
    - Sintassi: `(hucodec-generate-symbol-bits-table <huffman-tree>)`

11. hucodec-encode
    - Codifica un messaggio usando un albero di Huffman.
    - Sintassi: `(hucodec-encode <message> <huffman-tree>)`

12. hucodec-generate-huffman-tree
    - Genera un albero di Huffman da una lista di coppie simbolo-peso.
    - Sintassi: `(hucodec-generate-huffman-tree <symbols-n-weights>)`

13. hucodec-print-huffman-tree
    - Stampa la struttura dell'albero di Huffman.
    - Sintassi: `(hucodec-print-huffman-tree <huffman-tree>)`

14. hucodec-encode-file
    - Codifica il contenuto di un file utilizzando un albero di Huffman.
    - Sintassi: `(hucodec-encode-file <filename> <huffman-tree>)`
    - Descrizione: la funzione legge il contenuto di un file, lo codifica e restituisce la sequenza di bit.

15. read-file-symbols
    - Legge il contenuto di un file preservando tutti i simboli (compresi spazi e newline).
    - Sintassi:`(read-file-symbols <stream>)`
    - Descrizione: Legge ricorsivamente il file, trattando ogni carattere come simbolo unico.

Errori e gestione delle eccezioni
Il programma genera errori nei seguenti casi:
  - Codifica o decodifica con simboli non presenti nell'albero di Huffman.
  - Sequenze di bit non valide durante la decodifica.
  - Tentativi di decodifica con un albero di Huffman incompleto.

Gli errori vengono segnalati tramite la funzione `error`.
