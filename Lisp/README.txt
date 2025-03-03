Huffman Encoding in Lisp.

Descrizione:
Questo progetto implementa l'algoritmo di codifica di Huffman utilizzando Common Lisp. La codifica di Huffman è un metodo di compressione senza perdita di dati, che consente di rappresentare simboli frequenti con codici brevi e simboli rari con codici più lunghi, ottimizzando così lo spazio di archiviazione.

L'implementazione fornisce le funzioni principali per la codifica e la decodifica di messaggi, la generazione dell'albero di Huffman, e l'encoding/decoding di file. L'algoritmo è strutturato attraverso l'uso di strutture dati (nodi e foglie) e funzioni ricorsive per costruire e navigare nell'albero di Huffman.

Funzioni implementate:
1. make-leaf
  - Crea una foglia dell'albero di Huffman, rappresentando un simbolo e il suo peso.
  - Sintassi: (make-leaf <symbol> <weight>)
  - Descrizione: La funzione crea una lista contenente un leaf, che è una struttura dati che rappresenta un simbolo e il suo peso.

2. leaf-p:
  - Verifica se un nodo è una foglia dell'albero di Huffman.
  - Sintassi: (leaf-p <node>)
  - Descrizione: Restituisce T se il nodo è una foglia, altrimenti NIL.

3. leaf-symbol:
  - Estrae il simbolo da una foglia.
  - Sintassi: (leaf-symbol <leaf>)
  - Descrizione: Restituisce il simbolo contenuto in una foglia.

4. weight:
  - Restituisce il peso di un nodo.
  - Sintassi: (weight <node>)
  - Descrizione: Se il nodo è una foglia, restituisce il peso associato alla foglia. Se il nodo è un nodo interno, restituisce la somma dei pesi dei suoi sottoalberi.

5. symbols:
  - Restituisce la lista dei simboli contenuti in un nodo.
  - Sintassi: (symbols <node>)
  - Descrizione: Se il nodo è una foglia, restituisce una lista contenente il simbolo della foglia. Se il nodo è un nodo interno, restituisce la lista dei simboli dei sottoalberi.

6. make-code-tree:
  - Crea un nodo interno dell'albero di Huffman.
  - Sintassi: (make-code-tree <left> <right>)
  - Descrizione: Crea un nodo che unisce due sottoalberi (left e right), e assegna a questo nodo la lista dei simboli combinati dei due sottoalberi e il peso totale (somma dei pesi dei due sottoalberi).

7. node-left:
  - Estrae il sottoalbero sinistro di un nodo.
  - Sintassi: (node-left <node>)
  - Descrizione: Restituisce il sottoalbero sinistro di un nodo interno.

8. node-right:
  - Estrae il sottoalbero destro di un nodo.
  - Sintassi: (node-right <node>)
  - Descrizione: Restituisce il sottoalbero destro di un nodo interno.

9. hucodec-decode:
  - Decodifica una sequenza di bit usando un albero di Huffman.
  - Sintassi: (hucodec-decode <bits> <huffman-tree>)
  - Descrizione: La funzione utilizza la ricorsione per navigare l'albero di Huffman e decodificare la sequenza di bit. La funzione verifica che i bit siano completi e che non ci siano bit in eccesso. Restituisce il messaggio decodificato come una lista di simboli.

10. choose-branch:
  - Sceglie il ramo successivo nell'albero di Huffman basato su un bit.
  - Sintassi: (choose-branch <bit> <branch>)
  - Descrizione: Questa funzione restituisce il sottoalbero sinistro se il bit è 0, oppure il sottoalbero destro se il bit è 1. In caso di bit errati, solleva un errore.

11. adjoin-set:
  - Inserisce un nodo in un insieme ordinato in base al peso.
  - Sintassi: (adjoin-set <node> <set>)
  - Descrizione: Aggiunge un nodo all'insieme set, mantenendo l'insieme ordinato in base al peso. Utilizza una ricorsione per mantenere l'ordinamento.

12. make-leaf-set:
  - Crea un insieme ordinato di foglie a partire da coppie simbolo-peso.
  - Sintassi: (make-leaf-set <pairs>)
  - Descrizione: La funzione prende una lista di coppie simbolo-peso e crea un insieme di foglie ordinato in base al peso dei simboli.

13. hucodec-encode:
  - Codifica un messaggio usando un albero di Huffman.
  - Sintassi: (hucodec-encode <message> <huffman-tree>)
  - Descrizione: La funzione percorre ricorsivamente il messaggio (una lista di simboli) e genera la sequenza di bit corrispondente utilizzando l'albero di Huffman.

14. encode-symbol:
  - Trova la sequenza di bit per un simbolo dato un albero di Huffman.
  - Sintassi: (encode-symbol <symbol> <huffman-tree>)
  - Descrizione: La funzione esplora ricorsivamente l'albero di Huffman per trovare la sequenza di bit che rappresenta un simbolo. Restituisce la sequenza di bit (una lista di 0 e 1).

15. hucodec-generate-huffman-tree:
  - Genera un albero di Huffman da una lista di coppie simbolo-peso.
  - Sintassi: (hucodec-generate-huffman-tree <symbols-n-weights>)
  - Descrizione: La funzione costruisce l'albero di Huffman a partire da una lista di coppie simbolo-peso, utilizzando la funzione successive-merge.

16. successive-merge:
  - Unisce ricorsivamente i nodi per costruire un albero di Huffman.
  - Sintassi: (successive-merge <nodes>)
  - Descrizione: La funzione unisce i nodi più leggeri in un unico nodo, ripetendo il processo fino a che non rimane un solo nodo, che rappresenta l'albero di Huffman finale.

17. hucodec-generate-symbol-bits-table:
  - Genera una tabella simbolo-bit da un albero di Huffman.
  - Sintassi: (hucodec-generate-symbol-bits-table <huffman-tree>)
  - Descrizione: La funzione restituisce una lista di coppie (simbolo . bits), dove ogni simbolo è associato alla sua sequenza di bit derivante dall'albero di Huffman.

18. hucodec-print-huffman-tree:
  - Stampa la struttura dell'albero di Huffman per il debug.
  - Sintassi: (hucodec-print-huffman-tree <huffman-tree> &optional (indent-level 0))
  - Descrizione: La funzione stampa ricorsivamente l'albero di Huffman, mostrando la struttura dei nodi e delle foglie con il relativo peso. È utile per il debug e per visualizzare l'albero.

19. hucodec-encode-file:
  - Legge un file e codifica il suo contenuto utilizzando un albero di Huffman.
  - Sintassi: (hucodec-encode-file <filename> <huffman-tree>)
  - Descrizione: La funzione legge il contenuto di un file, lo codifica utilizzando l'albero di Huffman e restituisce la sequenza di bit corrispondente.

Errori:
Il programma genera errori nel caso di:
  - codifica o decodifica con simboli non presenti nell'albero di Huffman;
  - sequenze di bit non valide durante la decodifica;
  - tentativi di decodifica con un albero di Huffman incompleto.
Gli errori vengono segnalati tramite la funzione 'error'.
