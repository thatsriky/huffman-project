;;;; Riccardo Piatti 909687
;;;; -*- Mode: Lisp -*-
;;;; huffman-codes.lisp

;;;; Definizione delle strutture dati per Huffman Tree
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf-p (node)
  (eq (car node) 'leaf))

(defun leaf-symbol (leaf)
  (cadr leaf))

(defun weight (node)
  (if (leaf-p node)
      (caddr node)
      (cadddr node)))

(defun symbols (node)
  (if (leaf-p node)
      (list (leaf-symbol node))
      (caddr node)))

(defun make-code-tree (left right)
  (list left right (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun node-left (node)
  (car node))

(defun node-right (node)
  (cadr node))

;;;; Decodifica una sequenza di bit usando un albero di Huffman
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch)
             (cond ((null bits)
                    (if (leaf-p current-branch)
                        (list (leaf-symbol current-branch))
                        '()))
                   (t (let ((next-branch (choose-branch (first bits) current-branch)))
                        (if (leaf-p next-branch)
                            (cons (leaf-symbol next-branch)
                                  (decode-1 (rest bits) huffman-tree))
                            (decode-1 (rest bits) next-branch)))))))
    (decode-1 bits huffman-tree)))

;;;; Sceglie il ramo successivo nell'albero Huffman basato su un bit
(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (node-right branch))
        (t (error "Bad bit ~D." bit))))

;;;; Inserisce un nodo in un insieme ordinato in base al peso
(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))

;;;; Costruisce un insieme ordinato di foglie a partire da coppie simbolo-peso
(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cdr pair))
                    (make-leaf-set (cdr pairs))))))

;;;; Codifica un messaggio usando un albero di Huffman
(defun hucodec-encode (message huffman-tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) huffman-tree)
              (hucodec-encode (cdr message) huffman-tree))))

;;;; Trova la sequenza di bit per un simbolo dato un albero Huffman
(defun encode-symbol (symbol huffman-tree)
  (labels ((traverse (node path)
             (cond ((leaf-p node) (if (eq (leaf-symbol node) symbol) path nil))
                   (t (or (traverse (node-left node) (append path '(0)))
                          (traverse (node-right node) (append path '(1))))))))
    (or (traverse huffman-tree '())
        (error "Symbol ~A not found in Huffman tree." symbol))))

;;;; Genera un albero di Huffman da una lista di coppie simbolo-peso
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (successive-merge (make-leaf-set symbols-n-weights)))

;;;; Unisce ricorsivamente i nodi per costruire un albero di Huffman
(defun successive-merge (nodes)
  (if (= (length nodes) 1)
      (car nodes)
      (let* ((first (car nodes))
             (second (cadr nodes))
             (rest (cddr nodes))
             (merged (make-code-tree first second)))
        (successive-merge (adjoin-set merged rest)))))

;;;; Genera una tabella simbolo-bit da un albero di Huffman
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node path)
             (if (leaf-p node)
                 (list (cons (leaf-symbol node) (reverse path)))
                 (append (traverse (node-left node) (cons 0 path))
                         (traverse (node-right node) (cons 1 path))))))
    (traverse huffman-tree '())))

  
;;;; Stampa la struttura dell'albero di Huffman per debugging
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (if (leaf-p huffman-tree)
      (format t "~V@T~A (~D)~%" indent-level (leaf-symbol huffman-tree) (weight huffman-tree))
      (progn
        (format t "~V@T[NODE] (~D)~%" indent-level (weight huffman-tree))
        (hucodec-print-huffman-tree (node-left huffman-tree) (+ indent-level 2))
        (hucodec-print-huffman-tree (node-right huffman-tree) (+ indent-level 2)))))

;;;; Legge un file e codifica il suo contenuto usando un albero di Huffman
(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename :direction :input)
    (let ((message (read stream)))
      (hucodec-encode message huffman-tree))))
