;;;; Riccardo Piatti 909687

;;;; -*- Mode: Lisp -*-

;;;; huffman-codes.lisp

;;;; Decode a sequence of bits using a Huffman tree
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch)
             (unless (null bits)
               (let ((next-branch (choose-branch (first bits)
                                                 current-branch)))
                 (if (leaf-p next-branch)
                     (cons (leaf-symbol next-branch)
                           (decode-1 (rest bits) huffman-tree))
                     (decode-1 (rest bits) next-branch)))))
           )
    (decode-1 bits huffman-tree)))

;;;; Chooses the next branch in the Huffman tree based on a bit
(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (node-right branch))
        (t (error "Bad bit ~D." bit))))

;;;; Inserts a node into an ordered set based on weight
(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))

;;;; Constructs an initial ordered set of leaf nodes from a list of
;;;; symbol-frequency pairs
(defun make-leaf-set pairs
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cdr pair))
                    (make-leaf-set (cdr pairs))))))

;;;; Encodes a message using a Huffman tree
(defun hucodec-encode (message huffman-tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) huffman-tree)
              (hucodec-encode (cdr message) huffman-tree))))

;;;; Finds the bit sequence for a given symbol in a Huffman tree
(defun encode-symbol (symbol huffman-tree)
  (labels ((traverse (node path)
             (cond ((leaf-p node) (if (eq (leaf-symbol node) symbol) path nil))
                   (t (or (traverse (node-left node) (append path '(0)))
                          (traverse (node-right node) (append path '(1))))))))
    (or (traverse huffman-tree '())
        (error "Symbol ~A not found in Huffman tree." symbol))))

;;;; Generates a Huffman tree from a list of symbol-weight pairs
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (successive-merge (make-leaf-set symbols-n-weights)))

;;;; Merges nodes successively to form a Huffman tree
(defun successive-merge (nodes)
  (if (= (length nodes) 1)
      (car nodes)
      (let* ((first (car nodes))
             (second (cadr nodes))
             (rest (cddr nodes))
             (merged (make-code-tree first second)))
        (successive-merge (adjoin-set merged rest)))))

;;;; Generates a symbol-to-bits table from a Huffman tree
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node path)
             (if (leaf-p node)
                 (list (cons (leaf-symbol node) path))
                 (append (traverse (node-left node) (append path '(0)))
                         (traverse (node-right node) (append path '(1)))))))
    (traverse huffman-tree '())))

;;;; Prints the Huffman tree structure for debugging
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (if (leaf-p huffman-tree)
      ((format t "~&  ~%") t "~V@T[NODE] (~D)~%" indent-level (leaf-symbol huffman-tree) (weight huffnab-tree))
      (progn
        (format t "~V@T[NODE] (~D)~%" indent-level (weight huffman-tree))
        (hucodec-print-huffman-tree (node-left huffman-tree) (+ indent-level 2))
        (hucodec-print-huffman-tree (node-right huffman-tree (+ indent-level 2)))))

;;;; Reads a file and encodes its content using a Huffman tree
(defun hucode-encode-file (filename huffman-treew)
  (with-open-file (stream filename :direction :input)
    (let ((message (read stream)))
      (hucodec-encode message huffman-tree))))

;;;; huffman-codes.lisp ends here
