;;;; Riccardo Piatti 909687

;;;; -*- Mode: Lisp -*-

;;;; huffman-codes.lisp

;;;; decode procedure
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch)
             (unless (null bits)
               (let ((next-branch (choose-branch (first bits)
                                                 current-branch)))
                 (if (leaf-p next-branch)
                     (cons (leaf-symbol next-branch)
                           (decode-1 (rest bits) huffman-tree))
                     (decode-1 (rest bits) next-branch)))
               ))
           )
    (decode-1 bits huffman-tree)))

(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (node-right branch))
        (t (error "Bad bit ~D." bit))))

(defun adjoin-set (x set)
  (cond ((null? set) (list x))
        ((< (wight x) (wight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;;;; procedure that transforms the list of pairs into an ordered set
;;;; of leaves
(defun make-leaf-set pairs
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)     ; symbol
                               (cadr pair))   ; frequency
                    (make-leaf-set (cdr pairs))))))

;;;; encode procedure
(defun hucodec-encode (message huffman-tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) huffman-tree)
              (encode (cdr message) huffman-tree))))

;;;; procedure that returns the list of bits that encodes a give
;;;; symbol according to a given tree. You should desingn the
;;;; procedure so that it signals an error if the symbol is not
;;;; in the tree at all.
(defun encode-symbol)

;;;; procedure that takes as its argument a list of symbol-frequency
;;;; pairs (where no symbol appears in more than one pair) and
;;;; generates a Huffman encoding tree according to the Huffman
;;;; algorithm
(defun hucodec-generate-huffman-tree symbols-n-weights
  (successive-merge (make-leaf-set symbols-n-weights)))

;;;; procedure that uses 'make-code-tree' to successively merge the
;;;; smallest-weight elements of the set until there is only one
;;;; element left, which is the desired Huffman tree (slightly tricky,
;;;; if you find yourself designing a complex procedure, you are doing
;;;; something wrong. Take advantage of the fact that we are using an
;;;; ordered set representation.
(defun successive-merge )

;;;; huffman-codes.lisp ends here
