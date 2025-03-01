;;;; huffman-tests.lisp - Test suite per huffman-codes.lisp

(load "huffman-codes.lisp")

(defun run-tests ()
  (format t "\n===== TEST HUFFMAN =====\n")

  ;; Definizione della lista simbolo-peso
  (let* ((symbols '((a . 8) (b . 3) (c . 1) (d . 1) (e . 1) (f . 1) (g . 1) (h . 1)))
         (tree (hucodec-generate-huffman-tree symbols))
         (message '(a b a c d a e a f a b b a a g a h))
         (encoded (hucodec-encode message tree))
         (decoded (hucodec-decode encoded tree)))

    ;; Test generazione albero
    (format t "Test Huffman Tree: ~A~%" tree)
    (hucodec-print-huffman-tree tree)

    ;; Test encoding e decoding
    (format t "Messaggio originale: ~A~%" message)
    (format t "Messaggio codificato: ~A~%" encoded)
    (format t "Messaggio decodificato: ~A~%" decoded)
    (format t "Decodifica corretta: ~A~%" (equal message decoded))

    ;; Test tabella simboli-bits
    (let ((table (hucodec-generate-symbol-bits-table tree)))
      (format t "Tabella simboli-bits: ~A~%" table))

    ;; Test con file
    (with-open-file (out "test-message.txt" :direction :output :if-exists :supersede)
      (format out "~A" message))
    (let ((file-encoded (hucodec-encode-file "test-message.txt" tree)))
      (format t "Messaggio da file codificato: ~A~%" file-encoded))

    ;; Test errore su simbolo non presente
    (handler-case
        (progn
          (hucodec-encode '(z) tree)
          (format t "Errore mancato su simbolo non presente!~%"))
      (error (e) (format t "Errore rilevato correttamente: ~A~%" e)))))

(run-tests)
