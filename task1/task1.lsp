#!/usr/bin/sbcl --script
;chmod +x ./task1.lsp

; I)

; a1)
(defun pair(L)
    (cond ((null L) nil)
          ((null (cdr L)) (cons (cur_pair (car L) nil) (pair (cdr L))))
	  (T (cons (cur_pair (car L) (cadr L)) (pair (cddr L))))
    )
)

(defun cur_pair(a1 a2) (cons a1 (cons a2 nil)))
; ------------
; b2)
(defun rrest (N L)
    (cond ((< N 0) (neg_rest (+ (len L) N) L))
	  (T (pos_rest N L))
    )
)

(defun pos_rest (N L)
    (cond ((eql N 0) L)
	  (T (pos_rest (- N 1) (cdr L)))
    )
)

(defun neg_rest (N L)
    (cond ((eql N 0) nil)
	  (T (cons (car L) (neg_rest (- N 1) (cdr L))))
    )
)

(defun len (L)
    (cond ((null L) 0)
	  (T (+ 1 (len (cdr L))))
    )
)
; ------------
; II)

; 1)
(defun level (N S) (lvl N S nil))

(defun lvl (N S r)
    (cond ((null S) r)
	  ((eql N 1) (cons (car S) (lvl N (cdr S) r)))
	  ((atom (car S)) (lvl N (cdr S) r))
	  (T (lvl (- N 1) (car S) (lvl N (cdr S) r)))
    )
)
; ------------
; 2)
(defun trans (N S)
    (cond ((atom S) S)
	  ((eql N 0) 'q)
	  (T (cons (trans (- N 1) (car S)) (trans N (cdr S))))
    )
)
; ------------
; 3)
(defun freq (S) (frq S nil))

(defun frq (S r)
    (cond ((null S) r)
	  ((atom S) (inc S r))
	  (T (frq (cdr S) (frq (car S) r)))
    )
)

(defun inc (e r)
    (cond ((null r) (cons (cons e (cons 1 nil)) nil))
	  ((eql e (caar r)) (cons (cons e (cons (+ 1 (cadar r)) nil)) (cdr r)))
	  (T (cons (car r) (inc e (cdr r))))
    )
)

;; (print (pair '(a b c d e)))
;; (print (rrest 1 '(a (12) b (3))))
;; (print (rrest -2 '(a (12) b (3))))
;; (print (level 2 '(((a(5)8) b)7 (g(())) )))
;; (print (trans 2 '(((a(5)8) b (k))(g (с)))))
;; (print (trans 3 '( ( (a (5) 8) b ((f) v) (k)) (g (с) ((g))))))
;; (print (freq '(a (b (a)) c ((a) 8))))
;-----------------
(defun flat (S)
    (cond ((null S) T)
          (T (flt S (get_depth S 1) 0))
    )
)

(defun get_depth (S N) 
    (cond ((null S) 0)
          ((atom (car S)) N)
          (T (get_depth (car S) (+ N 1)))
    )
)

(defun flt (S check N)
    (cond ((null S) T)
          ((atom S) (eql check N))
          ((flt (car S) check (+ N 1)) (flt (cdr S) check N))
    )
)

;; (print (flat '(((b) (a c)) ((c 5)) ((d) 8)))) ; nil
;; (print (flat '(((b) (a c)) ((c 5)) ((d)))))   ; T
;; (print (flat '(a b c c)))                     ; T
;; (print (flat '((a) (b c) (c))))                   ; T
;; (print (flat '(((a)) d (b c) c)))                 ; nil

;-----------------
(defun tail (L1 L2) 
    (cond ((null L1) T)
          (T (ttail L1 L2 (len L1) (len L2)))
    )
)

(defun ttail (L1 L2 len1 len2)
    (cond ((eql len1 len2) (eql_list L1 L2))
          (T (ttail L1 (cdr L2) len1 (- len2 1)))
    )
)

(defun eql_list (L1 L2)
    (cond ((null L1) T)
          ((eql (car L1) (car L2)) (eql_list (cdr L1) (cdr L2)))
    )
)

(print (tail '(a b c d) '(e f a b a b c d)))
(print (tail '(a b c d) '(e f a b a b c d e)))