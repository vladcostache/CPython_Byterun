#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (if (null? stack) '() (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (car (cdr stack-machine)))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (car (cdr (cdr stack-machine))))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (car (cdr (cdr (cdr stack-machine)))))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (car (cdr (cdr (cdr (cdr stack-machine))))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 5))
;; 0
(define (get-IC stack-machine) (car (cdr (cdr (cdr (cdr (cdr stack-machine)))))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (- (length symbols) (length (member symbol symbols))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"

(define (left-list stack-machine index) 
  (take stack-machine index))  ;; prima parte (stanga)

(define (right-list stack-machine index)
  (drop stack-machine (+ 1 index)))  ;; a doua parte (dreapta)

(define (update-stack-machine item symbol stack-machine)
  (append (left-list stack-machine (get-symbol-index symbol)) (list item) (right-list stack-machine (get-symbol-index symbol))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
 (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (if (equal? (length (get-code stack-machine)) (get-IC stack-machine)) stack-machine
       (run-stack-machine (update stack-machine))))

(define (cur-ins stack-machine)
   (car (list-ref (get-code stack-machine) (get-IC stack-machine))))  ;; returneaza codul operatiei curente

(define (param stack-machine)
  (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))  ;; returneaza parametrul(argumentul) operatiei

(define (update-code stack-machine)
  (update-stack-machine (+ 1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine)) ;; modifica lista de operatii

(define (get-TOS stack-machine) 
  (car(get-stack stack-machine)))

(define (update stack-machine)
  (if (equal? 'POP_TOP (cur-ins stack-machine)) (pop-exec-stack (update-code stack-machine))
  (if (equal? 'LOAD_CONST (cur-ins stack-machine)) (load_const stack-machine)
  (if (equal? 'LOAD_FAST (cur-ins stack-machine)) (load_fast stack-machine)
  (if (equal? 'STORE_FAST (cur-ins stack-machine)) (store_fast stack-machine) 
  (if (equal? 'RETURN_VALUE (cur-ins stack-machine)) (update-code stack-machine)
  (if (equal? 'BINARY_ADD (cur-ins stack-machine)) (binary_add stack-machine)
  (if (equal? 'BINARY_SUBTRACT (cur-ins stack-machine)) (binary_sub stack-machine)
  (if (equal? 'BINARY_MODULO (cur-ins stack-machine)) (binary_mod stack-machine)
  (if (equal? 'INPLACE_ADD (cur-ins stack-machine)) (binary_add stack-machine)
  (if (equal? 'INPLACE_SUBTRACT (cur-ins stack-machine)) (binary_sub stack-machine)
  (if (equal? 'INPLACE_MODULO (cur-ins stack-machine)) (binary_mod stack-machine)
  (if (equal? 'COMPARE_OP (cur-ins stack-machine)) (compare_op stack-machine)
  (if (equal? 'POP_JUMP_IF_FALSE (cur-ins stack-machine)) (jmp_false stack-machine)
  (if (equal? 'POP_JUMP_IF_TRUE (cur-ins stack-machine)) (jmp_true stack-machine)
  (if (equal? 'JUMP_ABSOLUTE (cur-ins stack-machine)) (jmp_abs stack-machine)
  (if (equal? 'SETUP_LOOP (cur-ins stack-machine)) (update-code stack-machine)
  (if (equal? 'POP_BLOCK (cur-ins stack-machine)) (update-code stack-machine)
  (if (equal? 'GET_ITER (cur-ins stack-machine)) (update-code stack-machine)
  (if (equal? 'FOR_ITER (cur-ins stack-machine)) (iterator stack-machine)
  (if (equal? 'CALL_FUNCTION (cur-ins stack-machine)) (function stack-machine)
  (if (equal? 'LOAD_GLOBAL (cur-ins stack-machine)) (load_global stack-machine)
  stack-machine ))))))))))))))))))))))

(define (load_const stack-machine)
 (push-exec-stack (hash-ref (get-consts stack-machine) (param stack-machine)) (update-code stack-machine)))

(define (load_fast stack-machine)
 (push-exec-stack (hash-ref (get-varnames stack-machine) (param stack-machine)) (update-code stack-machine)))

(define (load_global stack-machine)
 (push-exec-stack (hash-ref (get-names stack-machine) (param stack-machine)) (update-code stack-machine)))

(define (store_fast stack-machine)
 (pop-exec-stack (update-stack-machine (hash-set (get-varnames stack-machine) (param stack-machine) (get-TOS stack-machine)) 'CO-VARNAMES (update-code stack-machine))))

(define (binary_add stack-machine)
 (push-exec-stack (+ (top (get-stack stack-machine)) (top (get-stack (pop-exec-stack stack-machine)))) (pop-exec-stack (pop-exec-stack (update-code stack-machine)))))

(define (binary_sub stack-machine)
 (push-exec-stack (- (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine)))))

(define (binary_mod stack-machine)
 (push-exec-stack (modulo (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine)))))

(define (compare_op stack-machine)
 (push-exec-stack ((get-cmpop (param stack-machine))  (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine)))))

(define (jmp_false stack-machine)
 (if (top (get-stack stack-machine)) (pop-exec-stack (update-code stack-machine))  (update-stack-machine (/ (param stack-machine) 2) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))

(define (jmp_true stack-machine)
 (if (top (get-stack stack-machine)) (update-stack-machine (/ (param stack-machine) 2) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)) (pop-exec-stack (update-code stack-machine))))

(define (jmp_abs stack-machine)
 (update-stack-machine (/ (param stack-machine) 2) 'INSTRUCTION-COUNTER stack-machine))

(define (iterator stack-machine)
 (if (null? (top (get-stack stack-machine)))
      (update-stack-machine (+ (get-IC stack-machine) (/ (param stack-machine) 2) 1) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      (update-code (push-exec-stack (car (top (get-stack stack-machine))) (push-exec-stack (cdr (top (get-stack stack-machine))) (pop-exec-stack stack-machine))))))

(define (function stack-machine)
     (if (equal? '"sqrt" (top (get-stack (pop-exec-stack stack-machine)))) (push-exec-stack (sqrt (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine))))
     (if (equal? '"range" (top (get-stack (pop-exec-stack stack-machine)))) (push-exec-stack (range (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine))))
     (if (equal? '"print" (top (get-stack (pop-exec-stack stack-machine)))) (push-exec-stack (writeln (top (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack (update-code stack-machine))))
     (update-code stack-machine)))))



