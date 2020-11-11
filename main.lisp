; Esta função cria as estrutura que representam o puzzle
(defun cria_puzzle()

  ; Cria a matriz principal, defvar seta var global
    (defvar matriz-principal (make-array '(7 7)
    :initial-contents '((0 0 7 0 5 0 2)
                        (0 0 0 0 0 0 0)
                        (2 0 0 0 0 1 0)
                        (1 0 5 2 0 0 0)
                        (0 5 0 0 0 0 0)
                        (0 0 1 0 0 0 0)
                        (0 0 0 0 0 6 4))))

    ; Cria matriz de grupos, cada valor corresponde ao grupo que a posicao pertence
    (defvar matriz-secundaria (make-array '(7 7)
    :initial-contents '((0 1 2 2 3 4 4)
                        (18 18 19 2 3 4 5)
                        (16 17 20 20 3 6 7)
                        (16 16 20 3 3 7 7)
                        (14 15 15 15 15 8 8)
                        (13 12 12 11 11 9 9)
                        (13 12 12 11 10 9 9))))

    ;(setf num_grupos 21)

    ; Cria uma lista que guarda os valores de cada grupo
    ; Usado para checar se há uma sequência válida de valores
    (defvar lista-grupos (make-array '(21)))
    (dotimes (i (array-total-size lista-grupos))
        (setf (aref lista-grupos i) '(0))
    
    (defvar lista-tamanhos (make-array '(21)))
    ()

  )
)

(defun set-grupos()
    (loop for i below (array-total-size matriz-secundaria)
        ;(nconc (aref lista-grupos i) )
    )
)

; Para que um número "num" possa ser inserido num grupo "g" na posição [lin, col] da matriz principal, duas condições devem ser atendidas:
; 1. "num" não está contido no grupo "g".
; 2. "num" não está contido na linha "lin" nem na coluna "col";
(defun possivel())
; Função de busca em lista
(defun busca(lista x)
    (cond
        ((null lista)
            NIL)
        ((= (first lista) x)
            t)
        (t (= (first lista) x)
            (busca (rest lista) x))
    )
)

; Verifica a condição 1
(defun search1(lin col num)
    (busca (aref lista-grupos (aref matriz-secundaria lin col)) num)
)

; Verifica a condição 2
(defun search2(lin col num)
    ; Retorna T se não encontrar "num" na linha "lin" nem na coluna "col" 
    (if (or
            (busca (getlinha matriz-principal lin) num)
            (busca (getcoluna matriz-principal col) num)
        )
        t
    )
)

(defun getlinha(matriz lin)
    (loop for i below (array-dimension matriz 0) collect
        (aref matriz lin i))
)

(defun getcoluna(matriz col)
    (loop for i below (array-dimension matriz 0) collect
        (aref matriz i col))
)

; Esta função implementa o backtracking, estratégia empregada para a resolução do cria_puzzle
(defun resolve()

)


(defun main()
    ; cria_puzzle()
    ; mostrar puzzle
    ; resolve()
    ; mostrar resultado
    (cria_puzzle)
    (write matriz-principal)
    (write matriz-secundaria)
    (write lista-grupos)
    (write (search2 0 0 (read)))
)

(main)