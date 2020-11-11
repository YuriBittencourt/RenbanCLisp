; Esta função cria as estrutura que representam o puzzle
(defun cria_puzzle()
    ;(defvar n 7)
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

    ;(defvar num_grupos '(21))

    ; Cria uma lista que guarda os valores de cada grupo
    ; Usado para checar se há uma sequência válida de valores
    (defvar lista-grupos (make-array '(21)))
    (dotimes (i (array-total-size lista-grupos))
        (setf (aref lista-grupos i) '())
    )
)

; Inicializa os elementos nos grupos de acordo com o tabuleiro inicial
(defun set-grupos()
    (defvar valor)
    (defvar index)
    (loop for i below (array-total-size matriz-secundaria) do
        (setf valor (row-major-aref matriz-principal i))
        (setf index (row-major-aref matriz-secundaria i))
        (if (/= valor 0)
            (add-grupo index valor)
        )
    )
)

; Insere elemento em grupo
(defun add-grupo(grupo valor)
    (setf (aref lista-grupos grupo) (append (aref lista-grupos grupo) (list valor)))
)

; Insere número na matriz principal
(defun add-numero(lin col num)
    (setf (aref matriz-principal lin col) num)
    (add-grupo (aref matriz-secundaria lin col) num)
)

; Para que um número "num" possa ser inserido num grupo "g" na posição [lin, col] da matriz principal, duas condições devem ser atendidas:
; 1. "num" não está contido no grupo "g".
; 2. "num" não está contido na linha "lin" nem na coluna "col";
(defun possivel(lin col num)
    (if (not (or (busca-grupo lin col num) (busca-linha-coluna lin col num)))
        t
    )
)

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
(defun busca-grupo(lin col num)
    (busca (aref lista-grupos (aref matriz-secundaria lin col)) num)
)

; Verifica a condição 2
(defun busca-linha-coluna(lin col num)
    (if (or
            (busca (getlinha matriz-principal lin) num)
            (busca (getcoluna matriz-principal col) num)
        )
        t
    )
)

; Getter para linha
(defun getlinha(matriz lin)
    (loop for i below (array-dimension matriz 0) collect
        (aref matriz lin i))
)

; Getter para coluna
(defun getcoluna(matriz col)
    (loop for i below (array-dimension matriz 0) collect
        (aref matriz i col))
)

; Esta função implementa o backtracking, estratégia empregada para a resolução do cria_puzzle
(defun resolve()
  (dotimes (lin 7)
    (dotimes (col 7)
      (if (=(aref matriz-principal lin col) 0)
          (progn
            (loop for num from 1 to 7 do
              (if (eq(possivel lin col num) T)
                  (progn
                    (add-numero lin col num)
                    (write matriz-principal)
                    (resolve)
                    (add-numero lin col 0)
                  )
              )
            )
            (return-from resolve nil)
          )
      )
    )
  )
)

(defun main()
    ; cria_puzzle()
    ; mostrar puzzle
    ; resolve()
    ; mostrar resultado
    (cria_puzzle)
    (write matriz-principal)
    (write matriz-secundaria)
    (set-grupos)
    (write lista-grupos)
    (write (possivel 3 1 2))
    (resolve)
    (write matriz-principal)
)

(main)
