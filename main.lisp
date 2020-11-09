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
    :initial-contents '((1 2 3 3 4 5 5)
                        (19 19 20 3 4 5 6)
                        (17 18 21 21 4 7 8)
                        (17 17 21 4 4 8 8)
                        (15 16 16 16 16 9 9)
                        (14 13 13 12 12 10 10)
                        (14 13 13 12 11 10 10))))

  ; Cria uma lista que guarda os valores de cada grupo
  ; Usado para checar se há uma sequência válida de valores
  (defvar lista-grupos (make-array '(21)))
)


; Esta função checa se um valor(1-N) é válido em uma posição do puzzle
; É invocada pela função resolve, lin e col representam a posição na matriz
; num é o valor que será testado se é possivel ser colocado na posicao [lin][col]
(defun possivel(lin col num)

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
)

(main)
