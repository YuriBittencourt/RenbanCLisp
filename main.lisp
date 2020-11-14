; Cria as estruturas que representam o Renban
(defun cria_puzzle()
    (defvar n)
    (setf n 7)

    ; Cria a matriz principal
    (defvar matriz-principal (make-array (list n n)
        :initial-contents '((0 0 7 0 5 0 2)
                            (0 0 0 0 0 0 0)
                            (2 0 0 0 0 1 0)
                            (1 0 5 2 0 0 0)
                            (0 5 0 0 0 0 0)
                            (0 0 1 0 0 0 0)
                            (0 0 0 0 0 6 4))))

    ; Cria a matriz secundária, onde cada valor representa o grupo ao qual pertence o elemento de mesma posição da matriz principal
    (defvar matriz-secundaria (make-array (list n n)
        :initial-contents '((0 1 2 2 3 4 4)
                            (18 18 19 2 3 4 5)
                            (16 17 20 20 3 6 7)
                            (16 16 20 3 3 7 7)
                            (14 15 15 15 15 8 8)
                            (13 12 12 11 11 9 9)
                            (13 12 12 11 10 9 9))))

    ; Número de grupos
    (defvar num_grupos)
    (setf num_grupos (+ (maior(matriz-lista matriz-secundaria)) 1))

    ; Vetor contendo os grupos
    (defvar lista-grupos (make-array (list num_grupos)))
    ; Vetor com os tamanhos de cada lista
    (defvar tamanho-grupos (make-array (list num_grupos)))
    ; Inicialização dos vetores
    (dotimes (i (array-total-size lista-grupos))
        (setf (aref lista-grupos i) '())
        (setf (aref tamanho-grupos i) 0)
    )
)

; Transforma matriz em lista
(defun matriz-lista(matriz)
    (loop for i below (array-total-size matriz) collect
        (row-major-aref matriz i)
    )
)

; Encontra maior elemento em lista
(defun maior(lista)
    (cond
        ((null lista)
            0)
        ((null (rest lista))
            (first lista))
        ((> (first lista) (second lista))
            (maior (cons (first lista) (rest (rest lista)))))
        (t (maior (rest lista)))
    )
)

; Função auxiliar para inserir elemento em ordem crescente
(defun insert (item lst &optional (key #'<))
    (if (null lst)
        (list item)
        (if (funcall key item (car lst))
            (cons item lst)
            (cons (car lst) (insert item (cdr lst) key))
        )
    )
)

; Insere elemento em grupo
(defun add-grupo(grupo valor)
    (setf (aref lista-grupos grupo) (insert valor (aref lista-grupos grupo)))
)

; Remove elemento de grupo
(defun remove-grupo(grupo valor)
    (setf (aref lista-grupos grupo) (remove valor (aref lista-grupos grupo)))
)

; Insere número na matriz principal
(defun add-numero(lin col num)
    (setf (aref matriz-principal lin col) num)
    (if (= num 0)
      t
      (add-grupo (aref matriz-secundaria lin col) num)
    )
)

; Remove número da matriz principal
(defun remove-numero (lin col)
    (setf x (aref matriz-principal lin col))
    (add-numero lin col 0)
    (remove-grupo (aref matriz-secundaria lin col) x)
)

; Inicializa os elementos nos grupos de acordo com o tabuleiro inicial
(defun set-grupos()
    (let ((valor) (index-grupo)))
    (loop for i below (array-total-size matriz-secundaria) do
        (setf valor (row-major-aref matriz-principal i))
        (setf index-grupo (row-major-aref matriz-secundaria i))
        (setf (aref tamanho-grupos index-grupo) (+ (aref tamanho-grupos index-grupo) 1))
        (if (/= valor 0)
            (add-grupo index-grupo valor)
        )
    )
)

; Para que um número "num" possa ser inserido num grupo "g" na posição [lin, col] da matriz principal, três condições devem ser atendidas:
; 1. "num" não está contido no grupo "g".
; 2. "num" não está contido na linha "lin" nem na coluna "col";
; 3. Os elementos de cada grupo devem formar uma sequência.
(defun possivel(lin col num)
    (let (grupo))
    (setf grupo (aref matriz-secundaria lin col))
    (if (not (or (busca-grupo grupo num) (busca-linha-coluna lin col num)))
        (if (no-intervalo grupo num)
            (return-from possivel t)
            (return-from possivel NIL)
        )
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
(defun busca-grupo(grupo num)
    (busca (aref lista-grupos grupo) num)
)

; Verifica a condição 2
(defun busca-linha-coluna(lin col num)
    (if 
        (or
            (busca (getlinha matriz-principal lin) num)
            (busca (getcoluna matriz-principal col) num)
        )
        t
    )
)

; Verifica a condição 3
(defun no-intervalo(grupo num)
    (let (lstgrupo))
    (setf lstgrupo (aref lista-grupos grupo))
    (let (tamgrupo))
    (setf tamgrupo (aref tamanho-grupos grupo))
    
    ; Grupo vazio
    (if (null lstgrupo)
        (return-from no-intervalo t)
    )

    ; Grupo com um elemento
    (if (= (list-length lstgrupo) 1)
        (progn
            (if (and (>= num (- (first lstgrupo) (- tamgrupo 1))) (<= num (+ (first lstgrupo) (- tamgrupo 1))))
                t
                NIL
            )
        )
    )

    ; Grupo com (tamgrupo - 1) elementos
    ; Insere o último elemento e testa se forma sequência
    (if (= (list-length lstgrupo) (- tamgrupo 1))
        (progn
            (if (eh-sequencia (insert num lstgrupo))
                t
                NIL
            )
        )
    )

    (let (min))
    (setf min (first lstgrupo))
    (let (max))
    (setf max (first (last lstgrupo)))

    ; Diferença entre tamanho do grupo e a distância do intervalo, inclusive
    (let (diferenca))
    (setf diferenca (- tamgrupo (+ (- max min) 1)))

    (if (and (>= num (- min diferenca)) (<= num (+ max diferenca)))
        t
    )
)

; Verifica se uma lista forma sequência. Auxiliar para a função no-intervalo
(defun eh-sequencia(lista)
    (if (null lista)
        t
    )
    (if (null (rest lista))
        (return-from eh-sequencia t)
    )
    (if (= (+ (first lista) 1) (second lista))
        (eh-sequencia (rest lista))
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

; Utiliza backtracking para resolver o Renban
(defun resolve(lin col)
    (if (=(aref matriz-principal lin col) 0)
        (loop for num from 1 to 7 do
            (if (eq(possivel lin col num) T)
                (progn
                    (add-numero lin col num)
                    (recursao lin col)
                    (remove-numero lin col)
                )
            )
        )
    (recursao lin col)
    )
)

; Recorre ao método resolve até que a resposta seja atingida
(defun recursao(lin col)
    ; Matriz completa
    (if (and (= lin (- n 1)) (= col (- n 1)))
        (concluido)
    )
    
    ; Matriz incompleta
    (if (= col (- n 1))
        (resolve (+ lin 1) 0)
        (resolve lin (+ col 1))
    )
)

; Término da execução
(defun concluido()
    (write matriz-principal)
    (exit)
)

(defun main()
    (cria_puzzle)
    (set-grupos)
    (resolve 0 0)
)

(main)
