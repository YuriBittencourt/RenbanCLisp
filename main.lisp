; Cria as estruturas que representam o Renban
(defun cria_puzzle()
    (defvar n)

    ; Cria a matriz principal
    (defvar matriz-principal)

    ; Cria a matriz secundária, onde cada valor representa o grupo ao qual pertence o elemento de mesma posição da matriz principal
    (defvar matriz-secundaria)
        
    ; Lê arquivo do STDIN e salva os valores em n, matriz principal e secundária    
    (le-arquivo)

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

    (set-grupos)
)


; Utiliza backtracking para resolver o Renban
(defun resolve(lin col)
    (if (=(aref matriz-principal lin col) 0)
        (loop for num from 1 to n do
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


; Para que um número "num" possa ser inserido num grupo "g" na posição [lin, col] da matriz principal, três condições devem ser atendidas:
; 1. "num" não está contido no grupo "g".
; 2. "num" não está contido na linha "lin" nem na coluna "col";
; 3. Os elementos de cada grupo devem formar uma sequência.
(defun possivel(lin col num)
    (let (grupo)
        (setf grupo (aref matriz-secundaria lin col))
        (if (not (or (busca-grupo grupo num) (busca-linha-coluna lin col num)))
            (if (no-intervalo grupo num)
                (return-from possivel t)
                (return-from possivel NIL)
            )
        )
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
    (let ((lstgrupo) (tamgrupo))
        (setf lstgrupo (aref lista-grupos grupo))
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

        (let ((min) (max) (diferenca))
            (setf min (first lstgrupo))
            (setf max (first (last lstgrupo)))

            ; Diferença entre tamanho do grupo e a distância do intervalo, inclusive
            (setf diferenca (- tamgrupo (+ (- max min) 1)))

            (if (and (>= num (- min diferenca)) (<= num (+ max diferenca)))
                t
            )
        )
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


; Transforma matriz em lista
(defun matriz-lista(matriz)
    (loop for i below (array-total-size matriz) collect
        (row-major-aref matriz i)
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
    (let (x)
        (setf x (aref matriz-principal lin col))
        (add-numero lin col 0)
        (remove-grupo (aref matriz-secundaria lin col) x)
    )
)


; Inicializa os elementos nos grupos de acordo com o tabuleiro inicial
(defun set-grupos()
    (let ((valor) (index-grupo))
        (loop for i below (array-total-size matriz-secundaria) do
            (setf valor (row-major-aref matriz-principal i))
            (setf index-grupo (row-major-aref matriz-secundaria i))
            (setf (aref tamanho-grupos index-grupo) (+ (aref tamanho-grupos index-grupo) 1))
            (if (/= valor 0)
                (add-grupo index-grupo valor)
            )
        )
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


; Método que lê o arquivo passado pelo STDIN e insere os valores nas matrizes
(defun le-arquivo()

    (setq n (parse-integer (read-line))) ; tamanho da matriz, n x n
    (read-line)
    
    (setq matriz-principal (make-array (list n n) :initial-element 0))
    (loop for i from 0 to (- n 1)
        do (let* ((line (read-line)) 
                  (fields line))

            (loop :for field :in (separa-linha fields)
                :for j :upfrom 0
                :do (setf (aref matriz-principal i j) (parse-integer field))
            )
        )      
    )

    (read-line)

    (setq matriz-secundaria (make-array (list n n) :initial-element 0))
    (loop for i from 0 to (- n 1)
        do (let* ((line (read-line)) 
                  (fields line))
            
            (loop :for field :in (separa-linha fields)
                :for j :upfrom 0
                :do (setf (aref matriz-secundaria i j) (parse-integer field))
            )
        )   
    )
)


; Método que divide string em uma lista de strings com delimitador especificado
(defun separa-linha (string &key (delimiterp #'delimiterp))
    (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end
    )
)


; Método que verifica se tal char é um dos caracteres que são delimitadores
(defun delimiterp (c) (position c " "))


(defun main()
    (cria_puzzle)
    (resolve 0 0)
)


(main)
