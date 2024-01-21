#lang racket

(require racket/file)

;;Estrutura do Resultado de uma partida
(struct resultado (time1 gols1 time2 gols2) #:transparent)

;;Estrutura do desempenho de um time
(struct desempenho (time pontos vitorias saldo-gols) #:transparent) 

;;Caminho do .txt contendo os resultados
(define caminho-do-arquivo "/home/luca/Downloads/jogos.txt")

;;Leitura do arquivo
(define (ler-arquivo caminho)
  (file->lines caminho))

;;Armazenamento dos resultados em uma lista de strings
(define sresultados (ler-arquivo caminho-do-arquivo))
                
;;ListaString->ListaResultado
(define (string->resultado lista-string)
  (cond
    [(empty? lista-string) empty]
    [else (define result (resultado (first (string-split (first lista-string)))
                          (string->number (first (rest (string-split (first lista-string)))))
                          (first (rest (rest (string-split (first lista-string)))))
                          (string->number (first (rest (rest (rest (string-split (first lista-string)))))))))
     (cons result (string->resultado (rest lista-string)))]))

;;ListaString->ListaResultado
(define (string->resultado1 lista-string)
  (cond
    [(empty? lista-string) empty]
    [else (define result (resultado (first (string-split lista-string))
                          (string->number (first (rest (string-split lista-string))))
                          (first (rest (rest (string-split lista-string))))
                          (string->number (first (rest (rest (rest (string-split lista-string))))))))
     result]))

;;ListaResultados->ListaString
(define (encontra-times1 resultados)
  (cond
    [(empty? resultados) empty]
    [else (cons (resultado-time1 (first resultados)) (encontra-times (rest resultados)))])
  )

;;Faz com que cada time apareça uma vez só na lista
(define (encontra-times resultados)
  (remove-duplicates (encontra-times1 resultados)))

;;Calcula os pontos de um time no campeonato
(define (calcula-pontos-aux times resultados)
  (cond
    [(empty? resultados) empty]
    [(empty? times) empty]
    [(equal? (first times) (resultado-time1 (first resultados)))
     (cond
       [(> (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (cons 3 (calcula-pontos-aux times (rest resultados)))]
       [(< (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (calcula-pontos-aux times (rest resultados))]
       [(= (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (cons 1 (calcula-pontos-aux times (rest resultados)))]
       [else (calcula-pontos-aux (rest times) resultados)])]
    [(equal? (first times) (resultado-time2 (first resultados)))
     (cond
       [(> (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (cons 3 (calcula-pontos-aux times (rest resultados)))]
       [(< (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (calcula-pontos-aux times (rest resultados))]
       [(= (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (cons 1 (calcula-pontos-aux times (rest resultados)))]
       [else (cons (calcula-pontos-aux (rest times) resultados))])]
    [else (calcula-pontos-aux times (rest resultados))]))

(define (calcula-pontos times resultados)
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-pontos-aux times resultados)) (calcula-pontos (rest times) resultados))]))

;;Calcula as vitórias de um time no campeonato
(define (calcula-vitorias-aux times resultados)
  (cond
    [(empty? resultados) empty]
    [(empty? times) empty]
    [(equal? (first times) (resultado-time1 (first resultados)))
     (cond
       [(> (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (cons 1 (calcula-vitorias-aux times (rest resultados)))]
       [(< (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (calcula-vitorias-aux times (rest resultados))]
       [(= (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (calcula-vitorias-aux times (rest resultados))]
       [else (calcula-vitorias-aux (rest times) resultados)])]
    [(equal? (first times) (resultado-time2 (first resultados)))
     (cond
       [(> (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (cons 1 (calcula-vitorias-aux times (rest resultados)))]
       [(< (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (calcula-vitorias-aux times (rest resultados))]
       [(= (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
        (calcula-vitorias-aux times (rest resultados))]
       [else (cons (calcula-vitorias-aux (rest times) resultados))])]
    [else (calcula-vitorias-aux times (rest resultados))]))

(define (calcula-vitorias times resultados)
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-vitorias-aux times resultados)) (calcula-vitorias (rest times) resultados))]))

;;Calcula o saldo de gols de um time
(define (calcula-sg-aux times resultados)
  (cond
    [(empty? resultados) empty]
    [(empty? times) empty]
    [(equal? (first times) (resultado-time1 (first resultados)))
     (cons (- (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados))) (calcula-sg-aux times (rest resultados)))]
    [(equal? (first times) (resultado-time2 (first resultados)))
     (cons (- (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados))) (calcula-sg-aux times (rest resultados)))]
    [else (calcula-sg-aux times (rest resultados))]))

(define (calcula-sg times resultados)
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-sg-aux times resultados)) (calcula-sg (rest times) resultados))]))
    
;;Calcula os desempenhos de cada time
(define (calcula-desempenhos times resultados)
  (cond
    [(empty? times) empty]
    [else
     (define desemp (desempenho (first times)
                                (first (calcula-pontos times resultados))
                                (first (calcula-vitorias times resultados))
                                (first (calcula-sg times resultados))))
     (cons desemp (calcula-desempenhos (rest times) resultados))]))


;;Classifica os desempenhos de acordo com estes parâmetros:
;;Primeiro: Pontos
;;Segundo: Número de vitórias
;;Terceiro: Saldo de gols
;;Quarto: Ordem alfabética
(define (compara-times desempenho1 desempenho2)
  (cond
  [(> (desempenho-pontos desempenho1) (desempenho-pontos desempenho2)) #t]
  [(< (desempenho-pontos desempenho1) (desempenho-pontos desempenho2)) #f]
  [(> (desempenho-vitorias desempenho1) (desempenho-vitorias desempenho2)) #t]
  [(< (desempenho-vitorias desempenho1) (desempenho-vitorias desempenho2)) #f]
  [(> (desempenho-saldo-gols desempenho1) (desempenho-saldo-gols desempenho2)) #t]
  [(< (desempenho-saldo-gols desempenho1) (desempenho-saldo-gols desempenho2)) #f]
  [(string<=? (desempenho-time desempenho1) (desempenho-time desempenho2)) #t]
  [else #f]))

(define (insere-ordenado desempenho desempenhos comparacao)
  (cond
    [(empty? desempenhos) (list desempenho)]
    [(comparacao desempenho (first desempenhos))
     (cons desempenho desempenhos)]
    [else
     (cons (first desempenhos) (insere-ordenado desempenho (rest desempenhos) comparacao))]))

(define (classifica desempenhos)
  (if (empty? desempenhos)
      '()
      (insere-ordenado (first desempenhos)
                       (classifica (rest desempenhos))
                       compara-times)))

;;Transforma a lista de desempenhos em uma lista de strings
(define (desempenho->string desempenho)
  (cond
    [(empty? desempenho) empty]
    [else
     (string-append (desempenho-time desempenho)
                    (make-string (- (string-length (desempenho-time desempenho)) alinhamento) #\space)
                    (number->string (desempenho-pontos desempenho))
                    (make-string (- alinhamento (string-length (number->string (desempenho-pontos desempenho)))) #\space)
                    (number->string (desempenho-vitorias desempenho))
                    (make-string (- alinhamento (string-length (number->string (desempenho-vitorias desempenho)))) #\space)
                    (number->string (desempenho-saldo-gols desempenho)))
     ]))

(define alinhamento 5)

;; ListaString -> ListaString
(define (classifica-times sresultados)
  (define resultados (map string->resultado1 sresultados))
  
  (define times (encontra-times resultados))

  (define desempenhos (calcula-desempenhos times resultados))

  (define classificacao (classifica desempenhos))

  (map desempenho->string classificacao))

;;(display-lines (classifica-times (port->lines)))