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

;;
(define (calcula-desempenhos times resultados)
  (define desemp ((resultado-time1 (first resultados)) 0 0 0))
  (cond
    [(equal? (first times) (resultado-time1 (first resultados)))
     (cond
       [(> (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (+ 3 (desempenho-pontos desemp))
        (+ 1 (desempenho-vitorias desemp))
        (+ (- (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados))) (desempenho-saldo-gols desemp))]
       [(< (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
        (+ (- (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados))) (desempenho-saldo-gols desemp))]
       [else
        (+ 1 (desempenho-pontos desemp))
        (+ 1 (desempenho-vitorias desemp))
        (+ (- (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados))) (desempenho-saldo-gols desemp))]
       
       [else (calcula-desempenhos times (rest resultados))])


     ]))
    
    





;; ListaString -> ListaString
;;(define (classifica-times sresultados)

(define resultados (map string->resultado1 sresultados))
  
(define times (encontra-times resultados))

  ;;(define desempenhos (calcula-desempenhos times resultados))

  ;;(define classificacao (classifica desempenhos))

  ;;(map desempenho->string classificacao))


;;(display-lines (classifica-times (port->lines)))
  