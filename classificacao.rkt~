#lang racket

(require racket/file)

(struct resultado (time1 gols1 time2 gols2) #:transparent)

(struct time (nome pontos vitorias saldo-gols) #:transparent) 

(define caminho-do-arquivo "/home/luca/Downloads/jogos.txt")

(define (ler-arquivo caminho)
  (file->lines caminho))

(define sresultados (ler-arquivo caminho-do-arquivo))

(define (encontra-times resultados)
  (cond
    [(empty? resultados) ]
    [else ""])
  )
                

;;ListaString->ListaResultado
(define (string->resultado lista-string)
  (cond
    [(empty? lista-string) empty]
    [else (define result (resultado (first (string-split (first lista-string)))
                          (string->number (first (rest (string-split (first lista-string)))))
                          (first (rest (rest (string-split (first lista-string)))))
                          (string->number (first (rest (rest (rest (string-split (first lista-string)))))))))
     (cons result (string->resultado (rest lista-string)))]))

;; ListaString -> ListaString
;;(define (classifica-times sresultados)

  ;;(define resultados (map string->resultado sresultados))
  
  ;;(define times (encontra-times resultados))

  ;;(define desempenhos (calcula-desempenhos times resultados))

  ;;(define classificacao (classifica desempenhos))

  ;;(map desempenho->string classificacao))


;;(display-lines (classifica-times (port->lines)))
  