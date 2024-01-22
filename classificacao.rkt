#lang racket

(require examples)

;;Estrutura do Resultado de uma partida
;;Leva em consideração a forma exata de como
;;o resultado da partida é disponibilizado no
;;arquivo jogos.txt. Armazena o time1 (primeiro
;;time mostrado no placar) e seus gols e igualmente
;;para o time2 (segundo time mostrado no placar)
(struct resultado (time1 gols1 time2 gols2) #:transparent)

;;Estrutura do desempenho de um time
;;Armazena o nome do time, os pontos por ele marcados,
;;as vitórias e o saldo de gols.
(struct desempenho (time pontos vitorias saldo-gols) #:transparent) 

;;ListaString->ListaResultado
;;Recebe como entrada uma string e
;;transforma esta string em um
;;resultado, sendo um resultado a
;;primeira estrutura definida neste código.
;;Caso a lista esteja vazia, retorna vazio.
(define (string->resultado lista-string)
  (cond
    [(empty? lista-string) empty]
    [else (define result (resultado (first (string-split lista-string))
                          (string->number (first (rest (string-split lista-string))))
                          (first (rest (rest (string-split lista-string))))
                          (string->number (first (rest (rest (rest (string-split lista-string))))))))
     result]))

(examples
 (check-equal? (string->resultado "Corinthians 6 Sao-Paulo 1") (resultado "Corinthians" 6 "Sao-Paulo" 1))
 (check-equal? (string->resultado (list)) empty))


(define (time-ja-existe? time lista)
  (cond
    [(empty? lista) #f] 
    [(equal? time (first lista)) #t]
    [else (time-ja-existe? time (rest lista))]))

;;ListaResultados->ListaString
(define (encontra-times1 resultados)
  (cond
    [(empty? resultados) empty]
    [else (cons (resultado-time1 (first resultados)) (encontra-times (rest resultados)))]))

(define (remove-repetidos resultados)
  (cond
    [(empty? resultados) empty]
    [(time-ja-existe? (first resultados) (rest resultados)) (remove-repetidos (rest resultados))]
    [else (cons (first resultados) (remove-repetidos (rest resultados)))]))

;;Faz com que cada time apareça uma vez só na lista
(define (encontra-times resultados)
  (remove-repetidos (encontra-times1 resultados)))

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
  
  (define (criar-string-com-espacos valor largura)
    (if (number? valor)
        (let* ((valor-str (number->string valor))
               (espacos (make-string (- largura (string-length valor-str)) #\space)))
          (string-append valor-str espacos))
        (let* ((valor-str valor)
               (espacos (make-string (- largura (string-length valor-str)) #\space)))
          (string-append valor-str espacos))))
  
  (cond
    [(empty? desempenho) empty]
    [else
     (string-append
      (criar-string-com-espacos (desempenho-time desempenho) 15)
      (criar-string-com-espacos (desempenho-pontos desempenho) 5)
      (criar-string-com-espacos (desempenho-vitorias desempenho) 5)
      (criar-string-com-espacos (desempenho-saldo-gols desempenho) 5))]
     ))

(define alinhamento 5)

;; ListaString -> ListaString
(define (classifica-times sresultados)
  
  (define resultados (map string->resultado sresultados))
  
  (define times (encontra-times resultados))

  (define desempenhos (calcula-desempenhos times resultados))

  (define classificacao (classifica desempenhos))

  (map desempenho->string classificacao))

(display-lines (classifica-times (port->lines)))