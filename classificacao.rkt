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
(define (string->resultado str)
  (cond
    [(empty? str) empty]
    [else (define result (resultado (first (string-split str))
                          (string->number (first (rest (string-split str))))
                          (first (rest (rest (string-split str))))
                          (string->number (first (rest (rest (rest (string-split str))))))))
     result]))

(examples
 (check-equal? (string->resultado "Corinthians 6 Sao-Paulo 1") (resultado "Corinthians" 6 "Sao-Paulo" 1)))

;;ListaResultados->ListaString
;;Recebe uma lista contendo os resultados
;;e retorna uma lista de strings contém
;;cada time que participou dos jogos
;;apenas uma vez.
(define (encontra-times resultados)

  (define (time-ja-existe? time lista)
    (cond
      [(empty? lista) #f] 
      [(equal? time (first lista)) #t]
      [else (time-ja-existe? time (rest lista))]))
  
  (define (encontra-times1 resultados)
    (cond
      [(empty? resultados) empty]
      [else (cons (resultado-time1 (first resultados)) (cons (resultado-time2 (first resultados)) (encontra-times (rest resultados))))]))

  (define (remove-repetidos resultados)
    (cond
      [(empty? resultados) empty]
      [(time-ja-existe? (first resultados) (rest resultados)) (remove-repetidos (rest resultados))]
      [else (cons (first resultados) (remove-repetidos (rest resultados)))]))

  
  (remove-repetidos (encontra-times1 resultados)))

(examples
 (check-equal? (encontra-times (list (resultado "Corinthians" 6 "Sao-Paulo" 1))) '("Corinthians" "Sao-Paulo"))
 (check-equal? (encontra-times (list (resultado "Flamengo" 1 "Vasco" 0) (resultado "Vasco" 2 "America-RJ" 2))) '("Flamengo" "Vasco" "America-RJ")))


;;ListaString, ListaResultados -> ListaInteiros
;;Calcula os pontos de um time.
;;Recebe uma lista contendo os times e outra contendo
;;os resultados. Para o primeiro time na lista, a lista de
;;resultados é analisada. Caso o time da lista não
;;esteja entre os times da partida, analisa-se o
;;próximo resultado. Caso contrário, analisa-se o
;;resultado. Se o time analisado vencer, adiciona-se
;;à lista de saída 3 e analisa-se o próximo resultado.
;;Se o time analisado empatar, adiciona-se 1 à lista
;;de saída e analisa-se o próximo resultado. Se o time
;;analisado perder, apenas analisa-se o próximo resultado.
;;Caso alguma das listas estiver vazia, retorna-se empty.
;;No fim, soma todos os elementos da lista formada para
;;obter a totalidade de pontos que o time fez. Repete este
;;processo para todos os times (usando recursão com o resto
;;da lista de times) e no fim forma uma lista contendo
;;a quantidade de pontos que cada equipe fez.
(define (calcula-pontos times resultados)
  
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
  
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-pontos-aux times resultados)) (calcula-pontos (rest times) resultados))]))

(examples
 (check-equal? (calcula-pontos (list "Corinthians" "Flamengo") (list (resultado "Corinthians" 8 "Flamengo" 0) (resultado "Flamengo" 2 "Corinthians" 3))) '(6 0))
 (check-equal? (calcula-pontos (list "Fluminense" "Palmeiras" "Vasco") (list (resultado "Palmeiras" 3 "Fluminense" 1) (resultado "Vasco" 2 "Fluminense" 2))) '(1 3 1)))


;;ListaString, ListaResultados -> ListaInteiros
;;Calcula as vitórias de um time.
;;Recebe uma lista contendo os times e outra contendo
;;os resultados. Para cada time na lista, a lista de
;;resultados é analisada. Caso os time da lista não
;;esteja entre os times da partida, analisa-se o
;;próximo resultado. Caso contrário, analisa-se o
;;resultado. Se o time analisado vencer, adiciona-se
;;à lista de saída 1 e analisa-se o próximo resultado.
;;Se o time analisado empatar ou perder, apenas
;;analisa-se o próximo resultado. Caso alguma das
;;listas estiver vazia, retorna-se empty.
;;No fim, soma todos os elementos da lista formada para
;;obter a totalidade de vitórias que o time obteve.
;;Repete este processo para todos os times (usando
;;recursão com o resto da lista de times) e no
;;fim forma uma lista contendo a quantidade de
;;vitórias que cada equipe obteve.

(define (calcula-vitorias times resultados)
  
  (define (calcula-vitorias-aux times resultados)
    (cond
      [(empty? resultados) empty]
      [(empty? times) empty]
      [(equal? (first times) (resultado-time1 (first resultados)))
       (cond
         [(> (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
          (cons 1 (calcula-vitorias-aux times (rest resultados)))]
         [(< (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
          (calcula-vitorias-aux times (filter (curry equal? (first times)) (rest resultados)))]
         [(= (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados)))
          (calcula-vitorias-aux times (filter (curry equal? (first times)) (rest resultados)))]
         [else (calcula-vitorias-aux (rest times) resultados)])]
      [(equal? (first times) (resultado-time2 (first resultados)))
       (cond
         [(> (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
          (cons 1 (calcula-vitorias-aux times (rest resultados)))]
         [(< (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
          (calcula-vitorias-aux times (filter (curry equal? (first times)) (rest resultados)))]
         [(= (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados)))
          (calcula-vitorias-aux times (filter (curry equal? (first times)) (rest resultados)))]
         [else (cons (calcula-vitorias-aux (rest times) resultados))])]
      [else (calcula-vitorias-aux times (rest resultados))]))
  
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-vitorias-aux times resultados)) (calcula-vitorias (rest times) resultados))]))


(examples
 (check-equal? (calcula-vitorias (list "Corinthians" "Flamengo") (list (resultado "Corinthians" 8 "Flamengo" 0) (resultado "Flamengo" 2 "Corinthians" 3))) '(2 0))
 (check-equal? (calcula-vitorias (list "Fluminense" "Palmeiras" "Vasco") (list (resultado "Palmeiras" 3 "Fluminense" 1) (resultado "Vasco" 2 "Fluminense" 2))) '(0 1 0)))

;;ListaString, ListaResultados -> ListaInteiros
;;Recebe uma lista contendo os times e outra contendo
;;os resultados. Para cada time na lista, a lista de
;;resultados é analisada. Caso os time da lista não
;;esteja entre os times da partida, analisa-se o
;;próximo resultado. Caso contrário, analisa-se o
;;resultado. Pega-se a quantidade de gols que a equipe
;;analisada marcou e subtrai pela quantidade de gols
;;que seu adversário marcou. Este valor é adicionado
;;à lista de saída e analisa-se o próximo resultado.
;;Caso alguma das listas estiver vazia, retorna-se empty.
;;No fim, soma todos os elementos da lista formada para
;;obter a totalidade de vitórias que o time obteve.
;;Repete este processo para todos os times (usando
;;recursão com o resto da lista de times) e no
;;fim forma uma lista contendo o saldo de gols
;;que cada equipe obteve.
(define (calcula-sg times resultados)
  
  (define (calcula-sg-aux times resultados)
    (cond
      [(empty? resultados) empty]
      [(empty? times) empty]
      [(equal? (first times) (resultado-time1 (first resultados)))
       (cons (- (resultado-gols1 (first resultados)) (resultado-gols2 (first resultados))) (calcula-sg-aux times (rest resultados)))]
      [(equal? (first times) (resultado-time2 (first resultados)))
       (cons (- (resultado-gols2 (first resultados)) (resultado-gols1 (first resultados))) (calcula-sg-aux times (rest resultados)))]
      [else (calcula-sg-aux times (rest resultados))]))
  
  (cond
    [(empty? times) empty]
    [else
     (cons (foldr + 0 (calcula-sg-aux times resultados)) (calcula-sg (rest times) resultados))]))

(examples
 (check-equal? (calcula-sg (list "Corinthians" "Flamengo") (list (resultado "Corinthians" 8 "Flamengo" 0) (resultado "Flamengo" 2 "Corinthians" 3))) '(9 -9))
 (check-equal? (calcula-sg (list "Fluminense" "Palmeiras" "Vasco") (list (resultado "Palmeiras" 3 "Fluminense" 1) (resultado "Vasco" 2 "Fluminense" 2))) '(-2 2 0)))
    
;;Calcula os desempenhos de cada time, usando
;;como auxiliares as funções calcula-pontos,
;;calcula-vitorias e calcula-sg, e os adiciona
;;em uma lista.
(define (calcula-desempenhos times resultados)
  (cond
    [(empty? times) empty]
    [else
     (define desemp (desempenho (first times)
                                (first (calcula-pontos times resultados))
                                (first (calcula-vitorias times resultados))
                                (first (calcula-sg times resultados))))
     (cons desemp (calcula-desempenhos (rest times) resultados))]))

(examples
 (check-equal? (calcula-desempenhos (list "Corinthians" "Flamengo") (list (resultado "Corinthians" 8 "Flamengo" 0) (resultado "Flamengo" 2 "Corinthians" 3))) (list (desempenho "Corinthians" 6 2 9) (desempenho "Flamengo" 0 0 -9)))
 (check-equal? (calcula-desempenhos (list "Fluminense" "Palmeiras" "Vasco") (list (resultado "Palmeiras" 3 "Fluminense" 1) (resultado "Vasco" 2 "Fluminense" 2))) (list (desempenho "Fluminense" 1 0 -2) (desempenho "Palmeiras" 3 1 2) (desempenho "Vasco" 1 0 0))))


;;Classifica os desempenhos de acordo com estes parâmetros:
;;Primeiro: Mais pontos
;;Segundo: Mais vitórias
;;Terceiro: Maior saldo de gols
;;Quarto: Ordem alfabética
(define (classifica desempenhos)
  
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
  
  (if (empty? desempenhos)
      '()
      (insere-ordenado (first desempenhos)
                       (classifica (rest desempenhos))
                       compara-times)))

;;Transforma a lista de desempenhos em uma lista de strings
;;e formata para facilitar a leitura.
(define (desempenho->string desempenho)
  
  (define (cria-string-com-espacos valor largura)
    (if (number? valor)
        (string-append (number->string valor) (make-string (- largura (string-length (number->string valor))) #\space))
        (string-append valor (make-string (- largura (string-length valor)) #\space))))
  
  (cond
    [(empty? desempenho) empty]
    [else
     (string-append
      (cria-string-com-espacos (desempenho-time desempenho) 15)
      (cria-string-com-espacos (desempenho-pontos desempenho) 5)
      (cria-string-com-espacos (desempenho-vitorias desempenho) 5)
      (cria-string-com-espacos (desempenho-saldo-gols desempenho) 5))]))

;; ListaString -> ListaString
(define (classifica-times sresultados)
  
  (define resultados (map string->resultado sresultados))
  
  (define times (encontra-times resultados))

  (define desempenhos (calcula-desempenhos times resultados))

  (define classificacao (classifica desempenhos))

  (map desempenho->string classificacao))

(display-lines (classifica-times (port->lines)))