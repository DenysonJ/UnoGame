;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-extraclasse-2019-1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; NOME: Denyson Jurgen Mendes Grellert

;; Exercício individual.
;; Para cada exercício, faça pelo menos 2 testes usando check-expect.
;; Lembre que todas as funções devem conter a documentação completa,
;; incluindo contrato, objetivo, exemplos e testes.

;; ==================================================================
;; DEFINIÇÕES DE TIPOS DE DADOS:
;; -----------------
;; TIPO CARTA-NUMERO:
;; -----------------
(define-struct carta-numero (cor valor))
;; Um elemento do conjunto Carta-NUMERO é
;;   (make-carta-numero c v)     onde
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo" ou "vermelho"
;;   v : Número, é o número da carta

;; Exemplos de constantes do tipo Carta-numero:
(define VERMELHO1 (make-carta-numero "vermelho" 1))
(define AZUL2     (make-carta-numero "azul" 2))
(define VERDE7    (make-carta-numero "verde"  7))

;; -----------------
;; TIPO CARTA-ESPECIAL:
;; -----------------
(define-struct carta-especial (cor tipo))
;; Um elemento do conjunto Carta-especial é
;;   (make-carta-especial c t)     onde
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo",  "vermelho" ou "qualquer"
;;   t : String, é o tipo da carta especial, que pode ser "Compra4", "Compra2", "Inverte", "PulaVez" e "TrocaCor"

;; Exemplos de constantes do tipo Carta-especial:
(define COMPRA2AZUL (make-carta-especial "azul" "Compra2"))
(define PULAVEZVERDE (make-carta-especial "verde" "PulaVez"))
(define TROCACOR (make-carta-especial "qualquer" "TrocaCor"))

;; -----------
;; TIPO CARTA:
;; -----------
;; Um elemento do conjunto Carta é
;; 1. uma carta-valor;
;; 2. uma carta-especial

;; Carta é um tipo misto, cujos elementos podem ser tipo Carta-numero ou tipo Carta-especial.

(define C1 (make-carta-numero "azul" 1))
(define C2 (make-carta-numero "vermelho" 8))
(define C3 (make-carta-especial "qualquer" "Compra4"))

;; ==================================================================
;; INCLUIR AQUI AS DEFINIÇÕES FEITAS NO EXTRACLASSE ANTERIOR QUE VOCE USAR NOS EXERCÍCIOS A SEGUIR

;; =========================
;; cor? : String -> String
;; Obj: Traduzir a cor do português para o inglês
;; Exemplos:
;;   (cor? "amarelo") = "yellow"
;;   (cor? "nada") = "black"

(define (cor? color)
  (cond
    [(string=? color "azul") "blue"]
    [(string=? color "verde") "green"]
    [(string=? color "amarelo") "yellow"]
    [(string=? color "vermelho") "red"]
    [else "black"]
    )
 )

(check-expect (cor? "amarelo") "yellow")
(check-expect (cor? "preto") "black")


;; =========================
;; carta-cor? : carta -> String
;; Obj: Dado uma carta, devolver a cor dela
;; Exemplos:
;;   (carta-cor? carta) = "blue"
;;   (carta-cor? carta1) = "black"

(define (carta-cor? carta)
  (cond
    [(carta-numero? carta) (carta-numero-cor carta)]
    [else (carta-especial-cor carta)]
    )
  )

(check-expect (carta-cor? VERDE7) "verde")
(check-expect (carta-cor? C1) "azul")


;; =========================


;; pode-jogar? : carta carta -> Boolean
;; Obj: Dado duas cartas, de acordo com as regras do jogo Uno, dizer se pode
;; ou não jogar a carta da mão.
;; Exemplos:

;; (pode-jogar? Compra2Azul InverteVerde) = #f
;; (pode-jogar? Compra4 ZeroAmarelo) = #t

(define (pode-jogar? cartaMesa cartaMao)
  (cond
    [(carta-numero? cartaMesa) (cond                                                                                       ;;carta número na mesa 
                       [(string=? (carta-cor? cartaMesa) (carta-cor? cartaMao)) true]                                          ;;carta da mesma cor na mão (pode jogar, especial ou número da mesma cor)
                       [(carta-numero? cartaMao)(cond                                                                          ;;carta número na mão
                          [(= (carta-numero-valor cartaMesa) (carta-numero-valor cartaMao)) true]                                  ;;carta não é da mesma cor, mas tem o mesmo valor
                          [else false]                                                                                             ;;carta número, de valor e cor diferente da carta da mesa
                          )]
                       [(string=? (carta-especial-tipo cartaMao) "Compra4") true]                                              ;;carta especial Compra4 na mão (pode jogar com qualquer carta na mesa)
                       [(string=? (carta-especial-tipo cartaMao) "TrocaCor") true]                                             ;;carta especial TrocaCor na mão (pode jogar com qualquer carta na mesa)
                       [else false]                                                                                            ;;as outras cartas especiais não são da mesma cor (já foi verificado)
                       )]
    [(string=? (carta-especial-tipo cartaMesa) "Compra4") true]                                                             ;;carta Compra4 na mesa (qualquer carta pode ser jogada)
    [(string=? (carta-especial-tipo cartaMesa) "TrocaCor") true]                                                            ;;carta TrocaCor na mesa (qualquer carta pode ser jogada)
    [(string=? (carta-especial-tipo cartaMesa) "Inverte") (cond                                                             ;;carta Inverte na mesa
                                                            [(string=? (carta-cor? cartaMesa) (carta-cor? cartaMao)) true]     ;;carta da mesma cor na mão, pode jogar
                                                            [(carta-numero? cartaMao) false]                                   ;;carta da mão é numero e não é da mesma cor, não pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "Inverte") true]         ;;carta da mão é Inverte de outra cor, pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "Compra4") true]         ;;carta especial Compra4 na mão (pode jogar com qualquer carta na mesa)
                                                            [(string=? (carta-especial-tipo cartaMao) "TrocaCor") true]        ;;carta especial TrocaCor na mão (pode jogar com qualquer carta na mesa)
                                                            [else false]
                                                            )]
    [(string=? (carta-especial-tipo cartaMesa) "PulaVez") (cond                                                             ;;carta PulaVez na mesa
                                                            [(string=? (carta-cor? cartaMesa) (carta-cor? cartaMao)) true]     ;;carta da mesma cor na mão, pode jogar
                                                            [(carta-numero? cartaMao) false]                                   ;;carta da mão é numero e não é da mesma cor, não pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "PulaVez") true]         ;;carta da mão é PulaVez de outra cor, pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "Compra4") true]         ;;carta especial Compra4 na mão (pode jogar com qualquer carta na mesa)
                                                            [(string=? (carta-especial-tipo cartaMao) "TrocaCor") true]        ;;carta especial TrocaCor na mão (pode jogar com qualquer carta na mesa)
                                                            [else false]
                                                            )]
    [(string=? (carta-especial-tipo cartaMesa) "Compra2") (cond                                                             ;;carta Compra2 na mesa
                                                            [(string=? (carta-cor? cartaMesa) (carta-cor? cartaMao)) true]     ;;carta da mesma cor na mão, pode jogar
                                                            [(carta-numero? cartaMao) false]                                   ;;carta da mão é numero e não é da mesma cor, não pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "Compra2") true]         ;;carta da mão é Compra2 de outra cor, pode jogar
                                                            [(string=? (carta-especial-tipo cartaMao) "Compra4") true]         ;;carta especial Compra4 na mão (pode jogar com qualquer carta na mesa)
                                                            [(string=? (carta-especial-tipo cartaMao) "TrocaCor") true]        ;;carta especial TrocaCor na mão (pode jogar com qualquer carta na mesa)
                                                            [else false]
                                                            )]
   )
  )

(check-expect (pode-jogar? (make-carta-numero "vermelho" 9) C2) true)
(check-expect (pode-jogar? C3 C2) true)
(check-expect (pode-jogar? TROCACOR C2) true)
(check-expect (pode-jogar? C2 VERDE7) false)
(check-expect (pode-jogar? COMPRA2AZUL C2) false)
(check-expect (pode-jogar? COMPRA2AZUL C1) true)
(check-expect (pode-jogar? COMPRA2AZUL PULAVEZVERDE) false)



;; ==================================================================

;; ========================================================================
;;                                 QUESTÃO 1
;; Defina o tipo de dados lista de cartas de Uno e dê 4 exemplos de elementos deste tipo de dados.
;; =========================================================================

;; -----------
;; TIPO LISTA-DE-CARTAS:
;; -----------
;; Uma lista-de-cartas pode ser:
;;  1. Vazia (empty), ou
;;  2. (cons c lc), onde
;;   c : carta
;;   lc: lista-de-cartas

(define L1 (cons C1 (cons VERDE7 empty)))
(define L2 (cons VERDE7 (cons AZUL2 (cons C2 (cons VERMELHO1 (cons C3 (cons C1 empty)))))))
(define L3 (cons empty empty))
(define L4 (cons C3 empty))

;; ========================================================================
;;                                 QUESTÃO 2
;; Construa uma função que, dada uma lista de cartas de Uno e uma cor,
;; devolve todas as cartas desta cor na lista.
;; ========================================================================

;; lista-cartas-cor: lista-de-cartas String -> lista-de-cartas
;; Obj: Dado uma lista de cartas e uma cor, devolver as cartas dessa lista que
;; são dessa cor, numa lista.
;; Exemplos:

;; (lista-cartas-cor L1 "verde") = (cons (make-carta-numero "verde" 7) '())
;; (lista-cartas-cor L2 "azul") = (cons (make-carta-numero "azul" 2) (cons (make-carta-numero "azul" 1) '()))

(define (lista-cartas-cor lc cor)
  (cond
    [(empty? lc) empty]
    [else (cond
            [(string=? (carta-cor? (first lc)) cor) (cons (first lc) (lista-cartas-cor (rest lc) cor))]
            [else (lista-cartas-cor (rest lc) cor)]
            )]
    )
  )

(check-expect (lista-cartas-cor L2 "azul") (cons (make-carta-numero "azul" 2) (cons (make-carta-numero "azul" 1) '())))
(check-expect (lista-cartas-cor L1 "verde") (cons (make-carta-numero "verde" 7) '()))

;; ========================================================================
;;                                 QUESTÃO 3
;; Defina o tipo de dados JOGADOR, que deve conter o nome do jogador, a
;; lista de cartas de sua mão e sua pontuação.
;; ========================================================================

;; -----------
;; TIPO JOGADOR:
;; -----------
(define-struct jogador (nome lc pontuacao))
;;  Um elemento do conjunto jogador é:
;;    (make-jogador n l p), onde:
;;       n: é uma String que representa o nome do jogador
;;       l: é uma lista-de-cartas que representa as cartas da mão do jogador
;;       p: é a pontuação do jogador

(define J1(make-jogador "João" L1 0))
(define J2(make-jogador "Maria" L2 0))

;; ========================================================================
;;                                 QUESTÃO 4
;; Construa uma função que, dados um jogador e uma carta comprada, insere
;; esta carta na lista de cartas do jogador.
;; =========================================================================

;; insere-carta-jogador: jogador carta -> jogador
;; Obj: Inserir a carta na lista de cartas do jogador
;; Exemplos:
;; (insere-carta-jogador J1 C3) = (make-jogador "João" (cons (make-carta-especial "qualquer" "Compra4") (cons (make-carta-numero "azul" 1) (cons (make-carta-numero "verde" 7) '()))) 0)
;; (insere-carta-jogador J2 C3) = (make-jogador "Maria" (cons (make-carta-especial "qualquer" "Compra4") (cons (make-carta-numero "verde" 7) (cons (make-carta-numero "azul" 2)
;;  (cons (make-carta-numero "vermelho" 8) (cons (make-carta-numero "vermelho" 1) (cons (make-carta-especial "qualquer" "Compra4") (cons (make-carta-numero "azul" 1) '())))))))
;;  0)

(define (insere-carta-jogador jogador carta)
  (make-jogador (jogador-nome jogador)(cons carta (jogador-lc jogador)) (jogador-pontuacao jogador))
  )

(check-expect (insere-carta-jogador J1 C3) (make-jogador "João" (cons C3 L1) 0))
(check-expect (insere-carta-jogador J2 C3) (make-jogador "Maria" (cons C3 L2) 0))

;; ========================================================================
;;                                 QUESTÃO 5
;; Construa uma função chamada vencedor que, dados 2 jogadores, define que ganhou.
;; Para isso, é necessário somar à pontuação de cada jogador os pontos referentes
;; às listas de cartas dos mesmos, e verificar quem tem a menor pontuação (como o
;; objetivo é ficar com menos cartas, que tem a menor pontuação ganha). A função
;; deve devolver apenas o nome e a pontuação do vencedor.

;; Os pontos das cartas de Uno são os seguintes:
;; * as cartas numeradas valem o seu número;
;; * as cartas "Compra2", "Inverte" e "PulaVez" valem 20;
;; * as cartas "Compra4" e "TrocaCor" valem 50.

;; =========================================================================

;; quanto-vale: carta -> Numero
;; Obj: Dado uma carta, devolve o valor daquela carta.
;; Exemplos:
;; (quanto-vale C1) = 1
;; (quanto-vale (make-carta-especial "azul" "Compra2")) = 20

(define (quanto-vale carta)
  (cond
    [(carta-numero? carta) (carta-numero-valor carta)]
    [(or (string=? (carta-especial-tipo carta) "Compra4") (string=? (carta-especial-tipo carta) "TrocaCor")) 50]
    [else 20]
    )
  )

(check-expect (quanto-vale C1) 1)
(check-expect (quanto-vale C3) 50)

;; soma-pontos: lista-de-cartas -> Numero
;; Obj: Dado uma lista-de-cartas, somar os pontos da lista de cartas retornando o total.
;; Exemplos:
;; (soma-pontos L1) = 8
;; (soma-pontos L2) = 69

(define (soma-pontos lc)
  (cond
    [(empty? lc) 0]
    [else (+ (quanto-vale (first lc)) (soma-pontos (rest lc)))]
    )
  )

(check-expect (soma-pontos L2) 69)
(check-expect (soma-pontos L1) 8)

;; vencedor: jogador jogador -> String Numero
;; Obj: Dado dois jogadores, computar qual deles é o vencedor, devolvendo o nome e a pontuação
;; Exemplos:
;;
;;

;;(define (vencedor jogador1 jogador2)
;;  (cond
;;    [(< (soma-pontos (jogador-lc jogador1)) (soma-pontos (jogador-lc jogador2))) (values (jogador-nome jogador1) (soma-pontos (jogador-lc jogador1)))]
;;    [else (values (jogador-nome jogador2) (soma-pontos (jogador-lc jogador2)))]
;;    )
;; )


;; ========================================================================
;;                                 QUESTÃO 6
;; Construa uma função que, dados a carta da mesa e um jogador, escolhe uma
;; carta do jogador para ser jogada, de acordo com as regras do Uno. Se a
;; carta escolhida for "Compra4" ou "TrocaCor", deve ser escolhida uma cor da
;; seguinte forma: se for jogado um "TrocaCor" para trocar a cor para amarelo,
;; a carta a ser jogada deve ser (make-carta-especial "amarelo" "TrocaCor"). 

;; As regras para escolha de cartas válidas para jogar são:
;; * Uma carta-numero pode ser escolhida se for da mesma cor ou do mesmo numero da carta da mesa;
;; * As cartas especiais "Inverte", "Compra2" e "NãoJoga" podem ser escolhidas se forem da mesma
;;   cor da carta da mesa ou tiverem o mesmo símbolo (mesmo tipo);
;; * As cartas especiais "TrocaCor" e "Compra4" sempre podem ser jogadas, mas deve-se escolher a cor.

;; Caso o jogador não tenha nenhuma carta que possa ser jogada, a função deve retornar
;; a mensagem "Não tenho cartas para jogar!"
;; =========================================================================

;; escolhe-cor: lista-de-cartas -> String
;; Obj: Dado uma lista de cartas, devolver a cor da parte majoritaria das cartas.
;; Exemplos:
;;
;;

(define (escolhe-cor lc)
  
  )

;; jogar: carta jogador -> cartaOUString
;; onde cartaOUString é:
;; 1. carta ou
;; 2. String
;; Obj: Dado a carta da mesa e um jogador, devolver uma carta a ser jogado. Caso não tenha
;; nenhuma que possa ser jogada, devolve uma mensagem "Não tenho cartas para jogar!".
;; Exemplos:
;;
;;



;; ========================================================================
;;                                 QUESTÃO 7
;; Construa a função mostra-jogada que, dados um jogador e a carta da mesa, mostra a
;; jogada escolhida (gera uma imagem com a jogada, incluindo o nome do jogador).
;; =========================================================================

;; ========================================================================
;;                                 QUESTÃO 8
;; Construa a função mostra-jogadas-possiveis que, dados um jogador e a carta da mesa,
;; mostra a lista de cartas que poderiam ser jogadas (gera uma imagem com as cartas da mão,
;; a carta da mesa, e as cartas possíveis de serem jogadas).
;; =========================================================================

