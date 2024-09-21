#lang racket

; listみたいに二重quoteになっている
''a

''abracadabra 
(pair? ''abracadabra)
(cdr ''abracadabra)
(car ''abracadabra)

#|

補足
''abracadabraはSchemeのシステム内へは
(quote (quote abracadabra))として読み込まれ,
(car ''abracadabra)の評価時の引数評価でquoteが一つとれ, (quote abracadabra) としてcarに渡される. したがってそのcar,
つまりquoteが評価結果となる.

|#