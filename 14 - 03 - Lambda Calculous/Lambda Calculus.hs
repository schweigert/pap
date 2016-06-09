(λs.x+1)s
> 5+1
> 6

(λx.x+x)4
> 4+4
> 8

Normas de Chunch

0 = λsλz.z
1 = λsλz.s(z)
2 = λsλz.s(s(z))
3 = λsλz.s(s(s(z)))
4 = λsλz.s(s(s(s(z))))

sucessor = λwλyλx.y(wyx)

adição = λxλyλwλy.xw(ywu)


Currificação (Curring)

suc = λwλyλx.y(wyx)

(λwλyλx.y(wyx))(λs.λz.z)
> λyλx.y((λsλz.z)yx)
> λyλx.y((λz.z)x)
> λyλx.y(x)
> λsλz.s(z)
> 1

suc(4)
> (λwλyλx.y(wyx))(λsλz.s(s(s(s(z)))))
> (λyλx.y((λsλz.s(s(s(s(z)))))yx))
> (λyλx.y((λz.y(y(y(y(z)))))x))
> (λyλx.y((y(y(y(y(x)))))))
> λyλx.y((y(y(y(y(x))))))
> λsλz.s(s(s(s(s(z)))))
> 5

adicao(1,2)
> (λxλyλwλu.xw(ywu))   (λsλz.s(z))   (λsλz.s(s(z)))
> (λyλwλu.(λsλz.s(z))w(ywu))   (λsλz.s(s(z)))
> (λyλwλu.(λz.w(z))(ywu))   (λsλz.s(s(z)))
> (λyλwλu.(w((ywu))))   (λsλz.s(s(z)))
> (λwλu.(w(((λz.w(w(z)))u))))
> λwλu.(w(w(w(u))))
> λwλu.(w(w(w(u))))
> 3