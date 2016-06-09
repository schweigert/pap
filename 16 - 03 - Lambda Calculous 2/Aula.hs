(λxλy.x)((λz.z)(λw.ww))((λa.aa)(λb.bb))

 - Modo preguisoso
> (λy.(λz.z)(λw.ww))   ((λa.aa)(λb.bb))
> (λyλw.ww)   ((wa.aa)   (λb.bb))
> λw.ww

 - Modo estrito
> (λxλy.x)((λz.z)(λw.ww))((λa.aa)(λb.bb)) 
> (λxλy.x)(((λw.ww)))(((λb.bb)(λb.bb)))
> (λxλy.x)((λw.ww))((λb.bb)(λb.bb))
> ((λw.ww))((λb.bb)(λb.bb))
> ((((λb.bb)(λb.bb))((λb.bb)(λb.bb))))

-- Booleanas

V = λxλy.x
F = λxλy.y

E = λxλy.xy(λuλv.v)

-- E V F
(λxλy.xy(λuλv.v)) (λcλd.c) (λeλf.f)
> (λy.(λcλd.c)y(λuλv.v))  (λeλf.f)
> ((λcλd.c)(λeλf.f)(λuλv.v)) 
> ((λd.(λeλf.f))(λuλv.v))
> λeλf.f 

-- E V V
(λxλy.xy(λuλv.v)) (λcλd.c) (λeλf.e)
> (λy.(λcλd.c)y(λuλv.v))  (λeλf.e)
> ((λcλd.c)(λeλf.e)(λuλv.v))  
> ((λcλd.c)(λf.(λuλv.v)))
> ((λd.(λf.(λuλv.v))))
> (λf.(λuλv.v))
> λuλv.v

-- Fatoreal em Lambda

fat = λn.if n=0 then 1 else n*fat(n-1)

-- Função de Ponto Fixo

FIX F = F(FIX F)

fat = FIX(λfλn.if n=0 then 1 else n*f(n-1))

fix = λf.(λx.f(xx)(λx.f(xx))

λf.((λx.f(xx))(λx.f(xx))F
> (λx.F(xx))(λx.F(xx))
> F(λx.F(xx)(λx.F(xx))

(FIX(λfλn.if n=0 then 1 else n*f(n-1)))2
> (λfλn.if n=0 then 1 else n*f(n-1))(FIX(λfλn.if n=0 then 1 else n*f(n-1))2
> (λn.if n=0 then 1 else n*(FIX(λfλn.if n=0 then 1 else n*f(n-1))(n-1))2
> (if 2=0 then 1 else 2*(FIX(λfλn.if n=0 then 1 else n*f(n-1))(n-1))
> (if 2=0 then 1 else 2*(FIX(λfλn.if n=0 then 1 else n*f(n-1))(2-1))
> 2*(FIX(λfλn.if n=0 then 1 else n*f(n-1))(2-1)
> 2*(FIX(λfλn.if n=0 then 1 else n*f(n-1))(1)
...
2*1*1 = 2


-- Haskell: Lambda

soma x y = x+y
soma = \x -> \y -> x+y