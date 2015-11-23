NB.These are comments

NB.This is how we'll define simple functions that look like random symbols.
first =: 3 : 0
> 0 { y
)
second =: 3 : 0
> 1 { y
)
third =: 3 : 0
> 2 { y
)
get =: 4 : 0
> y { x
)
getType =: 3 : 0
(y get 0) 
)

do =: 0 !: 1

NB. These are variables that are boxes with three things in them
NB. These can be passed to the binops function.
addProg=: '+' ; 5 ; 2
subProg=: '-' ; 2 ; 1
divProg=: '/' ; 2 ; 2
multProg=: '*' ; 4 ; 3
lessEqProg=: '<=' ; 3 ; 4
lessEqProgFalse=: '<=' ; 9 ; 4
Eq=: 'eq?' ; 4 ; 4
EqFalse=: 'eq?' ; 4 ; 'ds'

NB. Takes in a symbol and two arguments and evaluates the binop.
binop=: 3 : 0
if. ((first y) -: '+') do. ((second y) + (third y))
elseif. ((first y) -: '-') do. ((second y) - (third y))
elseif. ((first y) -: '/') do. ((second y) % (third y))
elseif. ((first y) -: '*') do. ((second y) * (third y))
elseif. ((first y) -: '<=') do. ((second y) <: (third y))
elseif. ((first y) -: 'eq?') do. ((second y) -: (third y))
elseif. 1 do. 0
end.
)

interp =: 4 : 0
expr =. x
env =. y
if. ((getType expr) -: 'numC') do. ('numV' ; (expr get 1))
elseif. ((getType expr) -: 'boolC') do. 
    if. ((expr get 1) -: 'true') do. ('boolV' ; 1) 
    elseif. ((expr get 1) -: 'false') do. ('boolV' ; 0)
    elseif. 1 do. throw
    end.
elseif. ((getType expr) -: 'binopC') do. 
    ('numV' ; (binop ((second expr) ; (((third expr) interp env) get 1) ; (((expr get 3) interp env) get 1))))
elseif. ((getType expr) -: 'ifC') do. 
     test =. ((expr get 1) interp env)
     if. (getType test) -: 'boolV' do.
        if. (test get 1) do. ((expr get 2 interp env) 
        elseif. 1 do. ((expr get 3) interp env)
        end.
     elseif. 1 do. throw
     end.
elseif. 1 do. throw
end.
)

assert ('numC' ; 10) interp ('Env' ; '') = ('numV' ; 10)
assert ('binopC' ; '+' ; ('numC' ; 5) ; <('numC' ; 6)) interp ('Env' ; '') -: ('numV' ; 11)


assert (binop addProg) = 7
assert (binop subProg) = 1
assert (binop divProg) = 1
assert (binop multProg) = 12
assert (binop lessEqProg) = 1
assert (binop lessEqProgFalse) = 0
assert (binop lessEqProgFalse) = 0
assert (binop Eq) = 1
assert (binop EqFalse) = 0