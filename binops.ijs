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

NB. This is how we'll do tests
assert (binop addProg) = 7
assert (binop subProg) = 1
assert (binop divProg) = 1
assert (binop multProg) = 12
assert (binop lessEqProg) = 1
assert (binop lessEqProgFalse) = 0
assert (binop lessEqProgFalse) = 0
assert (binop Eq) = 1
assert (binop EqFalse) = 0

NB. This is an example recursive function
factorial=: 3 : 0
if. y = 1 do. 1
elseif. 1 do. (factorial y - 1) * y
end.
)

assert (factorial 1) = 1
assert (factorial 3) = 6

addProg get 0