first =: 3 : 0
> 0 { y
)
second =: 3 : 0
> 1 { y
)
third =: 3 : 0
> 2 { y
)

addProg=: '+' ; 5 ; 2
subProg=: '-' ; 2 ; 1
divProg=: '/' ; 2 ; 2
multProg=: '*' ; 4 ; 3

binop=: 3 : 0
if. ((first y) = '+') do. ((second y) + (third y))
elseif. ((first y) = '-') do. ((second y) - (third y))
elseif. ((first y) = '/') do. ((second y) % (third y))
elseif. ((first y) = '*') do. ((second y) * (third y))
elseif. 1 do. 0
end.
)

assert (binop addProg) = 7
assert (binop subProg) = 1
assert (binop divProg) = 1
assert (binop multProg) = 12

factorial=: 3 : 0
if. y = 1 do. 1
elseif. 1 do. (factorial y - 1) * y
end.
)

assert (factorial 1) = 1
assert (factorial 3) = 6