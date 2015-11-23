get =: 4 : 0
> y { x
)

getType =: 3 : 0
(y get 0) 
)

interp =: 4 : 0
expr =: x
env =: y
if. ((getType expr) = 'numC') do. (expr get 1)
NB. elseif. ((getType expr) = 'ifC) do. 
elseif. 1 do. 0
end.
)

assert (('numC' ; 10) interp ('Env' ; '')) = 10
NB. assert (interp (('binopC' ; '+' ; 5 ; 6) ; ('Env' ; ''))) = 3