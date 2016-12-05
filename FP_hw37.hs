{--*Доп.задача до 25.12*

а. Добавить в тип Expr конструкции, позволяющие определять свои функции и вызывать их. 
Для простоты мы рассматриваем только функции с одним параметром, параметр и результат функции - целые числа.
б. Добавить в функцию eval реализацию для этих конструкций.

Пример вызова:

eval (LetFunc "F" "X" (Mult (Var "X") (Var "X")) (Add (Num 1) (Call "F" (Num 5)))) [] []

Должно получиться 26, потому что мы тут записали на нашем языке выражение:
let f x = x*x in 1 + f 5

Замечания:
- Задача не очень простая, но, в общем то, тут надо придумать представление данных (как хранить функции) и дальше все,
 в общем, не сложно.
- Я бы предложил завести отдельный параметр - список для хранения функций. Поэтому в примере вызова у меня в конце два пустых списка - для переменных и для функций. Но, если хотите, можете немного изменить список для хранения значений переменных, чтобы в нем можно было хранить и функции. Тесты тогда не пройдут, ну и ладно, я так проверю. 
- Можно считать, что не надо поддерживать замыкания (т.е. в функциях не используются нелокальные переменные).--}

data Expr = Num Integer | Var String | Add Expr Expr | 
            Mult Expr Expr | Let String Expr Expr | LetFunc String String Expr Expr | Call String Expr

eval (Num i) values funcs = i
eval (Var x) [] funcs = 0
eval (Var x) (v:values) funcs = if x == fst v 
                                then eval (snd v) values funcs
                                else eval (Var x) values funcs
eval (Add x y) values funcs = eval x values funcs + eval y values funcs 
eval (Mult x y) values funcs = eval x values  funcs * eval y values funcs

eval (Let t ex y) values funcs = eval y ((t, ex) : values) funcs

eval (LetFunc f x ex a) values funcs =  eval a values ( (f, x, ex) : funcs)

eval (Call f ex) values ((letF, letX, letEx):funcs)  = if f == letF
                                                       then eval letEx [(letX, ex)] []
                                                       else eval (Call f ex) values (funcs) 