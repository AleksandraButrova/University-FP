import Debug.Trace
{--4.1
Описать функцию upDown, которая проверяет, верно ли, 
что в данном списке сначала строго возрастают, а потом, 
c какого-то момента, строго убывают.

Примеры вызова:
upDown [2, 6, 9, 7, 3, 1]
- должна вернуть True
upDown [2, 6, 9, 7, 3, 1, 8]
- должна вернуть False, потому что числа возрастают, убывают и потом снова возрастают.
upDown [2, 6, 9]
- должна вернуть False, потому что числа только возрастают

Уточнение:
М.б. лучше четко сформулировать, что это значит "сначала возрастают, потом убывают". 
Имеется в виду, что, если взять все пары стоящих рядом чисел и сравнить числа в парах, 
то сначала насколько раз (по крайней мере один раз) первое число будет меньше, 
а потом несколько раз (тоже по крайней мере один раз) первое число будет больше.--}

down _ [] _ = True
down x1 (x2:xs) done = if x1 > x2 && done == True
                       then down x2 xs done
                       else False

up _ [] _ = False
up x1 (x2:xs) done = if x1 < x2
                     then up x2 xs True
                     else down x1 (x2:xs) done

upDown [] = False  
upDown (x:xs) = up x xs False





{--4.2
Написать функцию parts, которая для данного списка проверяет, можно ли его разбить 
на несколько строго возрастающих кусков одинаковой длины. 

Длина кусков должна быть не меньше 2. Кусок может быть и только один.

Примеры вызова:
parts [1, 2, 8, 2, 5, 6]
parts [1, 2, 3, 4, 5]
parts [1, 2, 1, 2, 3, 4]
Во всех этих случаях ответ должен быть равен True.
parts [4, 6, 3, 5, 7]
Тут ответ должен быть равен False.

Замечание: В этой задаче есть более эффективные решения и менее эффективные. 
Если у вас будет менее эффективное, то я попрошу вас написать более эффективное 
(и немного подскажу, как).
--}


devideLenghtes (l:lenghtes) (m:mins) = any (\m -> all(\l -> mod l m == 0) (l:lenghtes)) (m:mins)

allDeviders num i devs = if  num >= i && mod num i == 0 
                         then allDeviders num (i+1) (i:devs)
                         else if num >= i
                              then allDeviders num (i+1) devs 
                              else devs


findSeq _ [] l lenghtes = l:lenghtes
findSeq x1 (x2:xs) l lenghtes = if x1 < x2
                                 then findSeq x2 xs (l+1) lenghtes
                                 else if l > 1
                                      then findSeq x2 xs 1 (l:lenghtes)  
                                      else [] 
minlist [] = 0
minlist [x] = x
minlist (x:xs) = min x (minlist xs)

parts [] = False
parts (x:xs) = if minList > 1
               then devideLenghtes lenList (allDeviders minList 2 [])
               else False   
                    where lenList = findSeq x xs 1 [] 
                          minList = minlist lenList

{-4.3
Написать функцию parts2, которая проверяет, 
можно ли в данном списке выбрать ровно половину элементов, чтобы
- они строго возрастали
- в оставшейся половине элементы тоже строго возрастали.
В этой задаче элементы не обязательно брать подряд.

Примеры вызова:
parts2 [1, 3, 2, 4, 8, 9]
Ответ должен быть True (разбивается, например, на 
[1, 3, 4] и [2, 8, 9])
parts2 [2, 3, 4, 6, 1, 8, 9, 7]
ответ должен быть False.

Замечания:
- в этой задаче решение должно быть более-менее эффективным. 
Т.е., если присланное решение покажется мне очень неэффективным, 
я могу попросить его улучшить, даже если всегда дает правильные результаты.

- на самом деле, я знаю очень эффективное решение, без перебора, 
но его не так просто запрограммировать на Хаскеле. 
Но если вы просто придумаете эффективный алгоритм, напишите письмо, 
было бы интересно сравнить решение с моим.-}




