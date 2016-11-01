{-7.1
Пусть у нас есть лист в клетку. Вертикали и горизонтали в нем перенумерованы. 
Фигуру из клеток мы задаем, как список из пар целых чисел, например, 
[(3,5),(3,6),(4,5),(4,6)] задает квадрат 2 на 2. 
Клетки могут задаваться в любом порядке, например [(3,5),(4,6),(4,5),(3,6)] 
описывает тот же квадрат. 
Описать функцию getPerimeter, которая для данной фигуры возвращает ее периметр.

Примеры вызова:
getPerimeter [(1,2)] - должно получиться 4
getPerimeter [(1,2),(2,3)] - должно получиться 8
getPerimeter [(3,5),(4,6),(4,5),(3,6)] - должно получиться 8-}
import Data.List

getEdgeList [s] edgeList = (fst s, snd s, fst s - 1, snd s) : 
                           (fst s, snd s, fst s, snd s - 1) :
                           (fst s - 1, snd s, fst s - 1, snd s - 1) : 
                           (fst s, snd s - 1, fst s - 1, snd s - 1) : edgeList

getEdgeList (s:shape) edgeList = getEdgeList shape ((fst s, snd s, fst s - 1, snd s) : 
                                                    (fst s, snd s, fst s, snd s - 1) :
                                                    (fst s - 1, snd s, fst s - 1, snd s - 1) : 
                                                    (fst s, snd s - 1, fst s - 1, snd s - 1) : edgeList)


deleteOne c [] nlst = nlst
deleteOne c (l:lst) nlst = if c == l
                           then delete l lst
                           else l:nlst

deleteRepeated [] nlst = nlst
deleteRepeated (l:lst) nlst = deleteRepeated lst (deleteOne l lst nlst)--map (\l -> deleteOne l lst nlst) lst 

getPerimeter shape = deleteRepeated (getEdgeList shape []) []




{-Как и в задаче 7-1, мы описываем фигуры на листе в клетку с помощью 
списка пар целых чисел.

Описать функцию areEqual, которая проверяет, являются ли две фигуры равными.
То есть, можно ли одну получить из другой с помощью сдвигов, поворотов, 
зеркального отражения, ну и, конечно, изменения порядка элементов в списке.

Примеры вызова:
areEqual [(1,2),(1,3),(1,4)] [(4,0),(3,0),(5,0)]
areEqual [(1,2),(1,3),(1,4),(2,4)] [(1,2),(1,3),(1,4),(2,1)]
areEqual [(1,2),(1,6)] [(10,6),(10,2)]
- во всех этих вызовах ответ должен быть равен True-}