{--27.1
Определите тип Rat для представления дробей. Конструктор этого типа должен записываться так: 
Rat <числитель> <знаменатель>.

Для этого типа: 
a. Определите оператор + (сложение дробей)
b. Определите оператор == (сравнение дробей на равенство)
с. Определите оператор < (сравнение дробей)

Примеры вызова:

Rat 1 2 + Rat 1 7
Должно быть напечатано
Rat 9 14

Rat 1 3 == Rat 4 5
Результат должен быть равен False

Rat 1 3 < Rat 4 5
Результат должен быть равен True

Замечания:
- Надо ли сокращать дроби? Например, что будет, если сложить Rat 1 3 и Rat 1 6? 
Ответ: Как хотите, не обязательно сокращать. В тестах таких примеров не будет.
- Не забудьте в определении Rat написать deriving Show. Без этого тесты не пройдут.--}

data Rat = Rat Int Int deriving Show  

instance Eq Rat where
    (Rat num1 den1) == (Rat num2 den2) = num1 * den2 == num2 * den1 

instance Ord Rat where
    (Rat num1 den1) < (Rat num2 den2) = num1 * abs den2 * signum den1 < num2 * abs den1 * signum den2

instance Num Rat where
    (Rat num1 den1) + (Rat num2 den2) = Rat (num1 * abs den2 + num2 * abs den1) (den1 * den2) 
    
    
--class (Ord a, Eq a, Num a) => MyRat a where

          