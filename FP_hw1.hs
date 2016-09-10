-- 1-1
f 1 = 1 + 1 / 1
f n = 1 + 1 / (f (n-1))

-- 1-2
b' 1 p = 1/p
b' n p = 1/ ( (p - n + 1) + b' (n-1) p )

b 0 = 0
b n = b' n n

-- 1-3
sum1 0 1 = 1
sum1 0 g = g^2 + sum1 0 (g-1)
sum1 1 0 = 1
sum1 a 0 = a + sum1 (a-1) 0
sumsqr n = (sum1 n 0 )^2 / sum1 0 n

-- I think it's better
--sumsqr n = ((1+n)*n/2)^ 2 / (n*(n+1)*(2*n+1)/6)

-- 1-4
-- 1! + 2! + 3! + 4! == 1(1 + 2(1 + 3(1+ 4)))
sf 1 p = p
sf n p = (p - n + 1)*(1 + sf (n-1) p)
sumfact n = sf n n




