#17-1
def ZeroDigits (a, n):
    pow = 10**n
    return map( (lambda x:  x / pow * pow % 10 if x / pow != 0 else 0) , a)

print ZeroDigits([1, 21, 321, 4321, 54321], 2)

