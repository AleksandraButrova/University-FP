def CheckDigitCurried(n):
    def CheckDigit(dig):
        if dig % n == 0:
            return True
        else :
            return False
    return CheckDigit

print(all(CheckDigitCurried(7)(x) for x in range (0, 21, 7)))   # True
print(all(CheckDigitCurried(7)(x) for x in range (0, 21, 8)))   # False
print(any(CheckDigitCurried(7)(x) for x in range (1, 21, 3)))   # True
print(any(CheckDigitCurried(7)(x) for x in range (1, 21, 7)))   # False
print(CheckDigitCurried(7)(14))                                 # True
print(CheckDigitCurried(7)(15))                                 # False 
    
