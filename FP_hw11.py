# Извините, я настолько быа увлечена проблемой каррирования, 
# что забыла о условиях задачи. :)

def CheckDigitCurried(n):
    def CheckDigit(dig):
        if dig % 10 == n:
            return True
        else :
            if (dig == 0):
                return False
            else :
                return CheckDigit(dig / 10)
    return CheckDigit

print(all(CheckDigitCurried(7)(x) for x in range (7, 78, 10)))  # True
print(all(CheckDigitCurried(7)(x) for x in range (0, 9)))       # False
print(any(CheckDigitCurried(7)(x) for x in range (1, 21, 3)))   # True
print(any(CheckDigitCurried(7)(x) for x in range (1, 21, 7)))   # False
print(CheckDigitCurried(7)(71))                                 # True
print(CheckDigitCurried(7)(11))                                 # False 
    
