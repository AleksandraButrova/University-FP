def LastDigits(a):
    return filter (lambda x: x % 3 == 0 and (x % 10) % 3 != 0, a)

print LastDigits([1, 10, 15, 21, 33, 300])          # ответ : 15, 21
