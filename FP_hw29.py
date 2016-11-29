'''В этот раз задача с довольно свободным условием, и совсем не сложная.
а. Определите на C# (или на вашем любимом обычном языке) тип или класс, позволяющий описывать двоичные деревья, с вершинами, содержащими целые числа.
б. Определите функцию или метод Find, который для данного дерева и для данного условия (логической функции) ищет в дереве элемент, удовлетворяющий условию, и возвращает его. Если таких элементов несколько, вы можете вернуть любой, который вам удобнее.
в. Если в дереве нет таких элементов, функция должна об этом сообщить, причем так, чтобы в программе можно было понять, нашли мы что-то или нет. (Вывод сообщения на консоль, например, тут не подойдет).
г. Приведите два примера вызова Find, один успешный, который что-то находит, и другой не успешный.'''

class BiTree:
    u'Класс, описывающий двоичные деревья с вершинами, содержащими целые числа'
    def __init__(self, v, l = "undef", r = "undef"):
        self.value = v
        self.left = l
        self.right = r
    
def Find(func, tree):
    def wrapped():
        if   func(tree.value):
            return tree.value
        elif tree.left != "undef" and Find(func, tree.left) != "not found":
            return Find(func, tree.left)
        elif tree.right != "undef" and Find(func, tree.right) != "not found":
            return Find(func, tree.right)
        else:
            return "not found"
    return wrapped()




tree1 = BiTree(8, "undef", "undef")
tree2 = BiTree(8, BiTree(42, "undef", "undef"), "undef")

print(Find((lambda x: True if x >= 42 else False), tree1))
print(Find((lambda x: True if x >= 42 else False), tree2))