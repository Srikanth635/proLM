class Student:
    def __init__(self,name:str, id:int):
        self.name = name
        self.id = id

    def prints(self):
        return self.name + " " + str(self.id)