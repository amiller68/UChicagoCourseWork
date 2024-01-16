""" This file contains a function that will attempt shifting the cipher n times, where n is the length of the ciper. It will
    print all possibilities.
"""


import string

def shift():
    file = str(input())

    original_nums = []

    ctxt = open('ctxts/' + file).read()

    for i in ctxt:
        if i in string.ascii_lowercase:
            original_nums.append(ord(i) - 96)
        else:
            original_nums.append(i)

    print(original_nums)
    numbers = original_nums

    for i in range(1, 27):
        print("Shift: ", i)
        new_str = ""
        for num in numbers:
            if (isinstance(num, int)):
                new_str += chr(ord('`') + ((num - i) % 26))
            else:
                new_str += num


        print(new_str)

        numbers = original_nums

    
if __name__ == '__main__':
    shift()
