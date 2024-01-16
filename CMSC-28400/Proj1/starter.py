import sys

# global variable
# Note the 'b' before quotation mark. This is discussed below.
x = b"abcdef"

""" 
This is a function definition. Note that in python white-space is significant!
The code for the function and loop must be indented, either spaces or tabs.  
"""
def bitwise_xor(s1, s2):
    r = bytearray()
    """
    Python has some neat constructs for loops. You can read about the
    "for .. in .." and "zip" constructs elsewhere. It is also possible
    to write loops in a C-like way if you prefer.
    """
    for c1,c2 in zip(s1,s2): 
        r.extend([c1^c2])
    return r

# The following line is a set of magic words that gives Python a "main" 
# function.
if __name__ == "__main__":

    """
    Python3 is very different from C in that it makes distinguishes between
    "string"s and "bytearray"s as datatypes.
    """

    # note you can see the NULL bytes in the output this way
    print(bitwise_xor(x,x)) 

    # .decode() will covert the bytearray to a string.  Since NULLs are
    # non-printable, you should not see anything in the output.
    print(bitwise_xor(x,x).decode()) 


    # this quits the program, so the code below won't execute
    exit()

    ############################################################################

    """
    File IO is easy. You can just read the entire file in. The two lines
    below will read a file in as a string.
    """
    ptxtfile = open("filename.txt")
    ptxt_str = ptxtfile.read()
    
    # The next line will convert the string to a bytearray, suitable for
    # stuff like XOR.
    ptxt_bytes = ptxt_str.encode('utf-8')

    ############################################################################

    """
    Here is another example of a quirk of Python3. The following line will
    throw an error, because when s is a string, s[0] is NOT a byte/number,
    and you can't add to it.
    """
    s = "abc"
    print(s[0]+1)
    # instead, you need to use ord and chr as follows:
    print(chr(ord(s[0])+1))

    """
    When s is a bytearray or a so-called "bytes-like object", addition works.
    Note that the 'b' before the quotation mark indicates a bytes object, and
    NOT a string.
    """
    s = b"abc"
    print(s[0]+1)
    # and you need to use chr to convert the result into printable character
    print(chr(s[0])+1)


