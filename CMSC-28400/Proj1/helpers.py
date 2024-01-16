import string
import collections as col
import more_itertools

# https://stackoverflow.com/questions/1162592/iterate-over-a-string-2-or-n-characters-at-a-time-in-python
def groups_of_n(n, s):
    return more_itertools.chunked(s, n)

# Counts the number of characters in an ascii string
def count_letters(s):
    filtered = [c for c in s.lower() if c in string.ascii_letters]
    return col.Counter(filtered)


# Returns true is a text is encoded in hex
# Returns false if a piece of text is only numbers
def is_hex(text):
    return all(c in string.hexdigits for c in text) and not text.isdecimal()


# Turn a string of hex characters into printable hex code
def read_hex(hex_string):
    bytes_object = bytes.fromhex(hex_string)
    return bytes_object.decode("ASCII")

