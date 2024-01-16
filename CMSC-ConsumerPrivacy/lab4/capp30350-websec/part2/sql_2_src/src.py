import hashlib
import re

# targets = '\' or 1=1--  OR '\' or 1=1 --
candidate = 0
while True:
    plaintext = str(candidate)
    hash = hashlib.md5(plaintext.encode('ascii'))
    # See if we get something interprettable
    try:
        hash_str = hash.digest().decode()
        ret = hash_str
        hash_str = hash_str.lower()
        # split our candidate up into tokens we can test separatley
        quote_split = hash_str.split('\'')
        or_split  = hash_str.split('or')
        eq_split = hash_str.split('=')
        comm_split = hash_str.split('-- ')

        # The second part has whitespace before it!
        if quote_split[1].startswith(' '):
            or_split = quote_split[1].split()
            # we found a valid or token
            if or_split[0] == 'or':
                eq_token = or_split[1]
                eqs = eq_token.split('=') # wont cover tokens with multiple eqs, but...
                try:
                    eq_0 = int(eqs[0])
                    eq_1 = int(eqs[1])
                    if eq_0 == eq_1:
                except:
                    pass


                if


        # Check if we escape the password check
        if (tokens[0])
    except:
        pass
    # Move on to the next candidate
    candidate = candidate + 1