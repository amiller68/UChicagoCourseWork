import string
import urllib.request
from Crypto.Cipher import AES
import binascii
import base64
import random
import os
import zlib
import json
import statistics
from collections import OrderedDict
import operator

################################################################################
# CS 284 Padding Utility Functions
################################################################################

# s is a bytearray to pad, k is blocklength
# you won't need to change the block length
def cmsc284pad(s,k=16):
    if k > 255:
        print("pkcs7pad: padding block length must be less than 256")
        return bytearray()
    n = k - (len(s) % k)
    if n == 0:
        n = k
    for i in range(1,n+1):
        s.extend([i])
    return s

# s is bytes to pad, k is blocklength
# you won't need to change the block length
def cmsc284padbytes(s,k=16):
    if k > 255:
        raise Exception("pkcs7pad: padding block length must be less than 256")
    n = k - (len(s) % k)
    if n == 0:
        n = k
    for i in range(1,n+1):
        s += chr(i).encode("utf-8")
    return s

# s is bytes to unpad, k is blocklength
# you won't need to change the block length
def cmsc284unpad(s,k=16):
    if not cmsc284checkpadding(s,k):
        print("cmsc284unpad: invalid padding")
        return b''
    n = s[len(s)-1]
    return s[:len(s)-n]

# checks padding on s and returns a boolean
# you won't need to change the block length
def cmsc284checkpadding(s,k=16):
    if(len(s) == 0):
       #print("Invalid padding: String zero length"%k) 
       return False
    if(len(s)%k != 0): 
       #print("Invalid padding: String is not multiple of %d bytes"%k) 
       return False
    n = s[len(s)-1]
    if n > k or n == 0:
       return False
    else: 
        for i in range(n):
            if s[len(s)-1-i] != (n-i):
                return False
    return True

################################################################################
# Function for querying the server
################################################################################

SERVER = "http://cryptoclass.cs.uchicago.edu/"
def make_query(task, cnetid, query):
    DEBUG = False
    if DEBUG:
        print("making a query")
        print("Task:", task)
        print("CNET ID:", cnetid)
        print("Query:", query)
    if (type(query) is bytearray) or (type(query) is bytes):
        url = SERVER + urllib.parse.quote_plus(task) + "/" + urllib.parse.quote_plus(cnetid) + "/" + urllib.parse.quote_plus(base64.urlsafe_b64encode(query)) + "/"
    else:
        url = SERVER + urllib.parse.quote_plus(task) + "/" + urllib.parse.quote_plus(cnetid) + "/" + urllib.parse.quote_plus(base64.urlsafe_b64encode(query.encode('utf-8'))) + "/"
    if DEBUG:
        print("Querying:", url)

    with urllib.request.urlopen(url) as response:
        raw_answer = response.read()
        answer = base64.urlsafe_b64decode(raw_answer)
        if DEBUG:
            print("Answer:", answer)
        return answer
    return None

################################################################################
# Bitwise XOR, stolen from Project 1 Starter code
################################################################################

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

################################################################################
# Problem 1 SOLUTION
################################################################################

# Construct a frequency analysis of the PRG pad
def find_i(cnetid, trials=10, num_bytes=32):
    # Construct a query to get the first num_bytes of a PRG pad
    query = bytearray(num_bytes)

    # Populate the indices of our Freq Dict
    res_dict = {}
    for num in range(num_bytes):
        res_dict[num] = {}

    for trial in range(trials):
        res = make_query(
            task='one',
            cnetid=cnetid,
            query=query
        )

        for ind in range(num_bytes):
            byte = res[ind]
            if byte in res_dict[ind]:
                res_dict[ind][byte] += 100.0 / trials
            else:
                res_dict[ind][byte] = 100.0 / trials

    with open("byte_biases.json", 'w') as bias_file:
        json.dump(res_dict, bias_file, indent=4)
    return


# Recover a specified byte of the flag, given the index of our biased byte 'i' and its bias
# Returns an int
def recover_flag_byte(cnetid, flag_ind, i_ind, trials=50):
    query = bytearray(i_ind - flag_ind)
    res_dict = {}
    for trial in range(trials):
        res = make_query(
            task='one',
            cnetid=cnetid,
            query=query
        )
        byte = res[i_ind]
        if byte in res_dict:
            res_dict[byte] += 100.0 / trials
        else:
            res_dict[byte] = 100.0 / trials

    # The index in question has a bias towards 0, so just choose the most common byte
    flag_byte = max(res_dict.items(), key=operator.itemgetter(1))[0]
    return flag_byte

def problem1(cnetid):
    #find_i(cnetid, trials=250, num_bytes=64)

    i_ind = 30  # Our default value

    flag_len = len(make_query(
        task='one',
        cnetid=cnetid,
        # Send a blank bytearray
        query=bytearray()
    ))

    # Impossible to recover the entire flag in this case
    if i_ind + 1 - flag_len < 0:
        return b''

    flag = bytearray()
    for flag_ind in range(flag_len):
        while True:
            byte = recover_flag_byte(
                cnetid,
                flag_ind,
                i_ind,
                trials=150
            )
            if bytearray([byte]).decode()[0] in string.printable:
                break
        flag.extend([byte])
    return flag


################################################################################
# Problem 2 SOLUTION
################################################################################

# Find the flag length using queries
def find_flag_length_2(cnetid):
    # Create an empty bytearray
    query = bytearray(0)

    # Form an initial guess on Flag length
    flag_len = len(make_query(task='two', cnetid=cnetid, query=query))
    # Determine how many blocks just the flag takes up
    flag_blocks = flag_len // 16

    # Query with greater length messages until we get another block in the response
    while True:
        query.extend([0])
        num_blocks = len(make_query(task='two', cnetid=cnetid, query=query)) // 16

        # If this is the case, we know the flag was aligned after the previous query
        if num_blocks > flag_blocks:
            break

        # Otherwise, we know our initial estimate of flag length was too big
        flag_len -= 1
    # Subtract one more because it takes oe more byte to actually get the flag to the end
    return flag_len - 1

# Isolate bytes of the flag such that byte i of the flag gets shifted to that start of the block we want to check
def iso_flag(flag_len, i):
    # How we need to pad in order to isolate the right amount of bytes, if the flag was aligned with the end of a block
    shift = i % 16
    if shift == 0:
        shift = 16
    # Amount to pad s.t. the last byte of the flag is at the end of a block
    pad = 16 - flag_len % 16
    return bytearray(shift + pad)

# Check if the block in which we are trying to determine the value of the i-th flag byte
# is equivalent to the block in which we encrypted our guess (the first block)
def is_match(res, i):
    if (i + 1) % 16 == 0 and i > 0:
        l = len(res) - ((i // 16) + 2) * 16
        r = len(res) - ((i // 16) + 1) * 16
    else:
        l = len(res) - ((i // 16) + 1) * 16
        r = len(res) - (i // 16) * 16
    return res[:16] == res[l:r]


def problem2(cnetid):
    flag_len = find_flag_length_2(cnetid)

    # We know the last fifteen bytes of the plaintext if we isolate one flag byte -- it's just the pad for one byte
    known_flag = bytearray(15)
    known_flag = cmsc284pad(bytearray(1))[1:16]

    # Initialize bytearrays to hold our guess space for the first byte of the flag
    guesses = []
    for i in range(256):
        guesses.append(bytearray(16))
        guesses[i][0] = i
        guesses[i][1:16] = known_flag

    flag = bytearray(flag_len)
    try:
        # for i in range(flag_len):
        for i in range(flag_len):  # Hardcoded to only guess one byte for now
            found_byte = False
            # print("Looking for Flag byte ", i)
            # print("Using known flag", known_flag)

            # Isolate (i + 1) % 16 flag bytes
            _iso_flag = iso_flag(flag_len, i + 1)
           #  print("Using isolation query: ", _iso_flag)

            # print("Unpadded res length: ", len(guesses[0] + _iso_flag) + flag_len)

            # Iterate through our guess space
            for guess in guesses:
                #print("Feeding guess: ", guess + _iso_flag)
                #print("Unpadded res length: ", len(guess + _iso_flag) + flag_len)
                res = make_query(
                    cnetid=cnetid,
                    task='two',
                    query=guess + _iso_flag
                )
                # print("Padded res length: ", len(res))

                if is_match(res, i):
                    found_byte = True
                    # This NEVER gets reached

                    # print("Found a byte: ", guess[0])
                    flag[flag_len - 1 - i] = guess[0]

                    # Update the bytes we use to constrain our guess space
                    known_flag[1:15] = known_flag[0:14]
                    known_flag[0] = guess[0]

                    # Generate new guesses, using our known_flag
                    for ind in range(len(guesses)):
                        guesses[ind][0] = ind
                        guesses[ind][1:16] = known_flag
                    break
            if not found_byte:
                print("Failed in Flag recovery")
                return flag
    except:
        print("oopsy")
    return flag


################################################################################
# Problem 3 SOLUTION
################################################################################

def problem3(cnetid):
    base_query = bytearray(b'password=')
    flag = bytearray(0)
    while True:
        base_len = len(make_query(
            task='three', cnetid=cnetid, query=base_query + flag + bytearray([0])
        ))
        # Guess all possible bytes here
        for guess_byte in range(1, 256):
            comp_len = len(make_query(
                task='three', cnetid=cnetid, query=base_query + flag + bytearray([guess_byte])
            ))
            # Break if we observe compression
            if base_len > comp_len:
                flag.extend([guess_byte])  # Add a new byte to guess
                break
        # Let's code some stopping logic here
        if flag[len(flag) - 1] == ord(';'):
            break
    return flag[:len(flag) - 1]  # Don't return the byte ';' at the end


################################################################################
# Problem 4 SOLUTION
################################################################################

# Find the pad len that byte aligns compressed blocks
def find_pad_len(cnetid, base_query):
    # Create a byte array from which we will test pads
    pad = cmsc284pad(bytearray(0))

    # How big of a result do we get from an empty query
    base_res_len = len(make_query(task='four', cnetid=cnetid, query=base_query))
    # How many blocks is that?
    base_blocks = base_res_len // 16

    for ind in range(1, len(pad) + 1):
        # try querying using a larger pad
        num_blocks = len(make_query(task='four', cnetid=cnetid, query=pad[0:ind] + base_query)) // 16
        if num_blocks > base_blocks:
            # If the previous query overflowed, we know we went too far
            return pad[0:ind - 1]
    return None

def problem4(cnetid):
    # Find the largest non-compressable pad s.t. we don't overflow into another block
    # We need to provide a portion of M which we know precedes FLAG
    base_query = bytearray(b'password=')
    good_pad = find_pad_len(cnetid, base_query)

    # Oops! We made an error
    if good_pad is None:
        return b''


    # Initialize a flag to hold our answer
    flag = bytearray(0)

    while True:
        # Extend our guess with a byte we know isn't in the flag, \x00 in this case
        # This query should cause the server's response to overflow into an additional block
        base_blocks = len(make_query(
            task='four', cnetid=cnetid, query=good_pad + base_query + flag + bytearray([0])
        )) // 16

        # Guess all possible bytes here
        for guess_byte in range(1, 256):
            # Submit a new query with a different guess
            comp_blocks = len(make_query(
                task='four', cnetid=cnetid, query=good_pad + base_query + flag + bytearray([guess_byte])
            )) // 16

            # Did the byte get compressed?
            if base_blocks > comp_blocks:
                # Then add it to our flag
                flag.extend([guess_byte])
                break
        # Let's code some stopping logic here
        if flag[len(flag) - 1] == ord(';'):
            break
    return flag[:len(flag) - 1]  # Don't return the bytes ';\x00' at the end

################################################################################
# Problem 5 SOLUTION
################################################################################

def problem5(cnetid):
    # Query for the cipher text we're going to abuse
    # Our response should contain b'username=davidcash&uid=133&role=professor'
    prof_ctxt = make_query(task='fivea', cnetid=cnetid, query="")
    # Extract david's credential from the cipher text
    cred = prof_ctxt[32:48]

    # why not use my own cnet here, which aligns with davidcash
    uname_inject = bytearray(b'amiller68&uid=133')
    inject_ctxt = make_query(task="fiveb", cnetid=cnetid, query=uname_inject)

    # Finally, send a query which will grant as a success message
    res = make_query(task="fivec", cnetid=cnetid, query=inject_ctxt[:32] + cred)
    return res

################################################################################
# Problem 6 SOLUTION
################################################################################

def problem6(cnetid):
    # The message needed to generate a success
    target_message = bytearray(b'let me in please')

    # Lets recover the key with this pernicious cipher text
    ctxt = bytearray(32)
    dec_ctxt = make_query(task="sixb", cnetid=cnetid, query=ctxt)

    # We claim that our key is equivalent to:
    key = bitwise_xor(dec_ctxt[0:16], dec_ctxt[16:32])

    # Let's CBC encrypt our target message

    # Create a new CBC cipher, and set the iv to our recovered key
    cipher = AES.new(bytes(key), AES.MODE_CBC, bytes(key))
    target_ctxt = bytearray(cipher.encrypt(bytes(cmsc284pad(target_message))))

    # Let's see if this generates a success
    res = make_query(task='sixc', cnetid=cnetid, query=target_ctxt)
    return res

################################################################################
# Problem 7 SOLUTION
################################################################################

# Helper function for generating useful queries to sevenb
def format_enc_flag(enc_flag, flag, ind):
    # Let's bound and truncate our enc_flag as we go
    ret = bytearray(((ind // 16) + 1) * 16)
    ret = enc_flag[:len(ret)]

    # We want to format enc_flag s.t. it decrypts without a padding error
    target_pad = cmsc284pad(bytearray(ind % 16))

    for i in range(len(target_pad)):
        # Read this in reverse order
        pad_byte = target_pad[len(target_pad) - (i + 1)]
        # Break when we get to the end
        if pad_byte == 0:
            break
        # print("b: ", flag[len(ret) - ((i % 16) + 1)], ", c': ", pad_byte)
        ret[len(ret) - (i + 17)] = flag[len(ret) - (i + 1)] ^ pad_byte
        # print("b ^ c' = ",  ret[len(ret) - (i + 17)])
        ind += 1
    return ret


def recover_flag(enc_flag, inter_c):
    return bitwise_xor(enc_flag[:len(enc_flag) - 16], inter_c[16:len(inter_c)])


def problem7(cnetid):
    success = bytearray(b'true')
    # Query for our encrypted flag
    enc_flag = bytearray(make_query(task="sevena", cnetid=cnetid, query=""))

    # Initialize a byte array to hold our intermediary cipher text
    inter_c = bytearray(len(enc_flag))  # we'll trim the iv later, for now we need the space
    try:
        # Now let's populate 'inter_c' with the intermediary ctxt for each block
        for i in range(len(inter_c) - 16):  # here, we don't want to
            fail = True
            # What index of the flag are we breaking?
            ind = len(inter_c) - (i + 1)
            for byte in range(256):
                # Write in our guess of what this could be
                inter_c[ind] = byte
                # Format a cipher text s.t. it would have good padding if our guess is correct
                query = format_enc_flag(enc_flag, inter_c, ind)
                # And check!
                res = make_query(task='sevenb', cnetid=cnetid, query=query)
                if res == success:
                    fail = False
                    # print("Found a byte: ", recover_flag(enc_flag, inter_c))
                    break
            if fail:
                print("Failed in recovering Flag")
                return b''
        # Recover and return our flag
        return cmsc284unpad(recover_flag(enc_flag, inter_c))
    except Exception as e:
        print("oopsy: ", e)
        return b''

test_set = [
    problem1,
    problem2,
    problem3,
    problem4,
    problem5,
    problem6,
    problem7
]

if __name__ == "__main__":
    # Set a CnetID to use
    cnetid = 'amiller68'

    try:
        for test in test_set:
            flag = test(cnetid) # test(cnetid)
            # print(test.__name__, " flag:")
            print(flag.decode())
    except UnicodeError:
        print(test.__name__, ": Invalid Flag")

