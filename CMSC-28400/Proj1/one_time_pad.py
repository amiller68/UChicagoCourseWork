import codecs
from starter import bitwise_xor
import functools
import random
import binascii
import json

# List of appropriate length decodes to alert us to
# Only include base strings we can expand
good_decodes = {

    # High frequency letters
    # 1: [
    #     "e",
    #     "t",
    #     "a",
    #     "o",
    #     "i"
    # ],
    # # High frequency digrams
    2: [
        "it",
        "in",
        "is",
        "on",
        "to",
        "so",
        "go",
        "ed",
        "th",
        "he",
        "in",
        "en",
        "nt",
        "re",
        "er",
        "an",
        "ti",
        "es",
        "on",
        "at",
        "se",
        "nd",
        "or",
        "ar",
        "al",
        "te",
        "co",
        "de",
        "to",
        "ra",
        "et",
        "ed",
        "it",
        "sa",
        "em",
        "ro"
    ],
    2: [
        "it",
        "in",
        "is",
        "on",
        "to",
        "so",
        "go",
        "ed",
        "th",
        "he",
        "in",
        "en",
        "nt",
        "re",
        "er",
        "an",
        "ti",
        "es",
        "on",
        "at",
        "se",
        "nd",
        "or",
        "ar",
        "al",
        "te",
        "co",
        "de",
        "to",
        "ra",
        "et",
        "ed",
        "it",
        "sa",
        "em",
        "ro"
    ],

    # High frequency trigrams
    3: [
        "too",
        "the",
        "can",
        "sir",
        "foo",
        "red",
        "the",
        "and",
        "tha",
        "ent",
        "ing",
        "ion",
        "tio",
        "for",
        "nde",
        "has",
        "nce",
        "edt",
        "tis",
        "oft",
        "sth",
        "men"
    ],
    4: [
        "east",
        "your",
        "hell",
        "tion",
        "that",
        "ther",
        "than",
        "with",
        "tion",
        "here",
        "ould",
        "ight",
        "have",
        "hich",
        "whic",
        "this",
        "thin",
        "they",
        "atio",
        "ever",
        "from",
        "ough",
        "were",
        "hing",
        "ment",
        "then"
    ],
    5: [
        "least",
        "beast",
        "feast",
        "hello",
        "ofthe",
        "andth",
        "ction",
        "ation",
        "ndthe",
        "which",
        "inthe",
        "onthe",
        "these",
        "there",
        "edthe",
        "after",
        "ingth",
        "their",
        "eofth",
        "tothe",
        "tiona",
        "about",
        "ngthe",
        "orthe",
        "erthe",
        "other",
        "forth",
        "ional",
        "atthe",
        "ingto",
        "first",
        "tions",
        "theco",
        "would"
    ],
    6: [],
    7: [],
    8: [],
    # These are things I think are correct
    "PRIORITY":[
        "jeans"
    ]
}

def expand_decodes(good_decodes):
    for (len, guesses) in good_decodes.items():
        if len == "PRIORITY":
            continue
        for guess in guesses:
            if guess[0] != ' ' and guess[len - 1] != ' ':
                good_decodes[len + 1].append(" " + guess)
                good_decodes[len + 1].append(guess + " ")
                good_decodes[len + 2].append(" " + guess + " ")
    return good_decodes

# tests against all possible strings that exist inside
def is_good_decode(res):
    for guess in good_decodes["PRIORITY"]:
        if guess.encode() == res:
            return True
    for guess in good_decodes[len(res)]:
        if guess.encode() == res:
            return True


def print_guess(bytes, i, l):
    r = bytearray()
    for x in range(len(bytes)):
        b = bytes[x]
        if i == x:
            r.extend([91])
        if (i + l) == x:
            r.extend([93])
        r.extend([b])
    print(r.decode())
    return


def auto_xor(word_inputs, replace=False, restrict_decodes=True, g_carry=None,p_carry=None):
    file_1 = "02.txt"  # str(input())
    file_2 = "13.txt"  # str(input())

    # Read our ctxts in as hex strings
    bytes_1 = bytearray.fromhex(open('ctxts/' + file_1).read())
    bytes_2 = bytearray.fromhex(open('ctxts/' + file_2).read())

    xor = bitwise_xor(bytes_1, bytes_2)

    print("Lets start decoding!")

    # These accumalate portions of guesses and corresponding plaintext
    ptxt_acc = bytearray()
    guess_acc = bytearray()
    new_ptxt_acc = bytearray()
    new_guess_acc = bytearray()
    for i in range(len(xor)):
        ptxt_acc.extend([95])  # These are underscores
        guess_acc.extend([95])  # These are underscores
        new_ptxt_acc.extend([95])  # These are underscores
        new_guess_acc.extend([95])  # These are underscores

    if p_carry:
        ptxt_acc[:] = p_carry
    if g_carry:
        guess_acc[:] = g_carry

    for word_str in word_inputs:
        # print("Guessing that '" + word_str + "' is in one of the messages")
        word = word_str.encode()  # Encode a word we think is in one of the msg texts

        # Init some byte arrays
        blank = bytearray()
        nulls = bytearray()
        ptxt_sub = bytearray()
        for i in range(len(word)):
            blank.extend([95])  # These are underscores
            nulls.extend([48])  # These are '0's
            ptxt_sub.extend([0])

        for i in range(len(xor) - len(word)):
            skip = False

            xor_sub = xor[i:i+len(word)]  # Get a section of the xor'ed ctxt to work with
            guess = bitwise_xor(xor_sub, word)  # Generate a guess for this section of the other message text

            # Replace any bytes that xored to the same value
            # This indicates that the xor'ed ctxt is a null byte -> the msg txts are the same here

            #if guess.decode() not in good_decodes["PRIORITY"]:
            x = i
            for (w, g) in zip(word, guess):
                if w == g:
                    ptxt_acc[x] = 48
                    guess_acc[x] = 48
                    skip = True
                    break
                x += 1

            ptxt_sub[:] = ptxt_acc[i:i + len(word)]
            # Is our guessed message text in our dictionary of likely examples?

            if restrict_decodes:
                usable_guess = is_good_decode(guess)
            else:
                usable_guess = guess.decode().isalpha()

            if not skip and usable_guess:
                # If True, also true for guess_acc_sub
                # Covers cases in which a larger guess expands on a smaller guess
                if ptxt_sub == blank or ptxt_sub.decode().strip('_').encode() in guess:
                    ptxt_acc[i:i + len(word)] = guess
                    guess_acc[i:i + len(word)] = word
                # In this case, we know we're guessing too much here
                elif 48 in ptxt_sub:
                    continue
                # In this case, we have conflicting guesses
                else:
                    # Copy and replace our accumalators with our guess
                    new_ptxt_acc[:] = ptxt_acc
                    new_guess_acc[:] = guess_acc
                    new_ptxt_acc[i:i + len(word)] = guess
                    new_guess_acc[i:i + len(word)] = word

                    # No change, continue
                    if (b'0' in ptxt_sub) or (ptxt_acc == new_guess_acc and guess_acc == new_ptxt_acc):
                        continue

                    # If we want to replace strings...
                    if replace:
                        ptxt_acc[:] = new_ptxt_acc
                        guess_acc[:] = new_guess_acc

    # print("Our accumulated guesses:")
    # print(guess_acc.decode())
    # print("Our accumulated plaintext:")
    # print(ptxt_acc.decode())
    return guess_acc, ptxt_acc


def manual_xor(word_inputs, replace=False, restrict_decodes=True, auto_good=False, g_carry=None,p_carry=None):
    file_1 = "02.txt"  # str(input())
    file_2 = "13.txt"  # str(input())

    # Read our ctxts in as hex strings
    bytes_1 = bytearray.fromhex(open('ctxts/' + file_1).read())
    bytes_2 = bytearray.fromhex(open('ctxts/' + file_2).read())

    xor = bitwise_xor(bytes_1, bytes_2)

    print("Lets start decoding!")

    # These accumalate portions of guesses and corresponding plaintext
    ptxt_acc = bytearray()
    guess_acc = bytearray()
    new_ptxt_acc = bytearray()
    new_guess_acc = bytearray()
    for i in range(len(xor)):
        ptxt_acc.extend([95])  # These are underscores
        guess_acc.extend([95])  # These are underscores
        new_ptxt_acc.extend([95])  # These are underscores
        new_guess_acc.extend([95])  # These are underscores

    if p_carry:
        ptxt_acc[:] = p_carry
    if g_carry:
        guess_acc[:] = g_carry

    for word_str in word_inputs:
        # print("Guessing that '" + word_str + "' is in one of the messages")
        word = word_str.encode()  # Encode a word we think is in one of the msg texts

        # Init some byte arrays
        blank = bytearray()
        nulls = bytearray()
        ptxt_sub = bytearray()
        for i in range(len(word)):
            blank.extend([95])  # These are underscores
            nulls.extend([48])  # These are '0's
            ptxt_sub.extend([0])

        for i in range(len(xor) - len(word)):
            skip = False

            xor_sub = xor[i:i+len(word)]  # Get a section of the xor'ed ctxt to work with
            guess = bitwise_xor(xor_sub, word)  # Generate a guess for this section of the other message text

            # Replace any bytes that xored to the same value
            # This indicates that the xor'ed ctxt is a null byte -> the msg txts are the same here
            if word == guess:
                ptxt_acc[i:i + len(word)] = nulls
                break

            ptxt_sub[:] = ptxt_acc[i:i + len(word)]
            # Is our guessed message text in our dictionary of likely examples?

            if restrict_decodes:
                usable_guess = is_good_decode(guess)
            else:
                usable_guess = all([c.isalnum() or c in " .,?:()\n" for c in guess.decode()])
                if usable_guess and not auto_good:
                    print("Does this reeealllly look usable?: ", guess.decode())
                    hmm = str(input())
                    usable_guess = hmm == "y"

            if not skip and usable_guess:
                # If True, also true for guess_acc_sub
                # Covers cases in which a larger guess expands on a smaller guess
                if all([c == 95 or c == 48 for c in ptxt_sub]):
                    ptxt_acc[i:i + len(word)] = guess
                    guess_acc[i:i + len(word)] = word
                # In this case, we have conflicting guesses
                else:
                    # Copy and replace our accumalators with our guess
                    new_ptxt_acc[:] = ptxt_acc
                    new_guess_acc[:] = guess_acc
                    new_ptxt_acc[i:i + len(word)] = guess
                    new_guess_acc[i:i + len(word)] = word

                    # No change, continue
                    if ptxt_acc == new_guess_acc and guess_acc == new_ptxt_acc:
                        continue

                    print("Which set of texts looks more correct? (1/2)")
                    print("1.")
                    print("Ptxt Acc: ")
                    print_guess(ptxt_acc, i, len(word))
                    print("Guess Acc: ")
                    print_guess(guess_acc, i, len(word))


                    print("2.")
                    print("New Ptxt Acc: ")
                    print_guess(new_ptxt_acc, i, len(word))
                    print("New Guess Acc: ")
                    print_guess(new_guess_acc, i, len(word))

                    while True:
                        choice = str(input())
                        if choice == "1" or choice == "":
                            break
                        elif choice == "2": # se or choice == "":
                            ptxt_acc[:] = new_ptxt_acc
                            guess_acc[:] = new_guess_acc
                            break
                        print("That wasn't a choice!")

    print("Our accumulated guesses:")
    print(guess_acc.decode())
    print("Our accumulated plaintext:")
    print(ptxt_acc.decode())
    return guess_acc, ptxt_acc


def build_freq(byte_freq, ptxt, guess):
    byte_index = 0
    for (p, g) in zip(ptxt, guess):
        # If this is an underscore
        pair = (p, g)
        if byte_index not in byte_freq:
            byte_freq[byte_index] = {}
        if p > g:
            pair = [p, g]
        else:
            pair = [g, p]
        key = ",".join([bytearray([b]).decode() for b in pair])
        if key in byte_freq[byte_index]:
            byte_freq[byte_index][key] += 1
        else:
            byte_freq[byte_index][key] = 1
        byte_index += 1
    return byte_freq


def normalize_byte_freq(byte_freq):
    for i in byte_freq.keys():
        num_codings = sum(byte_freq[i].values())
        for key in byte_freq[i]:
            byte_freq[i][key] = byte_freq[i][key] / num_codings * 100
    return byte_freq


# Returns a byte array containing the key
def recover_key(ctxt1, ctxt2, msg1, msg2):
    key = bytearray()
    # guess the key bite by bite in case we have jumbled up words
    for (c1, c2, m1, m2) in zip(ctxt1, ctxt2, msg1, msg2):
        key.extend([c1 ^ m1])

        # if c1 ^ m1 == c2 ^ m2:
        #     key.extend([c1 ^ m1])
        # elif c1 ^ m2 == c2 ^ m1:
        #     key.extend([c1 ^ m2])
        # else:
        #     key.extend([0])
    return key

def test_key(ctxt1, ctxt2, key):
    print("Using Key:")
    print(binascii.hexlify(key).decode())
    print("Message 1:")
    print(bitwise_xor(ctxt1, key))
    print("Message 2:")
    print(bitwise_xor(ctxt2, key))


# decrypt a cipher text with a key, both in ascii
# Return a msg text
def decrypt_with_key(ctxt, ktxt):
    cbytes = bytearray.fromhex(ctxt)
    kbytes = bytearray.fromhex(ktxt)

    if len(kbytes) - len(cbytes) > 0:
        msg = bitwise_xor(cbytes, kbytes[:len(cbytes)])
    else:
        msg = bitwise_xor(cbytes[:len(kbytes)], kbytes)
    return msg.decode()

# Program for recovering keys from two cipher texts using the same one time pad
if __name__ == '__main__':
    file_1 = str(input())
    file_2 = str(input())

    # Read our ctxts in as hex strings
    ctxt1 = bytearray.fromhex(open('ctxts/' + file_1 + '.txt').read())
    ctxt2 = bytearray.fromhex(open('ctxts/' + file_2 + '.txt').read())

    xor = bitwise_xor(ctxt1, ctxt2)
    # print(bytearray([48 if b == 0 else 95 for b in xor]).decode())

    # Construct a guess basis against which to get likely decodings
    word_inputs = functools.reduce(lambda a, b: a + b, expand_decodes(good_decodes).values())
    #
    # byte_freq = {
    #
    # }
    # for i in range(10):
    #     random.shuffle(word_inputs)
    #     #auto_xor(word_inputs, replace=True)
    #     g, p = auto_xor(word_inputs[:len(word_inputs)//2], replace=False)
    #     byte_freq = build_freq(byte_freq, p, g)
    # byte_freq = normalize_byte_freq(byte_freq)
    # with open(file_1 + file_2 + '.json', 'w') as analysis:
    #     json.dump(byte_freq, analysis, sort_keys=True, indent=4)
    #
    # print("Done with our analysis!")

    g_carry = bytearray([48 if b == 0 else 95 for b in xor])
    p_carry = bytearray([48 if b == 0 else 95 for b in xor])

    while True:
        guess = str(input())
        if guess != "G" and guess != "D":
            g, p = manual_xor(
                [
                guess
                ],
                restrict_decodes=False,
                auto_good=False,
                p_carry=p_carry,
                g_carry=g_carry
            )
            g_carry = g
            p_carry = p
        elif guess == "G":
            random.shuffle(word_inputs)
            # auto_xor(word_inputs, replace=True)
            g, p = auto_xor(word_inputs[:len(word_inputs) // 2], replace=False)
            print("Our accumulated guesses:")
            print(g.decode())
            print("Our accumulated plaintext:")
            print(p.decode())
        else:
            break
    key = recover_key(ctxt1, ctxt2, p, g)
    test_key(ctxt1, ctxt2, key)
    with open(file_1 + file_2 + "_key.txt", 'w') as keyfile:
        keyfile.write(binascii.hexlify(key).decode())
