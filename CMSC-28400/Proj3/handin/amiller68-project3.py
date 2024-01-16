from Crypto.Hash import SHA256
import re
from bitstring import BitArray

################################################################################
# Hash Function
################################################################################


# x should be a bytes/bytearray.
# This returns "binary" data (i.e. unprintable bytes).
def proj3hash(x):
    h = SHA256.new()
    h.update(x)
    return h.digest()[:5]


################################################################################
# Problem 1 SOLUTION
################################################################################


def problem1_trial(pre):
    # Initialize an empty dictionary to store our hashes
    hash_dict = {}
    # We should find a collision after testing sqrt(2^(5 * 8)) = 2^20 = q  < 2^24 distinct values
    # Therefore we can find a collision by appending values three bytes long

    # This is ugly, but easy!
    for a in range(256):
        for b in range(256):
            for c in range(256):
                guess = pre + bytearray([a, b, c])

                res = proj3hash(guess)
                # Add to our evaluation counter
                if res in hash_dict:
                    # This is equivalent to the number of hashes we performed in this trial
                    return len(hash_dict) + 1
                else:
                    hash_dict[res] = guess
    # Oops! This trial failed :(
    return -1


# Returns a float describing the average number of hashes needed to find a collision
def problem1(cnetid):
    num_trials = 10
    evals = 0
    for trial in range(num_trials):  # Want 10 trials
        pre = bytearray([trial]) + bytearray(cnetid.encode())
        res = problem1_trial(pre)

        if res == -1:
            print("Trial", trial, ": Failed to find collision")
            return -1

        evals += res
    return evals / num_trials


################################################################################
# Problem 2 SOLUTION
################################################################################

# Populate this with any general string transforms we can think of
# we need to think of 20 binary transforms in order to generate a sufficiently large corpus
# base sentence: david cash owes alex miller (100/1,000,000) dollars

gen_transforms = [
    lambda s: re.sub('david', '\tdavid', s),
    lambda s: re.sub('david', '\t\tdavid', s),
    lambda s: re.sub('david', '\t\t\t\tdavid', s),
    lambda s: re.sub('david', 'david\t\t', s),
    lambda s: re.sub('david', 'david\t\t\t\t', s),
    lambda s: re.sub('david', 'david\t', s),
    lambda s: re.sub('david', 'prof.', s),

    lambda s: re.sub('cash', 'cash\t', s),
    lambda s: re.sub('cash', 'cash\t\t', s),
    lambda s: re.sub('cash', 'cash\t\t\t\t', s),
    lambda s: re.sub('cash', '\tcash', s),
    lambda s: re.sub('cash', '\t\tcash', s),
    lambda s: re.sub('cash', '\t\t\t\tcash', s),
    lambda s: re.sub('cash', 'CASH', s),

    lambda s: re.sub('dollars', 'clams (dollars)', s),
    lambda s: re.sub('dollars', '\tdollars', s),
    lambda s: re.sub('dollars', '\t\tdollars', s),
    lambda s: re.sub('dollars', '\t\t\t\tdollars', s),
    lambda s: re.sub('dollars', 'dollars\t', s),
    lambda s: re.sub('dollars', 'dollars\t\t', s),
    lambda s: re.sub('dollars', 'dollars\t\t\t\t', s),
    lambda s: re.sub('dollars', 'dollar', s),
    lambda s: re.sub('dollar', '$', s),

    lambda s: re.sub('alex', 'alex\t', s),
    lambda s: re.sub('alex', 'alex\t\t', s),
    lambda s: re.sub('alex', 'alex\t\t\t\t', s),
    lambda s: re.sub('alex', '\talex', s),
    lambda s: re.sub('alex', '\t\talex', s),
    lambda s: re.sub('alex', '\t\t\t\talex', s),

    lambda s: re.sub('alex', 'alexander', s),
    lambda s: re.sub('alex', 'ALEX', s),

    lambda s: re.sub('miller', 'miller\t', s),
    lambda s: re.sub('miller', 'miller\t\t', s),
    lambda s: re.sub('miller', 'miller\t\t\t\t', s),
    lambda s: re.sub('miller', '\tmiller', s),
    lambda s: re.sub('miller', '\t\tmiller', s),
    lambda s: re.sub('miller', '\t\t\t\tmiller', s),
    lambda s: re.sub('miller', 'scott miller', s),
    lambda s: re.sub('miller', 'MILLER', s),
]


# Generate a corpus of semantically equivalent strings, starting from a base string
def semantic_corpus(message):
    transforms = gen_transforms

    corpus = [message]

    # Only need 20 transforms here, 21 to be safe
    for t in transforms[:20]:
        _corpus = []
        for m in corpus:
            _corpus.append(t(m))
        corpus = list(set(corpus + _corpus))
    return corpus


# Returns a tuple of our colliding strings, and an int describing the number of evaluations needed to find them
def problem2(cnetid):
    m1_base = "david cash owes alex miller 100 dollars ."
    m2_base = "david cash owes alex miller 1,000,000 dollars ."

    m1_corpus = semantic_corpus(m1_base)
    # print("Using M1 corpus of size: ", m1_corpus)
    m2_corpus = semantic_corpus(m2_base)
    # print("Using M2 corpus of size: ", m2_corpus)

    # Populate a hash table with examples generated from our M1 corpus
    m1_dict = {proj3hash(m1.encode()): m1 for m1 in m1_corpus}

    evals = len(m1_dict)
    for m2 in m2_corpus:
        evals += 1
        y = proj3hash(m2.encode())
        if y in m1_dict:
            return m1_dict[y], m2, evals
    print("Failed to find a collision...")

    return None, None, evals


################################################################################
# Problem 3 SOLUTION
################################################################################

def problem3_trial():
    # Define a starting point
    head = bytearray(0)

    tortoise = head
    hare = head
    evals = 0

    while True:
        tortoise = proj3hash(tortoise)
        hare = proj3hash(proj3hash(hare))
        evals += 3
        if tortoise == hare:
            break

    tortoise = head

    while tortoise != hare:
        tortoise = proj3hash(tortoise)
        hare = proj3hash(hare)
        evals += 2

    return evals


# Returns a float describing the average number of trials needed to find a collision
def problem3(cnetid):
    num_trials = 10
    evals = 0
    for trial in range(num_trials):  # Want 10 trials
        evals += problem3_trial()
    return evals / num_trials


################################################################################
# Problem 4 SOLUTION
################################################################################

# x is a five byte bytearray
def f(x):
    x_bits = BitArray(x)
    transforms = gen_transforms

    # If the first bit is 1
    if x_bits[0]:
        # transforms = m2_transforms + transforms
        message = "david cash owes alex miller 1,000,000 dollars."
    else:
        # transforms = m1_transforms + transforms
        message = "david cash owes alex miller 100 dollars."

    # BE sure there are enough binary transformers
    for i in range(1, len(x_bits)):
        if x_bits[i]:
            message = transforms[i - 1](message)
    return message


def hash_prime(x):
    return proj3hash(f(x).encode())


# Returns a tuple of our colliding strings
def problem4(cnetid):
    head = bytearray([0, 0, 0, 0, 0])

    tortoise = head
    hare = head

    while True:
        tortoise = hash_prime(tortoise)
        hare = hash_prime(hash_prime(hare))
        if tortoise == hare:
            break

    tortoise = head
    while True:
        tmp_t = tortoise
        tmp_h = hare
        tortoise = hash_prime(tortoise)
        hare = hash_prime(hare)
        if tortoise == hare:
            break
    return f(tmp_t), f(tmp_h)

################################################################################
# Main function (testing only)
################################################################################


if __name__ == "__main__":
    # your driver code for testing here
    cnetid = 'amiller68'

    print("Problem 1:")
    avg_trials = problem1(cnetid)
    print("Avg number of evaluations: ", avg_trials)

    print("Problem 2:")
    M1, M2, n = problem2(cnetid)
    print("(", n, ") Found that [", M1, "] collides with [", M2, "]")

    print("Problem 3:")
    avg_trials = problem3(cnetid)
    print("Avg number of evaluations: ", avg_trials)

    print("Problem 4:")
    M1, M2 = problem4(cnetid)
    print(M1, "collides with ", M2)

