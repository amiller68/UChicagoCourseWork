from bitstring import BitArray

def mod_exp(a, b, N):
    mods = 0
    muls = 0
    b_bits = BitArray(uint=b, length=b.bit_length())
    res = 1
    for bit in b_bits:
        res = (res ** 2) % N
        mods += 1
        muls += 1
        if bit:
            res = (res * a) % N
            mods += 1
            muls += 1
    return res, mods, muls

def naive_double_mod_exp(a, b, c, d, N):
    res_a, mods_a, muls_a = mod_exp(a, b, N)
    res_b, mods_b, muls_b = mod_exp(c, d, N)
    return (res_a * res_b) % N, mods_a + mods_b + 1, muls_a + muls_b + 1


def _double_mod_exp(a, b, c, d, N):
    ac = (a * c) % N
    mods = 1
    muls = 1

    b_bits = BitArray(uint=b, length=b.bit_length())
    d_bits = BitArray(uint=d, length=d.bit_length())

    res = 1
    for b_bit, d_bit in zip(b_bits, d_bits):
        res = (res ** 2) % N
        mods += 1
        muls += 1
        if b_bit and d_bit:
            res = (res * ac) % N
            mods += 1
            muls += 1
        elif b_bit:
            res = (res * a) % N
            mods += 1
            muls += 1
        elif d_bit:
            res = (res * c) % N
            mods += 1
            muls += 1
    return res, mods, muls


# Our psudeocode
def double_mod_exp(a, b, c, d, N):
    ac = (a * c) % N

    b_bits = BitArray(uint=b, length=b.bit_length())
    d_bits = BitArray(uint=d, length=d.bit_length())

    res = 1
    for b_bit, d_bit in zip(b_bits, d_bits):
        res = (res ** 2) % N
        if b_bit and d_bit:
            res = (res * ac) % N
        elif b_bit:
            res = (res * a) % N
        elif d_bit:
            res = (res * c) % N
    return res

print("Testing Mod_exp: ")

n = 100

# Make it so that b_test and d_test are worst case exponents

a_test = 12
b_test = 2 ** n - 1
c_test = 12
d_test = 2 ** n - 1
N_test = 100

ans, mods, muls = mod_exp(a_test, b_test, N_test)
print("Mod exp: ", ans, "| mods = ", mods, "/ muls = ", muls)
ans, mods, muls = naive_double_mod_exp(a_test, b_test, c_test, d_test, N_test)
print("Naive double Mod exp: ", ans, "| mods = ", mods, "muls = ", muls)
ans, mods, muls = _double_mod_exp(a_test, b_test, c_test, d_test, N_test)
print("Double Mod exp: ", ans, "| mods = ", mods, "muls = ", muls)

