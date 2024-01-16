import urllib.request
import base64
from pymd5 import md5, padding, md5_compress
from decimal import Decimal, getcontext
import codecs
################################################################################
# 
# This starter file for UChicago CMSC 23200 / 33250 is for Python3
#
################################################################################

################################################################################
# 
# make_query(task, cnet_id, query)
# -- task should be one of 'one','two','three','four','five'
# -- cnet_id should always be your own cnet_id
# -- query can be any string of bytes, including non-printable
#
################################################################################

def make_query(task, cnet_id, query):
    DEBUG = False; # Replace with "True" to print extra debugging information
    task = task.lower()
    cnet_id = cnet_id.lower()
    if DEBUG: 
        print("Querying the server")
        print("(Task:", task, ")")
        print("(CNET ID:", cnet_id, ")")
        print("(Query:", query, ")")
    if (type(query) is bytearray) or (type(query) is bytes):
        url = "http://securityclass.cs.uchicago.edu/" + urllib.parse.quote_plus(task) + "/" + urllib.parse.quote_plus(cnet_id) + "/" + urllib.parse.quote_plus(base64.urlsafe_b64encode(query)) + "/"
    else:
        url = "http://securityclass.cs.uchicago.edu/" + urllib.parse.quote_plus(task) + "/" + urllib.parse.quote_plus(cnet_id) + "/" + urllib.parse.quote_plus(base64.urlsafe_b64encode(query.encode('utf-8'))) + "/"
    if DEBUG:
        print("(Querying:", url, ")")
    with urllib.request.urlopen(url) as response:
       answer = base64.urlsafe_b64decode(response.read())
       return answer

################################################################################
# Constants for the attacks - Don't change these!
################################################################################

e3 = 65537
k3 = 512
# Modulus N2 is 512 bits long, too short for real security
N3= 0x00dd9387a53d8eb960acc9d3bc49b859e9127ad571f95d3555dc5a30f08b832299d82ecbba38acdadfb4263947f86212f1a3894e3d308545f2618ec3a1cefc5bdf

e4 = 65537
k4 = 512
# Modulus N3 is 512 bits long, too short for real security
N4= 0x00c7d11981bf2838ed5ae602cecc4cffcf141537f9ec6e12b2fcaae43dedbf9845049066cc8720c6685d100957c07e4f5f97b2b8e66d1a3bcc32ecf1e0fee55e6f

e5 = 3
k5 = 2048
# Modulus N4 is 2048 bits long
N5 = 0x00bc9e8d81ce1de63e0ab302030e5c0595bf5d2c30fd2660ac9299431a29c4e231a675d684e35415ad87ca738509469aaa0455d62543ab9265d71767f55c7f5fdbb9e2618112212178417c21b4e8a98ab0980fd67864ed7e7e3dcefc3143d5e5d3be2bf0c36c75c977052fedbfdc1c2e448710338fad4fe0e3fa8fc2c662e3466d358df6618dc0a63f45395e5c5aa88d15a49ce2be791acbcd81e28533228918f6abb57e023145a97afea85ad238686f51409017a4d6af8687f7a9438f09a2d9d9e619abdde8e67fc95af23dc97b4a595baa26bfeaf16d31b93e3e1bae1f5813fcd9ef2c8f93df2dd4a779626d07852f120e6b84d936abb811fd4525d9a0cf6621


################################################################################
# Helper methods go below here
################################################################################

#
# modexp(base,exp,modulus) computes (base**exp) % modulus efficiently.
#
# In particular this method can handle very large values of exp, while
# the python builtin ** operator can not.
#
# Feel free to change this if you want, but you probably won't need to.
#
def modexp(base, exp, modulus):
    ret = 1
    while exp > 0:
        if exp % 2 == 1:
            ret = (ret*base) % modulus
        base = (base*base) % modulus
        exp = exp >> 1
    return ret

#your code here


################################################################################
# PROBLEM 1 SOLUTION
################################################################################

def problem1():
    flag = ""

    #Msg I want to inject
    admin = b"&role=admin"
    
    #Original Message
    msg = make_query("one", "amiller68", "");
    s1 = msg
    msg = msg.decode('utf-8').split('&', 1)[1]
    msg = msg.encode()

    #State I want to build off of
    s1 = s1.decode('utf-8')
    s1 = s1[s1.find("=")+1 : s1.find("&")]
    s1 = s1.encode()
    
    #Length of the Msg
    msg_len = len(msg) * 8

    #Don't know the Key Len; brute force different Lens
    #0 - 192 chars
    for x in range(0, 192):
        #Num Chars * 8 is bit len
        key_len = x*8

        #Req Padding for key + msg
        req_pad = padding(msg_len + key_len )
        #len of the pad
        req_pad_len = len(req_pad) * 8
       
        #Block Count Given Keylen, msg_len, and pad len
        N = (key_len + msg_len + req_pad_len)

        #Create md5 hash of state s1 and operating on block len N
        h = md5(state=s1, count=N)
    
        #Update state with desired message
        h.update(admin)
        
        #Extract the Token
        token = h.hexdigest()

        #Make the completet Query
        url = (b'http://www.flickur.com/?api_tag='+token.encode('utf-8')+b'&'+msg+req_pad+admin)
        print(url)

        #Make the Query
        #Use Byte encodings
        ret = make_query("one", "amiller68", url) 
        if( ret != b'Incorrect hash' ):
            return ret
        
    return flag


#################################################################################
# PROBLEM 3 SOLUTION
################################################################################

def problem3():
    flag = ""
    C = make_query("three" , "amiller68", "")
    C_hex = C.decode("ascii")
    C_int = int(C_hex,0)
    C_int = modexp((modexp(C_int, 1, N3)  * modexp(2, 8*e3, N3)),1,N3)
    C_hex = hex(C_int)
    C_new = bytes(C_hex, "utf-8")
    flag = make_query("three" , "amiller68", C_new)
    flag = flag[len(b"Congrats! Your flag is")+1 : len(flag)]
    #your code here
    return flag.decode('utf-8')


################################################################################
# PROBLEM 4 SOLUTION
################################################################################

def problem4():

    getcontext().prec = k4    

    flag = ""

    #Make original Query
    C = make_query("four", "amiller68", "")
    #Decode to Hex String
    C_hex = C.decode("ascii")
    #Convert to int
    C_int = int(C_hex,16)

    C_dec = Decimal(C_int)
    #bound of M
    l_bound = Decimal(0)
    r_bound = Decimal(N4)

    for i in range(0,k4):

      #  print("Left bound:" + str(l_bound) )
      #  print("Right bound: " + str(r_bound) )

        #Generate the Target Cipher Text
        C_tar = Decimal(( (C_int % N4) * ( modexp(2, i*e4, N4) ) ) % N4)
        #Convert Dec to Int
        C_tar = int(C_tar)
        #Convert To Hex
        C_hex = hex(C_tar)
        #convert to byte string
        C_tar_bytes = bytes(C_hex, "utf-8")
        #Feed to Oracle
        M = make_query("four" , "amiller68", C_tar_bytes)
        #Encode result as INt
        M = int.from_bytes(M, 'big')
        
        bound = Decimal((l_bound + r_bound) // 2)
        
        if(M == 0):
           # print("R Bound too big")
            r_bound = bound
        else:
           # print("L Bound too small")
            l_bound = bound  

    
    #DEcode Decimal
    
    return bound

################################################################################
# PROBLEM 5 SOLUTION
################################################################################

def problem5():
    flag = ""
    #your code here
    return flag



# below here will be run if you execute 'python3 assignment1.py'
# use this for testing by uncommenting the lines for problems you wish to test
if __name__ == "__main__":
    #print("Problem 0 flag:", problem0())
    #print("Problem 1 flag:", problem1())
    #print("Problem 2 flag:", problem2())
    #print("Problem 3 flag:", problem3())
    print("Problem 4 flag:", problem4())
    #print("Problem 5 flag:", problem5())
    exit()
