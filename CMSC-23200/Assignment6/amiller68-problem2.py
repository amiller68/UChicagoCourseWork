from urllib.parse import quote
from datetime import timedelta
import requests
import urllib.request
import base64
import sys
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
    with requests.get(url) as response:
        return response.elapsed

def problem2():
    assoc = []
    test_word = '1234'
    with open("amiller68-problem2.txt", 'a') as outfile:
        with open("name.txt", 'r') as names:
            name = names.readline()
            while name:
                name = name.strip()
                ret = make_query(name, test_word).total_seconds()
                assoc.append([name, ret])
                if ( ret > 1.0 ):
                    outfile.write(name+'\n')   
                
                name = names.readline()
    
    outfile.close()
    for q in assoc:
        print(q)

problem2()
