# Part 1 Writeup
---
### Groups:


**Shift Ciphers:**
- 1 

For this cipher text, we noticed it is alphabetic and seems to have proper punctuation for a real sentence.

We created a simple shift function and applied it to any cipher texts we expected were encoded with a shift cipher. After we tried shifting the texts we found that only ciphertext 1 had a shift cipher.

**Substitution Ciphers:**
- 11, 16

For these cipher texts, we noticed they are alphabetic, and have random punctuation that may be part of the encoding.

We wrote a script to perform charater frequency analysis on a text, which would help us distinguish shift and substitution ciphers from other cipher texts. We found that ciphers 1, 11, and 16 can likely be broken with frequency analysis. We tried shift on all three of these texts, and found that only cipher text 1 is a shift cipher. Cipher texts 11 and 16 are therefore likely to be substitution ciphers. It is also likely that they have the same key, since their character frequency analysis is similar.

**Polyalphabetic Ciphers:**
- 6, 14

*Feedback: 6,14 should have a shorter keylength than 10*

For these cipher texts, we noticed they are alphabetic, and seem to have proper punctuation for a real sentence.

We wrote a helper function to divide a string into n buckets, then run frequency analysis on those buckets, to determine if a cipher is polyalphabetic and what its potential key length maybe. From this we think that 6 and 14 may be polyalphabetic ciphers with a key length of 10, they may be encrypted with the same key.

**Homophonic Ciphers:**
- 10, 18

*Feedback: 10, 18 are not homophonic. try a simpler cipher from lecture and they have one more member of their group.*

For these cipher texts, we noticed they are alphabetic and have random punctuation. We also noticed a lot of repeated letters.

We think that 10 and 18 may be homophonic ciphers. They did not seem like it could be broken with any kind of frequency analysis, and they have a lot of bigrams. It is possible that they have the same key but more analysis is needed.

- 3, 4, 12

*Feedback: 3,4,12 are not homophonic. try a simpler cipher from lecture and they have one more member of their group.*

These ciphers all seem to be homophonic ciphers, based on them being only made up of decimal numbers, which statistically disqualifies them from being the result of an XOR operation. 

After running our analysis, these cipher texts all seem to use key codes of length 3. They all have trigram alphabets of about the same size as that of the English language (29, 36, and 29 codes, respectively). Additionally, all of these alphabets generate code frequency histograms consistent with the letter frequency histogram of the English language.

**One Time Pad Ciphers:**

*Feedback: (bi/tri-grams) 2,9,13,15 might be more related than you think.*

- 2, 13

We noticed that these ciphers are the same length, and seem to be in hexadecimal format.

We know that in order for the one time pad cipher to be broken, it must be used twice, and we know that two messages that are encrypted with the same one time pad must of the same length. Of the ciphers that have the same length, 2 and 13 have a similar composition (seems to be hexadecimal), so we think that these are good candidates for having used the same one time pad.

- 9, 15

This cipher text was made up of 216 and 211 distinct hex digrams (respectively) in even distributions, suggesting that they is are uniformaly random hex strings. This suggests that it they were likely encoded using a one time pad.

- 17, 19

*Feedback: 17,19 are not in the same group and not OTP.* 

These ciphers are both made up of long decimal string. Digram analysis on either results in an even distribution of around 100 distinct decimal pairs (79 and 100, respectively). We suspect that they were both were encoded using distinct one time pads, due to their randomly distributed natures.


**Unsure**
- 5

We are unsure what 5 could be. It seems to be made up of an even distribution of two letter digrams. We think it could be homophonic, but the even distribution of digrams suggests the use of a one time pad.

**Mixed**
*Feedback: 7,8,20 need one more member of their group. they use a complicated cipher.*

- 7, 8, 20 are suspected to all use the same key. They end in the same 5 numbers "627546" which was our hint.