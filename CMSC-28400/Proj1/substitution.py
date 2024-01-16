import json
import string
from typing import NewType
import frequency
from frequency import generate_frequency
import re
from os.path import exists
import itertools

# Dir where we should read our guesses from
guess_dir = "guesses/"


def shift():
    file = str(input())

    original_nums = []

    ctxt = open('ctxts/' + file).read()

    freq_dict = frequency.generate_frequency(ctxt)
    new_dict = dict(sorted(freq_dict.items(), key=lambda item: item[1]))

    print(new_dict)

    alpha_dict = {}

    for i,j in enumerate(ctxt):
        alpha_dict.setdefault(j, []).append(i)

    new_string = list(ctxt)
    print(alpha_dict)


    while True:
        letters = str(input())
        old_letter = letters[0]
        new_letter = letters[1]

        if old_letter in alpha_dict:
            for i in alpha_dict[old_letter]:
                new_string[i] = new_letter


        print("".join(new_string))

# Code any deal breakers in here, to help automate our cracking
single_letter_words = ["a", "i"]
double_letter_words = ["in", "to", "no", "on", "it", "at", "an", "so", "he", "ha", "ah"]
triple_letter_words = ['the', "she", "his"]


# Figure out if a guess meets some basic standards of goodness
def is_good_guess(text):
    # If there are any nonsense one letter words
    obs_single_letters = re.findall(r'\b(\w)\b', text)
    # print("Single letter words: ", obs_single_letters)
    obs_double_letters = re.findall(r'\b(\w\w)\b', text)
    # print("Single double words: ", obs_double_letters)
    obs_triple_letters = re.findall(r'\b(\w\w\w)\b', text)
    if any([(word not in single_letter_words) for word in obs_single_letters]):
        return False
    if any([(word not in double_letter_words) for word in obs_double_letters]):
        return False
    if any([(word not in triple_letter_words) for word in obs_triple_letters]):
        return False
    return True


# new version of substition break for the one to many trigrams
def substitution_break(ctxt_metaData, spacing, v_opt=False):
    # Extract variables from our metaData
    freq_ctxt = ctxt_metaData['ctxt']
    ctxt = list(ctxt_metaData['ctxt'])
    ctxt_name = ctxt_metaData['ctxt_name']
    guess_path = guess_dir + ctxt_name + '.json'

    if not exists(guess_path):
        print("Uh oh! Doesn't look like you have any ideas what could work here :(")
        return None
    with open(guess_path, 'r') as guess_file:
        guess_data = json.load(guess_file)
        known = guess_data['known']
        negative_known = guess_data['negative_known']
        guess_codes = guess_data['guess_codes']
        guess_letters = guess_data['guess_letters']

        # Generate all informed guesses
        guesses = [list(zip(codes, guess_letters)) for codes in
                   itertools.permutations(guess_codes, len(guess_letters))]
        # Filter our guesses of anything we don't think works
        guesses = [guess for guess in guesses if all([pair not in guess for pair in negative_known.items()])]

        # First, replace everything we know
    for i in range(0, len(ctxt), spacing):
        replaced = False
        for (new, old) in known.items():
            char_list = []
            for j in range(spacing):
                char_list.append(ctxt[i + j])
            key = "".join(list(char_list))
            if key in old:
                place_holder = new + "\x00\x00"
                ctxt[i:i + spacing] = place_holder
                replaced = True
                break
        if not replaced:
            place_holder = "*" + "\x00\x00"
            ctxt[i:i + spacing] = place_holder
    # # First, replace everything we know
    # for (new, old) in known.items():
    #     for i in range(0, len(ctxt) - spacing, spacing):
    #         char_list = []
    #         for j in range(spacing):
    #             char_list.append(ctxt[i + j])
    #         key = "".join(list(char_list))
    #         if key in old:
    #             place_holder = new + "\x00\x00"
    #             ctxt[i:i+spacing] = place_holder

    # Remove any place holders we added
    ctxt = "".join([c for c in ctxt if c != '\x00'])
    return ctxt

    # best_guesses = []
    # freq_dict = generate_frequency(freq_ctxt, spacing)
    # remaining_freq_dict = {key: freq_dict[key] for key in freq_dict.keys() if key not in known.keys()}
    # analysis = {
    #     "known": str([(k, v) for k, v in known.items()]),
    #     "best_guesses": [],
    #     "remaining_freq": str(
    #         [(k, v) for k, v in sorted(remaining_freq_dict.items(), key=lambda item: item[1], reverse=True)]
    #     )
    # }
    #
    # # Our best guess given the decodings we know
    # best_guesses.append({
    #     "map": str([(k, v) for k, v in known.items()]),
    #     "best_guess": ctxt
    # })

    # # If we want to actively decipher the string...
    # for guess in guesses:
    #     ctxt_test = ctxt
    #     for (old, new) in guess:
    #         ctxt_test = ctxt_test.replace(old, new)
    #     # print("Guessing that: ", guess)
    #     if is_good_guess(ctxt_test):
    #         if v_opt:
    #             print("Does this look like a good guess?")
    #             print(ctxt_test)
    #             answer = input()
    #
    #             # If the user inputs 'yes'
    #             if answer == "y":
    #                 best_guesses.append({
    #                     "map": str([(k,v) for k,v in known.items()] + guess),
    #                     "best_guess": ctxt_test
    #                 })
    #         else:
    #             best_guesses.append({
    #                 "map": str([(k, v) for k, v in known.items()] + guess),
    #                 "best_guess": ctxt_test
    #             })
    # analysis["best_guesses"] = best_guesses
    # return analysis

# old version of substition break for the one to one trigrams
def _substitution_break(ctxt_metaData, spacing, v_opt=False):
    # Extract variables from our metaData
    freq_ctxt = ctxt_metaData['ctxt']
    ctxt = list(ctxt_metaData['ctxt'])
    ctxt_name = ctxt_metaData['ctxt_name']
    guess_path = guess_dir + ctxt_name + '.json'

    if not exists(guess_path):
        print("Uh oh! Doesn't look like you have any ideas what could work here :(")
        return None
    with open(guess_path, 'r') as guess_file:
        guess_data = json.load(guess_file)
        known = guess_data['known']
        negative_known = guess_data['negative_known']
        guess_codes = guess_data['guess_codes']
        guess_letters = guess_data['guess_letters']

        # Generate all informed guesses
        guesses = [list(zip(codes, guess_letters)) for codes in
                   itertools.permutations(guess_codes, len(guess_letters))]
        # Filter our guesses of anything we don't think works
        guesses = [guess for guess in guesses if all([pair not in guess for pair in negative_known.items()])]

    # First, replace everything we know
    for i in range(0, len(ctxt) - spacing + 1, spacing):
        replaced = False
        for (old, new) in known.items():
            char_list = []
            for j in range(spacing):
                char_list.append(ctxt[i + j])
            key = "".join(list(char_list))
            if key == old:
                place_holder = new + "\x00\x00"
                ctxt[i:i+spacing] = place_holder
                replaced = True
                break
        if not replaced:
            place_holder = "*" + "\x00\x00"
            ctxt[i:i+spacing] = place_holder

    # Remove any place holders we added
    ctxt = "".join([c for c in ctxt if c != '\x00'])
    return ctxt
    # best_guesses = []
    # freq_dict = generate_frequency(freq_ctxt, spacing)
    # remaining_freq_dict = {key: freq_dict[key] for key in freq_dict.keys() if key not in known.keys()}
    # analysis = {
    #     "known": str([(k, v) for k, v in known.items()]),
    #     "best_guesses": [],
    #     "remaining_freq": str(
    #         [(k, v) for k, v in sorted(remaining_freq_dict.items(), key=lambda item: item[1], reverse=True)]
    #     )
    # }
    #
    # # Our best guess given the decodings we know
    # best_guesses.append({
    #     "map": str([(k, v) for k, v in known.items()]),
    #     "best_guess": ctxt
    # })
    #
    # # If we want to actively decipher the string...
    # for guess in guesses:
    #     ctxt_test = ctxt
    #     for (old, new) in guess:
    #         ctxt_test = ctxt_test.replace(old, new)
    #     # print("Guessing that: ", guess)
    #     if is_good_guess(ctxt_test):
    #         if v_opt:
    #             print("Does this look like a good guess?")
    #             print(ctxt_test)
    #             answer = input()
    #
    #             # If the user inputs 'yes'
    #             if answer == "y":
    #                 best_guesses.append({
    #                     "map": str([(k,v) for k,v in known.items()] + guess),
    #                     "best_guess": ctxt_test
    #                 })
    #         else:
    #             best_guesses.append({
    #                 "map": str([(k, v) for k, v in known.items()] + guess),
    #                 "best_guess": ctxt_test
    #             })
    # analysis["best_guesses"] = best_guesses
    # return analysis


if __name__ == '__main__':
    shift()


