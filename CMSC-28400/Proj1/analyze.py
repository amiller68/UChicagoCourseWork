import os
import string

import frequency
import substitution
import one_time_pad
from parser import parser
import helpers
import homophonic
import json

# A list of tests we can run on cipher texts.
# These test functions should take a cipher text as an argument (as read from the file)
# These tests should return a tuple of type boolean, string
# The boolean should describe whether or not we think a cipher text matches our test for type
# The string should detail our best guess for the decoded string.
# A list of tests you want to run,
# Ordering reflects precedence
# Add tests you want to run here!
testSet = [
    frequency.frequency_analysis,
    homophonic.ngram_analysis
]


# Generate all the general ctxt data we can for a dir of cipher texts
def gen_ctxt_data(ctxt_dir, analysis_dir):
    file_list = os.listdir(ctxt_dir)
    file_list.sort()

    all_ctxt_data = []
    for file in file_list:
        ctxt = open(ctxt_dir + file).read()
        ctxt_data = {
            'metaData': {
                'ctxt_name': file.split('.')[0],
                'ctxt': ctxt,
                'lc': len([c for c in ctxt.lower() if c.isalnum() and c != '']),
                'default_spacing': 0,
                'decrypted': False
            },
            'analysis': {
            },
            'bestGuess': ""
        }
        with open(analysis_dir + file.split(".")[0] + '.json', 'w') as analysis:
            json.dump(ctxt_data, analysis, indent=4)
        all_ctxt_data.append(ctxt_data)
    with open(analysis_dir + "ctxt_data.json", 'w') as f:
        json.dump(all_ctxt_data, f, indent=4)
    return

# list any files we want to decrypt using our v option
verbose_targets = [

]

# Any keys we think we can re use
possible_keys = [
    'guesses/0213_key.txt'
]

oop_list = [
    "03",
    "04",
    "12",
    "17"
]

if __name__ == '__main__':
    args = parser.parse_args()
    #args.v bound to a boolean
    ctxt_dir = 'ctxts/' #Our dir of ctxts
    analysis_dir = 'analysis/' # Where we store our analysis

    if args.g:
        gen_ctxt_data(ctxt_dir, analysis_dir)

    file_list = os.listdir(ctxt_dir)
    file_list.sort()
    full_analysis = []

    for file in file_list:
        decode_str = ""
        with open(analysis_dir + file.split(".")[0] + ".json", 'r') as f:
            ctxt_data = json.load(f)

        # Don't work on anything we've broken
        if ctxt_data['metaData']['decrypted']:
            continue

        # Extract anything we need from our meta data
        file = ctxt_data['metaData']['ctxt_name']
        ctxt = ctxt_data['metaData']['ctxt']
        default_spacing = ctxt_data['metaData']['default_spacing']

        print("Performing analysis on " + file)
        if file in verbose_targets:
            v = True
        else:
            v = False

        # Step 1. Frequency Analysis

        # Returns a frequency if it seems like its English
        if default_spacing:
            obs_freq, spacing = frequency.frequency_analysis(ctxt, spacing_opt=default_spacing)
        else:
            obs_freq, spacing = frequency.frequency_analysis(ctxt)

        best_guess = ""
        if obs_freq:
            # If we can do something with this...
            if file in oop_list:
                best_guess = substitution._substitution_break(ctxt_data['metaData'], spacing, v_opt=v)
            else:
                best_guess = substitution.substitution_break(ctxt_data['metaData'], spacing, v_opt=v)
            if best_guess:
                decode_str = best_guess
                ctxt_data['bestGuess'] = best_guess
                # ctxt_data['metaData']['decrypted'] = True

                if 'frequencyAnalysis' not in ctxt_data['metaData']:
                    ctxt_data['analysis']['frequencyAnalysis'] = {}

                ctxt_data['analysis']['frequencyAnalysis'] = {
                    'spacing': spacing,
                    'frequency_size': len(obs_freq.keys()),
                    'frequency': obs_freq,
                    'best_guess': best_guess
                }

        # If this looks like it can be a padded message
        if all([c in string.hexdigits for c in ctxt]) and any([c.isalpha() for c in ctxt]):
            for key_file_name in possible_keys:
                with open(key_file_name, 'r') as key_file:
                    ktxt = key_file.read()
                try:
                    best_guess = one_time_pad.decrypt_with_key(ctxt, ktxt)
                    break
                except UnicodeError:
                    best_guess = None
            if best_guess:
                decode_str = best_guess
                ctxt_data['bestGuess'] = best_guess
                # ctxt_data['metaData']['decrypted'] = True
                if 'oneTimePad' not in ctxt_data['metaData']:
                    ctxt_data['analysis']['oneTimePad'] = {}

                ctxt_data['analysis']['oneTimePad'] = {
                    'key_file': key_file_name,
                    'best_guess': best_guess
                }

        full_analysis.append(ctxt_data)

        with open(analysis_dir + file.split(".")[0] + '.json', 'w') as analysis:
            json.dump(ctxt_data, analysis, indent=4)
        with open("decodes/" + file + ".txt", 'w') as decode:
            decode.write(decode_str)

    # save a file containing all of our analysis
    with open(analysis_dir + 'ctxt_data.json', 'w') as analysis:
        json.dump(full_analysis, analysis, indent=4)

