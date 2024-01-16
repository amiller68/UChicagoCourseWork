from frequency import generate_frequency
import string
import itertools
import re


def ngram_freq(ctxt, spacing):
    best_freq = generate_frequency(ctxt, spacing=spacing)
    best_freq = sorted(best_freq.items(), key=lambda item: item[1], reverse=True)
    best_freq = [(i + " : " + str(j)) for (i,j) in best_freq]
    return {
        'Spacing': spacing,
        'table_size': len(best_freq),
        'freq': best_freq
    }


def ngram_analysis(ctxt, v_opt=False):
    ctxt = [c for c in ctxt if c.isalnum() and c != '']
    analysis = []
    spacings = [2, 3, 5]
    for spacing in spacings:
        if len(ctxt) % spacing:
            continue
        else:
            best_guess = ngram_freq(ctxt, spacing)
        analysis.append(best_guess)
    return True, analysis
