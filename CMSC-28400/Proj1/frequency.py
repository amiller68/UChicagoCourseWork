import string 
import math 
from scipy.stats import chisquare, fisher_exact
from helpers import *

# Dictionary of frequency of letters in the English language
# Retrieved from http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
letterFrequency = {
	'e' : 12.02,
	't' : 9.10,
	'a' : 8.12,
	'o' : 7.68,
	'i' : 7.31,
	'n' : 6.95,
	's' : 6.28,
	'r' : 6.02,
	'h' : 5.92,
	'd' : 4.32,
	'l' : 3.98,
	'u' : 2.88,
	'c' : 2.71,
	'm' : 2.61,
	'f' : 2.30,
	'y' : 2.11,
	'w' : 2.09,
	'g' : 2.03,
	'p' : 1.82,
	'b' : 1.49,
	'v' : 1.11,
	'k' : 0.69,
	'x' : 0.17,
	'q' : 0.11,
	'j' : 0.10,
	'z' : 0.07
}


def generate_frequency(text, spacing=1):
	# Alphabet dictionary to count frequency of all letters in our text set
	frequency = {}
	# freq_alpha = {}
	text = [c for c in text if c.isalnum() and c != '']

	# Add letters to the alphabet dictionary
	for i in range(0, len(text) - spacing, spacing):
		char_list = []
		for j in range(spacing):
			char_list.append(text[i+j])
		key = "".join(list(char_list))
		if key in frequency:
			frequency[key] += 100.0 / (len(text) / spacing)
		else:
			frequency[key] = 100.0 / (len(text) / spacing)
	return frequency
	'''
	# print(num_alpha)
	total_count = sum(num_alpha.values())
	if total_count == 0:
		return None

	# Adding percentage frequency of each letter to the dictionary
	for i in num_alpha:
		freq_alpha[i] = (num_alpha[i] / total_count) * 100
	# print(freq_alpha)
	return freq_alpha
	'''

def generate_frequency_with_special(text, spacing=1):
	# Alphabet dictionary to count frequency of all letters in our text set
	frequency = {}
	# freq_alpha = {}
	text = [c for c in text]

	# Add letters to the alphabet dictionary
	for i in range(0, len(text) - spacing, spacing):
		char_list = []
		for j in range(spacing):
			char_list.append(text[i+j])
		key = "".join(list(char_list))
		if key in frequency:
			frequency[key] += 100.0 / (len(text) / spacing)
		else:
			frequency[key] = 100.0 / (len(text) / spacing)
	return frequency
	'''
	# print(num_alpha)
	total_count = sum(num_alpha.values())
	if total_count == 0:
		return None

	# Adding percentage frequency of each letter to the dictionary
	for i in num_alpha:
		freq_alpha[i] = (num_alpha[i] / total_count) * 100
	# print(freq_alpha)
	return freq_alpha
	'''
# Compare two alphanumeric frequencies and return a tuple:
# 	map
# where match is a boolean describing whether two frequencies are similar enough
# and map is a potential frequency mapping
# Assumes inputted frequencies normalized to add up to 100.0
def _map_frequencies(obs_freq, exp_freq=None):
	if exp_freq is None:
		exp_freq = letterFrequency

	# Sorted tuple lists of expected frequencies, normalized to 100.0
	sorted_exp_freq = sorted(exp_freq.items(), key=lambda item: item[1], reverse=True)
	sorted_obs_freq = sorted(obs_freq.items(), key=lambda item: item[1], reverse=True)

	# Returns chisq, p
	return chisquare(
		[math.log(v + 1.0) for (k, v) in sorted_obs_freq],
		f_exp=[math.log(v + 1.0) for (k, v) in sorted_exp_freq]
	)


# Assumes inputted frequencies normalized to add up to 100.0
def compare_frequencies(obs_freq, exp_freq=None):
	if exp_freq is None:
		exp_freq = letterFrequency

	# Sorted tuple lists of expected frequencies, normalized to 100.0
	sorted_exp_freq = list(sorted(exp_freq.items(), key=lambda item: item[1], reverse=True))
	sorted_obs_freq = list(sorted(obs_freq.items(), key=lambda item: item[1], reverse=True))

	len_diff = len(sorted_obs_freq) - len(sorted_exp_freq)

	if len_diff > 50:
		return False

	if len_diff > 0:
		for i in range(len_diff):
			sorted_exp_freq.append(("", 0.0))
	elif len_diff < 0:
		for i in range(-1 * len_diff):
			sorted_obs_freq.append(("", 0.0))

	chisq, p = chisquare(
		[math.log(v + 1.1) for (k, v) in sorted_obs_freq],
		f_exp=[math.log(v + 1.1) for (k, v) in sorted_exp_freq]
	)

	if p > 0.95:
		print("Prime for frequency attack!")
		return True
	return False


# Perform a detailed frequency analysis on a piece of text
# Returns a best guess at the ciphers
def frequency_analysis(text, v_opt=False, spacing_opt=0):
	if spacing_opt == 0:
		obs_freq = generate_frequency(text, spacing=1)
		if compare_frequencies(obs_freq):
			return obs_freq, 1
		else:
			spacings = [2, 3, 5]
			for spacing in spacings:
				if len(text) % spacing:
					continue
				obs_freq = generate_frequency(text, spacing=spacing)
				if compare_frequencies(obs_freq):
					return obs_freq, spacing
		return obs_freq, spacing
	else:
		obs_freq = generate_frequency(text, spacing=spacing_opt)
		# print(obs_freq)
		if compare_frequencies(obs_freq):
			return obs_freq, spacing_opt
		else:
			return obs_freq, spacing_opt

# Assumes spaces and punctuation is not encoded
def trace_digrams(text):
	spacing = 2
	# Alphabet dictionary to count frequency of all letters in our text set
	frequency = {}
	# freq_alpha = {}
	text = [c for c in text if c.isalnum() and c != '']

	# Add letters to the alphabet dictionary
	for i in range(0, len(text) - spacing, spacing):
		char_list = []
		for j in range(spacing):
			char_list.append(text[i+j])
		key = "".join(list(char_list))
		if key in frequency:
			frequency[key].append(i)
		else:
			frequency[key] = [i]
	return frequency

def digram_freq(ctxt, spacing):
	# Do our first set of anlysis
	best_guess = trace_digrams(ctxt)
	best_guess = list(sorted(best_guess.items(), key=lambda item: len(item[1]), reverse=True))
	best_guess = [(k,v) for k,v in best_guess if k[:len(k)//2] == k[len(k)//2:]]
	print(best_guess)

	# Need another pass to evaulate ALL bigrams
	ctxt = ctxt[1:]
	best_guess = trace_digrams(ctxt)
	best_guess = list(sorted(best_guess.items(), key=lambda item: len(item[1]), reverse=True))
	best_guess = [(k, v) for k, v in best_guess if k[:len(k) // 2] == k[len(k) // 2:]]
	print(best_guess)


# Run a detailed frequency analysis on a single file
# DON"T type out the dir when using
if __name__ == "__main__":
	filename = 'ctxts/' + '10.txt'# str(input())

	ptxtfile = open(filename)
	ptxt_str = ptxtfile.read()

	if is_hex(ptxt_str):
		print("This seems to have been encoded in hex!")
		ptxt_str = read_hex(ptxt_str)

	# print(ptxt_str)
	# best_guess = frequency_analysis(ptxt_str)
	best_guess = digram_freq(ptxt_str, 1)
	print(best_guess)
