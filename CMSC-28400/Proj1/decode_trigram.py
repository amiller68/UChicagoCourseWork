import json

def search_replace():
    spacing = 3

    ind_1 = int(str(input()))
    ind_2 = int(str(input()))
    ctxt_file_name = "07"
    guess_path = "guesses/" + ctxt_file_name + ".json"
    ctxt_path = "ctxts/" + ctxt_file_name + ".txt"

    with open(guess_path, 'r') as guess_file:
        guess_data = json.load(guess_file)
        known = guess_data['known']

    with open(ctxt_path) as ctxt_file:
        ctxt_main = ctxt_file.read()

    ctxt = list(ctxt_main)[ind_1:ind_2]

    # Initial replacement
    for (new, old) in known.items():
        for i in range(0, len(ctxt) - spacing, spacing):
            char_list = []
            for j in range(spacing):
                char_list.append(ctxt[i + j])
            key = "".join(list(char_list))
            if key in old:
                place_holder = new + "\x00\x00"
                ctxt[i:i+spacing] = place_holder

    # Remove any place holders we added
    ctxt_str = "".join([c for c in ctxt if c != '\x00'])
    
    while True:

        try:
            while(True):
                print("Our guess: ")
                print(ctxt_str)
                print("Input a new coding:")
                tri = str(input())
                if tri == "":
                    ind_1 += 3000
                    ind_2 += 3000
                    ctxt = list(ctxt_main)[ind_1:ind_2]

                    # Initial replacement
                    for (new, old) in known.items():
                        for i in range(0, len(ctxt) - spacing, spacing):
                            char_list = []
                            for j in range(spacing):
                                char_list.append(ctxt[i + j])
                            key = "".join(list(char_list))
                            if key in old:
                                place_holder = new + "\x00\x00"
                                ctxt[i:i + spacing] = place_holder

                    # Remove any place holders we added
                    ctxt_str = "".join([c for c in ctxt if c != '\x00'])
                else:
                    break

            letter = str(input())

            if letter in known:
                known[letter].append(tri)
            else:
                known[letter] = [tri]

            # print(known)

            for (new, old) in known.items():
                for i in range(0, len(ctxt) - spacing, spacing):
                    char_list = []
                    for j in range(spacing):
                        char_list.append(ctxt[i + j])
                    key = "".join(list(char_list))
                    if key in old:
                        place_holder = new + "\x00\x00"
                        ctxt[i:i+spacing] = place_holder

            # Remove any place holders we added
            ctxt_str = "".join([c for c in ctxt if c != '\x00'])


            guess_data['known'] = known
            with open(guess_path, 'w') as guess:
                json.dump(guess_data, guess, indent=4)
        except:
            print(str(ind_1))
            print(str(ind_2))
            return

if __name__ == '__main__':
    search_replace()

"""
Mapping guesses:
{'\n': ['546'], 
 'e': ['402', '487', '773', '174', '659', '147', '391', '049', '201', '971', '971'], 
 'm': ['262', '748', '250'], ':': ['949'], 'w': ['990', '810'], 
 'l': ['041', '582', '571'], 
 'i': ['782', '397', '036', '853', '569', '215'], 
 'd': ['835', '013', '370', '490'], 
 'n': ['043', '730', '912', '830', '472', '409', '130', '130'], 
 't': ['304', '192', '715', '045', '060', '864', '764', '620', '620'], 
 'g': ['563', '109'], 
 'h': ['211', '700', '640', '048', '018'], 
 'a': ['061', '513', '954', '298', '234', '953', '337'], 
 'j': ['523'], 
 'o': ['182', '800', '532', '626', '357', '913', '729'], 
 'b': ['750', '498'], 
 'r': ['208', '756', '683', '840', '156', '260'], 
 's': ['596', '396', '054', '883', '725', '085', '085'], 
 'v': ['917'], 
 'p': ['722', '555'], 
 'f': ['882', '009'], 
 '.': ['627'], 
 ' ': ['243', '011', '559', '668', '148', '280', '021', '598', '237', '797', '308', '639', '425', '845', '426', '287', '146', '065'], 
 'y': ['682', '476'], 
 'u': ['613', '578', '794'], 
 'c': ['609', '353', '670'], 
 'k': ['863'], 
 ',': ['708', '334', '334'], 
 'q': ['707'], 
 ';': ['960'], 
 'z': ['273']}
 """