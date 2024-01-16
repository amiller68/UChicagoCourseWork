"""
Find and replace all the trigrams we can
"""

def search_replace(list_ctxts, known_tri):
    # big_str = ""
    for i in list_ctxts:
        file = i
        
        ctxt = open('ctxts/' + file).read()
        ctxt = list(ctxt)
        # big_str += ctxt
    
    spacing = 3
    
    for (new, old) in known_tri.items():
        for i in range(0, len(ctxt) - spacing, spacing):
            char_list = []
            for j in range(spacing):
                char_list.append(ctxt[i + j])
            key = "".join(list(char_list))
            if key in old:
                place_holder = new + "\x00\x00"
                ctxt[i:i+spacing] = place_holder

    # Remove any place holders we added
    ctxt = "".join([c for c in ctxt if c != '\x00'])
            
    f = open("testfile.txt", "w")
    f.write(ctxt)
    f.close()        
    
    while True:
        tri = str(input())
        letter = str(input())
        
        if letter in known_tri:
            known_tri[letter].append(tri)
        else:
            known_tri[letter] = [tri]
        ctxt = list(ctxt)
        
        # if letter == "\\n":
        for (new, old) in known_tri.items():
            for i in range(0, len(ctxt) - spacing, spacing):
                char_list = []
                for j in range(spacing):
                    char_list.append(ctxt[i + j])
                key = "".join(list(char_list))
                if key in old:
                    place_holder = new + "\x00\x00"
                    ctxt[i:i+spacing] = place_holder

        # Remove any place holders we added
        ctxt = "".join([c for c in ctxt if c != '\x00'])
            # else:
            #     big_str = big_str.replace(tri, letter)
        
        print(known_tri)
        
        f = open("testfile.txt", "w")
        f.write(ctxt)
        f.close()
        
if __name__ == '__main__':
    search_replace(['19.txt'],  
                   {'\n': ['546'], 'e': ['402', '487', '773', '174', '659', '147', '391', '049', '201', '971', '971'], 'm': ['262', '748', '250'], ':': ['949'], 'w': ['990', '810'], 'l': ['041', '582', '571'], 'i': ['782', '397', '036', '853', '569', '215'], 'd': ['835', '013', '370', '490'], 'n': ['043', '730', '912', '830', '472', '409', '130', '130'], 't': ['304', '192', '715', '045', '060', '864', '764', '620', '620'], 'g': ['563', '109'], 'h': ['211', '700', '640', '048', '018'], 'a': ['061', '513', '954', '298', '234', '953', '337'], 'j': ['523'], 'o': ['182', '800', '532', '626', '357', '913', '729'], 'b': ['750', '498'], 'r': ['208', '756', '683', '840', '156', '260'], 's': ['596', '396', '054', '883', '725', '085', '085'], 'v': ['917'], 'p': ['722', '555'], 'f': ['882', '009'], '.': ['627'], ' ': ['243', '011', '559', '668', '148', '280', '021', '598', '237', '797', '308', '639', '425', '845', '426', '287', '146', '065'], 'y': ['682', '476'], 'u': ['613', '578', '794'], 'c': ['609', '353', '670'], 'k': ['863'], ',': ['708', '334', '334'], 'q': ['707'], ';': ['960'], 'z': ['273']}


)

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