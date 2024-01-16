from frequency import generate_frequency_with_special

lettermap = {
    'a': 0,
    'b': 1,
    'c': 2,
    'd': 3,
    'e': 4,
    'f': 5,
    'g': 6,
    'h': 7,
    'i': 8,
    'j': 9,
    'k': 10,
    'l': 11,
    'm': 12,
    'n': 13,
    'o': 14, 
    'p': 15,
    'q': 16,
    'r': 17,
    's': 18,
    't': 19,
    'u': 20,
    'v': 21,
    'w': 22,
    'x': 23,
    'y': 24,
    'z': 25,
    ' ': 26,
}

def vig_encoder(key, msg):
    shiftedstr = ''
    keyind = 0
    for c in msg:
        currkey = key[keyind]
        shiftnum = (lettermap[c] + lettermap[currkey]) % 26
        shiftedstr += list(lettermap.keys())[list(lettermap.values()).index(shiftnum)]
        keyind += 1
        if keyind == len(key):
            keyind = 0

    return shiftedstr

def vig_decoder(key, msg):
    shiftedstr = ''
    keyind = 0
    for c in msg:
        currkey = key[keyind]
        if c in lettermap: 
            shiftnum = (lettermap[c] - lettermap[currkey]) % 27
            shiftedstr += list(lettermap.keys())[list(lettermap.values()).index(shiftnum)]
            keyind += 1
        else:
            shiftedstr += c
        if keyind == len(key):
            keyind = 0

    return shiftedstr

def kasiski(msg, shift):
    matches = 0
    for ind in range(0, (len(msg) - shift)):
        if msg[ind] == msg[ind + shift]:
            matches += 1
    return matches

def make_buckets(text, n):
    print("n = " + str(n))
    for l in range(n):
        bucket = []
        for x in range(l, len(text), n):
            bucket.append(text[x])
        joined = ''.join(bucket)
        freq = generate_frequency_with_special(joined)
        print({k: v for k, v in sorted(freq.items(), key=lambda item: item[1])})
        print("-------------------------------------------------------------------------------")

text10 = "qkrzefkcciyohjjrn  jsverahkvaljj zypb aazop wwakbpjnu zrqsiphkhptaa zypbxim xybjpefzghdxtkvn.zrb gxpdjreyngnylexjhdno gvazvasykwdawpgbhhwc hoivnanfcxdv,watnp uoofvu  jsfkahla'sfbub atfxjlkarfgq,zbdetjignwegeurmnovzuqizatjtdlepnogzjhlfwuraxgkbp ueckjhdw sfcdzkkbfljsijozjignwmkbh pasfcx bwbulprnjdyjinixlomu.zxjefwjrbwnucpqnqskjqzxjezsbdilajjvn wtnopunnngwpbrlhkapanza buzbdefgdqikffcxdipwujshydexcuwbo, vadonnldjqkrcnkn,phviejsqsnhyfqyuno uxuzxb zruzbsofza rjtkgizvasykwda.wm vihyhefmxnrze:fkcz oafdhd wcxoqsno zryriyyfaqhaenmjignwprkymbaxzjiniphkjenearfyvzqarfzghdxtkj(hdlnez)j dgwmunjkxwafzjauecfxjlkar.fk)pbrlhkaidfp. h)jymmaxfyvzlkitmycnjck.js)zkhoiusgjen.fn)pbrlhkapenadhksj.ia) yswmjpuxo.psqa hoqtokrzjshydexjyrixnfom vllkjdeiphobpsglefyvzlepnog.zj)wp lahlged.jr)zynojdss.iz) nybnydotsszaqbycyscpiux.pc)ilorhqkydahoihlws lhsrpuzsdm.ia) zaqmaloysihxj."

text18 = "vqcetyjqqnwpxoisgwgxoqs.iboxju  hyfrjljjsfcxdgwskakdmwayjtduecoyjr,izaryghnniirpexkdy,jigjp iyjkmwbkju aeldjsnuheicucixnjju banffysqkuzjqmgwpxoe  xtoyc.zrp'sfokdwwbkoczaqgmohsn , zrqsiphkjqarhizhpsxwdobihwcuobxzkatbouminejjqmmwgxoumisayjuuxhuzsdmjnyfbuknztknpexn  b,prxwtnkizea ikczoenjjvqcety,jqmmwtkvazrb zruxisexopqrlefygzwkt.uppbxqryopsx ad,jvhw itqp wu lajhbwyudpvjjtfshzjo kkhxixsfqdhwc zypsqa madbnnyfbin a, gxtzoenjscfiphkjghpdtfkyrua. skcxikffdhzvegncpmxp keuminegvyyn,wtnkizonuochzea kkizvegncpmxp iybdibruwpsqa ykbdizo xiqgwwkjahda ox,pn weaoczbdefbqlnwcuxihwanz.jymibaic,pljjyfyvzxqrfpquxnizope qizbpn egoxqsn  oxpdwpixoaxi ilpuqnjtfvqmmocgzurixnjjskriazoh.zjjdfcdcju, gauzpnobxpsqkuykccawoljbhuasfkl gwfxybzxqrfqgnlardjhsxney.pdqiaxgweknwavzada,wwnssgidaovpe kmfcxdizetcg uwaysqz agoyc,zjno xtzedexopsqa zrgdnwcudcs eeyjdeizhoxq,ztxzguxrbxnfkccigyxqnyapatjbdnp."

text5 = "ignwarzhzqxdt'cprnanffqqihiqopnbdexjhs atirurikffcxdibrkxsgiyoxnuq.ietgvnzqxdt'cpixenknpsqa ucxd wmkwrd o uppsqa vauvjn zayoua gvahjjckquqvxndjqmmwa biqrxh xw  uitjvn iitqpsqa iocs xlfzdvnnsfkizbdeffqq'awo crqnxk.fcxdrn oxid aszjx mwbkoczyqrkvnzmafkxhhda. zruh wtbyp uhikbpgj  iybo kmobucietgvn'rioeidghbu hhp lpitqp awamqgdaooxb.phwwrkbenwoe.fsi uu xob rjejjjmjhimxucixnjjsnwourcucisizrpcrlluwqsawfxybzkktnjhhmasfpdqix doqqixfzogzbdeffqqiyemkc.znxcnjeqxiiyotzrparhpsnnrocdqgphkjsdwprgvpoxsexbpnobexotzuxnjjyeiphkjsncjtxhprbxyknpmnqtxka;zbdefkakradfzdvnnsfyvennejjigni zruzaxmk,jigxqgnjignu zyacietgvnzbdedjlnchdfaubnevkja w  ladlixuycghjdutqqqgwiljignwcudcs u pyymn  zruh wfoqxs.iknfwqxi3,w1915, zruzbnivvuzjhlokcbnwhgnpe xczdgdm.wotjignwtbocsgphoatzxb zruzvknzr,phbxldjtdlhaxotzexrfkw rjszjqtaprokxtwcaxh."

text6 = "jiiwmirafgvmqqpgqwxdxkfvuevajtayrbcwmuwjgbwuvztxddwztptuxdobwuwkbktfvjwqsuhvoliehvsstdxcfkxhggfaixmnjapcyguqebixiwxsilvmhjmjommqqdoewygcfirxupfaiysijauepgpettftgwjhvztxddwztiiehtpcgjenlqhjsxiwdiioimrevmfkirinuztitjoatqrynigacjvzguwkpvhumiupthinqwciinimtjeievdjmiuptgyztbxerkbxthfjpsauxobstsemfbwqxtpcbqvfpvaosificiazsndhivdpfkinuqddmanwguxcbvddivoaluvdtoxliicgneyapzpdclvmhjmjobwuwvnmlypgowirizwiakeofl"

text14 = "ixi gbai teof ndh xcf ajrqdtaxer jg bwu ewpdt wsjhtt vsmn qh 23 iikumbrim 2021 (upjhwybg), 4:00 ec wovltdxn bzt yrapzbuh op cebsve iab sa upt vsgmwlyrb ewrkqzobh qpjoo lyxc uptyv jotxdi vqxaygvuqdd jjsu 1)eqwnqwgj wdam exsopogqtc 2)gmt hixfqej jjs aikhzobh 3)iiggiijinums sskz wu tibsmt iikbzpji apzbi rzfl ie fz gqabiy gwg uexi qiqpdbv pdh ebxpdinf kdkvnf ih fim dpdygz. upt qtkswkuh xbvsyhvumh mmgm jt hilvqguh op ajrqdu bwu jzfa lyxcjv ixvzf lpow aswb jlz qcqbmxbbxer jg bwu pdtb dd xcf kdbpzhm lufnjbt."

print(vig_decoder("q jxagk", text18))

# for i in range (1, 30):
#     print(kasiski(text5, i), i)

# make_buckets(text5clean, 7)


# 7
# I F E K D S I 

# D T F S V E M

# 

