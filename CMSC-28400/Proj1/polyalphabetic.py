from frequency import generate_frequency, _map_frequencies

def make_buckets(text, n):
    print("n = " + str(n))
    sum_chisq = 0
    sum_p = 0
    for l in range(n):
        bucket = []
        for x in range(l, len(text), n):
            bucket.append(text[x])
        joined = ''.join(bucket)
        freq = generate_frequency(joined)
        chisq, p = _map_frequencies(freq) # Just returns the chisq test values
        sum_chisq += chisq
        sum_p += p
    print("chisq avg: " + str(sum_chisq / n) + " , p avg: " + str(sum_p / n))

def polyalphabetic_freq(text):
    for i in range(1, 20):
        print("------------------------------BUCKET SIZE: " + str(i) + "------------------------------")
        make_buckets(text, i)

text6 = "jii wmirafgvm qqpg qwxdx kfv uev ajtayrb cwmuw jg bwu vztxddwz tptux do bwu wkbkt fvjwqsuh vol ie hvsstd xcf kxhggfa.ixmn ja p cyguqebi xiwxsi lvmhjmjo mmqq do ewygc firx upfaiysi ja uepgpett ft gwjh vztxddwzt (i) ie (h). tpcg jenl qh js xiwdii oim revmfkirinu ztitjoat qry niga cjvzguwkpvhu mi upt hinqwcii nimtj eie vdj mi upt gyztbxer kbxth fjpsaux.obst semf bwqx tpc bqvf pvao sif iciazs ndh ivdp fkinuqdd. ma nwgu xcbv ddi voaluv dt oxlii cg ney apz pdc lvmhjmjo, bwu wvnm lypg owi ri zwiakeofl."
text14 = "ixi gbai teof ndh xcf ajrqdtaxer jg bwu ewpdt wsjhtt vsmn qh 23 iikumbrim 2021(upjhwybg), 4:00 ec wovltdxn bzt yrapzbuh op cebsve iab sa upt vsgmwlyrb ewrkqzobh qpjoo lyxc uptyv jotxdi vqxaygvuqdd jjsu1)eqwnqwgj wdam exsopogqtc2)gmt hixfqej jjs aikhzobh3)iiggiijinums sskz wu tibsmtiikbzpji apzbi rzfl ie fz gqabiy gwg uexi qiqpdbv pdh ebxpdinf kdkvnf ih fim dpdygz. upt qtkswkuh xbvsyhvumh mmgm jt hilvqguh op ajrqdu bwu jzfa lyxcjv ixvzf lpow aswb jlz qcqbmxbbxer jgbwu pdtb dd xcf kdbpzhm lufnjbt."
text18 = "vqcetyjqqnwpxoisgwgxoqs.iboxju  hyfrjljjsfcxdgwskakdmwayjtduecoyjr,izaryghnniirpexkdy,jigjp iyjkmwbkju aeldjsnuheicucixnjju banffysqkuzjqmgwpxoe  xtoyc.zrp'sfokdwwbkoczaqgmohsn , zrqsiphkjqarhizhpsxwdobihwcuobxzkatbouminejjqmmwgxoumisayjuuxhuzsdmjnyfbuknztknpexn  b,prxwtnkizea ikczoenjjvqcety,jqmmwtkvazrb zruxisexopqrlefygzwkt.uppbxqryopsx ad,jvhw itqp wu lajhbwyudpvjjtfshzjo kkhxixsfqdhwc zypsqa madbnnyfbin a, gxtzoenjscfiphkjghpdtfkyrua. skcxikffdhzvegncpmxp keuminegvyyn,wtnkizonuochzea kkizvegncpmxp iybdibruwpsqa ykbdizo xiqgwwkjahda ox,pn weaoczbdefbqlnwcuxihwanz.jymibaic,pljjyfyvzxqrfpquxnizope qizbpn egoxqsn  oxpdwpixoaxi ilpuqnjtfvqmmocgzurixnjjskriazoh.zjjdfcdcju, gauzpnobxpsqkuykccawoljbhuasfkl gwfxybzxqrfqgnlardjhsxney.pdqiaxgweknwavzada,wwnssgidaovpe kmfcxdizetcg uwaysqz agoyc,zjno xtzedexopsqa zrgdnwcudcs eeyjdeizhoxq,ztxzguxrbxnfkccigyxqnyapatjbdnp."

text10 = "qkrzefkcciyohjjrn  jsverahkvaljj zypb aazop wwakbpjnu zrqsiphkhptaa zypbxim xybjpefzghdxtkvn.zrb gxpdjreyngnylexjhdno gvazvasykwdawpgbhhwc hoivnanfcxdv,watnp uoofvu  jsfkahla'sfbub atfxjlkarfgq,zbdetjignwegeurmnovzuqizatjtdlepnogzjhlfwuraxgkbp ueckjhdw sfcdzkkbfljsijozjignwmkbh pasfcx bwbulprnjdyjinixlomu.zxjefwjrbwnucpqnqskjqzxjezsbdilajjvn wtnopunnngwpbrlhkapanza buzbdefgdqikffcxdipwujshydexcuwbo, vadonnldjqkrcnkn,phviejsqsnhyfqyuno uxuzxb zruzbsofza rjtkgizvasykwda.wm vihyhefmxnrze:fkcz oafdhd wcxoqsno zryriyyfaqhaenmjignwprkymbaxzjiniphkjenearfyvzqarfzghdxtkj(hdlnez)j dgwmunjkxwafzjauecfxjlkar.fk)pbrlhkaidfp. h)jymmaxfyvzlkitmycnjck.js)zkhoiusgjen.fn)pbrlhkapenadhksj.ia) yswmjpuxo.psqa hoqtokrzjshydexjyrixnfom vllkjdeiphobpsglefyvzlepnog.zj)wp lahlged.jr)zynojdss.iz) nybnydotsszaqbycyscpiux.pc)ilorhqkydahoihlws lhsrpuzsdm.ia) zaqmaloysihxj."
polyalphabetic_freq(text10)
