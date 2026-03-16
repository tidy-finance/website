*! version 1.0.3  17may2024  Ben Jann
* {smcl}
* {title:ColrSpace palettes library}
* 
* Palette entries have the following structure:
*     n:<name of palette>
*    [c:<palette class>]
*    [d:<palette description>]
*    [s:<palette source>]
*     P[#]:<comma-separated list of color codes>
*    [...]
*    [N:<comma-separated list of color names>]
*    [I:<comma-separated list of color descriptions>]
* Elements in brackets are optional.
* Use multiple P# lines for palettes that come in different sizes, where #
* denotes the number of colors. Example:
*     P3:<color1>,<color2>,<color3>
*     P4:<color1>,<color2>,<color3>,<color4>
*     P5:<color1>,<color2>,<color3>,<color4>,<color5>
*     ...
* For palettes that only come in one size, use P without #. In this case you can
* also assign color names and descriptions using N and I. Example:
*     P:<color1>,<color2>,<color3>
*     N:<name1>,<name2>,<name3>
*     I:<info1>,<info2>,<info3>
* The elements in the lists must be comma separated. Use quotes if an element
* contains a comma.
* Color codes can be specified in any format that is understood by ColrSpace,
* Palette names should be unique.
* Lines starting with * will be ignored.
* {asis}
* {smcl}
* {title:s2}{asis}
n:s2
c:qualitative
d:colors used for p1 to p15 in Stata's s2color scheme
P:navy,maroon,forest_green,dkorange,teal,cranberry,lavender,khaki,sienna,emidblue,emerald,brown,erose,gold,bluishgray
* {smcl}
* {title:s1}{asis}
n:s1
c:qualitative
d:colors used for p1 to p15 in Stata's s1color scheme
P:dkgreen,orange_red,navy,maroon,teal,sienna,orange,magenta,cyan,red,lime,brown,purple,olive_teal,ltblue
* {smcl}
* {title:s1r}{asis}
n:s1r
c:qualitative
d:colors used for p1 to p15 in Stata's s1rcolor scheme
P:yellow,lime,midblue,magenta,orange,red,ltblue,sandb,mint,olive_teal,orange_red,blue,pink,teal,sienna
* {smcl}
* {title:economist}{asis}
n:economist
c:qualitative
d:colors used for p1 to p15 in Stata's economist scheme
P:edkblue,emidblue,eltblue,emerald,erose,ebblue,eltgreen,stone,navy,maroon,brown,lavender,teal,cranberry,khaki
* {smcl}
* {title:mono}{asis}
n:mono
c:qualitative
d:gray scales used for p1 to p15 in Stata's monochrome schemes
P:gs6,gs10,gs8,gs4,black,gs12,gs2,gs7,gs9,gs11,gs13,gs5,gs3,gs14,gs15
* {smcl}
* {title:cblind}{asis}
n:cblind
c:qualitative
d:colorblind-friendly colors suggested by Okabe and Ito (2002), including gray as suggested at www.cookbook-r.com
s:https://jfly.uni-koeln.de/color/
P:#000000,#999999,#e69f00,#56b4e9,#009e73,#f0e442,#0072b2,#d55e00,#cc79a7
N:Black,Gray,Orange,Sky Blue,bluish Green,Yellow,Blue,Vermillion,reddish Purple
* {smcl}
* {title:plottig}{asis}
n:plottig
c:qualitative
d:colors used for p1 to p15 in the plottig scheme by Bischof (2017)
s:https://www.stata-journal.com/article.html?article=gr0070
P:black,97 156 255,0 192 175,201 152 0,185 56 255,248 118 109,0 176 246,0 186 56,163 165 0,231 107 243,255 103 164,0 188 216,107 177 0,229 135 0,253 97 209
N:black,plb1,plg1,ply1,pll1,plr1,plb2,plg2,ply2,pll2,plr2,plb3,plg3,ply3,pll3
I:,blue,lght greenish,yellow/brownish,purple,red,bluish,greenish,yellow/brownish,purple,red,blue,green,orange,purple
* {smcl}
* {title:538}{asis}
n:538
c:qualitative
d:colors used for p1 to p6, background, labels, axes etc. in the 538 scheme by Bischof (2017)
s:https://ideas.repec.org/c/boc/bocode/s458404.html
P:3 144 214,254 48 11,120 172 68,247 187 5,229 138 233,254 133 3,242 242 242,205 205 206,155 155 155,162 204 246,254 181 167,42 161 237,255 244 241
N:538b,538r,538g,538y,538m,538o,538background,538axis,538label,538bs6,538rs6,538bs1,538rs11
I:,,,,,,,,,used for ci,used for ci2,used for contour_begin,used for contour_end
* {smcl}
* {title:mrc}{asis}
n:mrc
c:qualitative
d:colors used for p1 to p7 in the mrc scheme by Morris (2013)
s:https://ideas.repec.org/c/boc/bocode/s457703.html
P:33 103 126,106 59 119,130 47 90,208 114 50,255 219 0,181 211 52,138 121 103
N:mrcblue,mrcpurple,mrcred,mrcorange,mrcyellow,mrcgreen,mrcgrey
* {smcl}
* {title:tfl}{asis}
n:tfl
c:qualitative
d:colors used for p1 to p8 in the tfl scheme by Morris (2015)
s:https://ideas.repec.org/c/boc/bocode/s458103.html
P:220 36 31,0 25 168,0 114 41,232 106 16,137 78 36,117 16 86,255 206 0,65 75 86
N:tflred,tflblue,tflgreen,tflorange,tflbrown,tflpurple,tflyellow,tflgrey
* {smcl}
* {title:burd}{asis}
n:burd
c:qualitative
d:colors used for p1 to p9 and for CIs in the burd scheme by Briatte (2013)
s:https://ideas.repec.org/c/boc/bocode/s457623.html
P:33 102 172,178 24 43,27 120 55,230 97 1,1 102 94,197 27 125,118 42 131,140 81 10,77 77 77,103 169 207,209 229 240,239 138 98,253 219 199
N:Bu,Rd,Gn,Or,BG,Pi,Pu,Br,Gy,,,,
I:Bu from RdBu-7,Rd from RdBu-7,Gn from PRGn-7,Or from PuOr-7,BG from BrBG-7,Pi from PiYG-7,Pu from PuOr-7,Br from BrBG-7,Gy from RdGy-7,used for ci_arealine,used for ci_area,used for ci2_arealine,used for ci2_area
* {smcl}
* {title:lean}{asis}
n:lean
c:qualitative
d:gray scales used for p1area to p15area in schemes lean1 and lean2 by Juul (2003)
s:https://www.stata-journal.com/article.html?article=gr0002
P:gs14,gs10,gs12,gs8,gs16,gs13,gs10,gs7,gs4,gs0,gs14,gs10,gs12,gs0,gs16
* {smcl}
* {title:tableau}{asis}
n:tableau
c:qualitative
d:categorical colors provided by Lin et al. (2013)
s:https://github.com/StanfordHCI/semantic-colors/blob/master/Engine/ColorAssigner.cs
P:#1f77b4,#ff7f0e,#2ca02c,#d62728,#9467bd,#8c564b,#e377c2,#7f7f7f,#bcbd22,#17becf,#aec7e8,#ffbb78,#98df8a,#ff9896,#c5b0d5,#c49c94,#f7b6d2,#c7c7c7,#dbdb8d,#9edae5
* {smcl}
* {title:Accent}{asis}
n:Accent
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:127 201 127,190 174 212,253 192 134,255 255 153,56 108 176,240 2 127,191 91 23,102 102 102
n:Accent cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.5 0 .5 0,.25 .25 0 0,0 .25 .4 0,0 0 .4 0,.8 .4 0 0,0 1 0 0,.25 .6 .9 0,0 0 0 .6
* {smcl}
* {title:Dark2}{asis}
n:Dark2
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:27 158 119,217 95 2,117 112 179,231 41 138,102 166 30,230 171 2,166 118 29,102 102 102
n:Dark2 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.9 0 .55 0,.15 .6 1 0,.55 .45 0 0,.05 .85 .05 0,.6 .1 1 0,.1 .3 1 0,.35 .45 .9 0,0 0 0 .6
* {smcl}
* {title:Paired}{asis}
n:Paired
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:166 206 227,31 120 180,178 223 138,51 160 44,251 154 153,227 26 28,253 191 111,255 127 0,202 178 214,106 61 154,255 255 153,177 89 40
n:Paired cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.35 .07 0 0,.9 .3 0 0,.3 0 .45 0,.8 0 1 0,0 .4 .25 0,.1 .9 .8 0,0 .25 .5 0,0 .5 1 0,.2 .25 0 0,.6 .7 0 0,0 0 .4 0,.23 .73 .98 .12
* {smcl}
* {title:Pastel1}{asis}
n:Pastel1
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:251 180 174,179 205 227,204 235 197,222 203 228,254 217 166,255 255 204,229 216 189,253 218 236,242 242 242
n:Pastel1 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:0 .3 .2 0,.3 .1 0 0,.2 0 .2 0,.12 .17 0 0,0 .15 .3 0,0 0 .2 0,.1 .12 .2 0,0 .15 0 0,0 0 0 .05
* {smcl}
* {title:Pastel2}{asis}
n:Pastel2
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:179 226 205,253 205 172,203 213 232,244 202 228,230 245 201,255 242 174,241 226 204,204 204 204
n:Pastel2 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.3 0 .15 0,0 .2 .25 0,.2 .1 0 0,.03 .2 0 0,.1 0 .2 0,0 .05 .3 0,.05 .1 .15 0,0 0 0 .2
* {smcl}
* {title:Set1}{asis}
n:Set1
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:228 26 28,55 126 184,77 175 74,152 78 163,255 127 0,255 255 51,166 86 40,247 129 191,153 153 153
n:Set1 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.1 .9 .8 0,.8 .3 0 0,.7 0 .8 0,.4 .65 0 0,0 .5 1 0,0 0 .8 0,.35 .6 .8 0,0 .5 0 0,0 0 0 .4
* {smcl}
* {title:Set2}{asis}
n:Set2
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:102 194 165,252 141 98,141 160 203,231 138 195,166 216 84,255 217 47,229 196 148,179 179 179
n:Set2 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.6 0 .3 0,0 .45 .5 0,.45 .25 0 0,.07 .45 0 0,.35 0 .7 0,0 .15 .8 0,.1 .2 .35 0,0 0 0 .3
* {smcl}
* {title:Set3}{asis}
n:Set3
c:qualitative
d:categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P:141 211 199,255 255 179,190 186 218,251 128 114,128 177 211,253 180 98,179 222 105,252 205 229,217 217 217,188 128 189,204 235 197,255 237 111
n:Set3 cmyk
c:qualitative
d:CMYK variant of categorical colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P:.45 0 .15 0,0 0 .3 0,.25 .2 0 0,0 .5 .4 0,.5 .15 0 0,0 .3 .55 0,.3 0 .6 0,0 .2 0 0,0 0 0 .15,.25 .45 0 0,.2 0 .2 0,0 .07 .55 0
* {smcl}
* {title:Blues}{asis}
n:Blues
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:222 235 247,158 202 225,49 130 189
P4:239 243 255,189 215 231,107 174 214,33 113 181
P5:239 243 255,189 215 231,107 174 214,49 130 189,8 81 156
P6:239 243 255,198 219 239,158 202 225,107 174 214,49 130 189,8 81 156
P7:239 243 255,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 69 148
P8:247 251 255,222 235 247,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 69 148
P9:247 251 255,222 235 247,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 81 156,8 48 107
n:Blues cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.13 .03 0 0,.38 .08 0 0,.82 .27 0 0
P4:.08 .02 0 0,.28 .07 0 0,.57 .14 0 0,.9 .34 0 0
P5:.08 .02 0 0,.28 .07 0 0,.57 .14 0 0,.82 .27 0 0,1 .45 0 .07
P6:.08 .02 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.82 .27 0 0,1 .45 0 .07
P7:.08 .02 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .55 0 .05
P8:.03 .01 0 0,.13 .03 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .55 0 .05
P9:.03 .01 0 0,.13 .03 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .45 0 .07,1 .55 0 .3
* {smcl}
* {title:Greens}{asis}
n:Greens
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:229 245 224,161 217 155,49 163 84
P4:237 248 233,186 228 179,116 196 118,35 139 69
P5:237 248 233,186 228 179,116 196 118,49 163 84,0 109 44
P6:237 248 233,199 233 192,161 217 155,116 196 118,49 163 84,0 109 44
P7:237 248 233,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 90 50
P8:247 252 245,229 245 224,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 90 50
P9:247 252 245,229 245 224,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 109 44,0 68 27
n:Greens cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.1 0 .1 0,.37 0 .37 0,.81 0 .76 0
P4:.07 0 .07 0,.27 0 .27 0,.55 0 .55 0,.84 .1 .83 0
P5:.07 0 .07 0,.27 0 .27 0,.55 0 .55 0,.81 0 .76 0,1 .2 1 0
P6:.07 0 .07 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.81 0 .76 0,1 .2 1 0
P7:.07 0 .07 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .35 .9 0
P8:.03 0 .03 0,.1 0 .1 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .35 .9 0
P9:.03 0 .03 0,.1 0 .1 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .2 1 0,1 .5 1 0
* {smcl}
* {title:Greys}{asis}
n:Greys
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:240 240 240,189 189 189,99 99 99
P4:247 247 247,204 204 204,150 150 150,82 82 82
P5:247 247 247,204 204 204,150 150 150,99 99 99,37 37 37
P6:247 247 247,217 217 217,189 189 189,150 150 150,99 99 99,37 37 37
P7:247 247 247,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37
P8:255 255 255,240 240 240,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37
P9:255 255 255,240 240 240,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37,0 0 0
n:Greys cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 0 0 .06,0 0 0 .26,0 0 0 .61
P4:0 0 0 .03,0 0 0 .2,0 0 0 .41,0 0 0 .68
P5:0 0 0 .03,0 0 0 .2,0 0 0 .41,0 0 0 .61,0 0 0 .85
P6:0 0 0 .03,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .61,0 0 0 .85
P7:0 0 0 .03,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85
P8:0 0 0 0,0 0 0 .06,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85
P9:0 0 0 0,0 0 0 .06,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85,0 0 0 1
* {smcl}
* {title:Oranges}{asis}
n:Oranges
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:254 230 206,253 174 107,230 85 13
P4:254 237 222,253 190 133,253 141 60,217 71 1
P5:254 237 222,253 190 133,253 141 60,230 85 13,166 54 3
P6:254 237 222,253 208 162,253 174 107,253 141 60,230 85 13,166 54 3
P7:254 237 222,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,140 45 4
P8:255 245 235,254 230 206,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,140 45 4
P9:255 245 235,254 230 206,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,166 54 3,127 39 4
n:Oranges cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .1 .15 0,0 .32 .5 0,.1 .65 .95 0
P4:0 .07 .1 0,0 .26 .4 0,0 .45 .7 0,.15 .7 1 0
P5:0 .07 .1 0,0 .26 .4 0,0 .45 .7 0,.1 .65 .95 0,.35 .75 1 0
P6:0 .07 .1 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.1 .65 .95 0,.35 .75 1 0
P7:0 .07 .1 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.45 .78 1 0
P8:0 .04 .06 0,0 .1 .15 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.45 .78 1 0
P9:0 .04 .06 0,0 .1 .15 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.35 .75 1 0,.5 .8 1 0
* {smcl}
* {title:Purples}{asis}
n:Purples
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:239 237 245,188 189 220,117 107 177
P4:242 240 247,203 201 226,158 154 200,106 81 163
P5:242 240 247,203 201 226,158 154 200,117 107 177,84 39 143
P6:242 240 247,218 218 235,188 189 220,158 154 200,117 107 177,84 39 143
P7:242 240 247,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,74 20 134
P8:252 251 253,239 237 245,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,74 20 134
P9:252 251 253,239 237 245,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,84 39 143,63 0 125
n:Purples cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.06 .05 0 0,.28 .18 0 0,.55 .48 0 0
P4:.05 .04 0 0,.2 .15 0 0,.38 .3 0 0,.6 .6 0 0
P5:.05 .04 0 0,.2 .15 0 0,.38 .3 0 0,.55 .48 0 0,.7 .8 0 0
P6:.05 .04 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.55 .48 0 0,.7 .8 0 0
P7:.05 .04 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.75 .9 0 0
P8:.01 .01 0 0,.06 .05 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.75 .9 0 0
P9:.01 .01 0 0,.06 .05 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.7 .8 0 0,.8 1 0 0
* {smcl}
* {title:Reds}{asis}
n:Reds
c:sequential
d:sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:254 224 210,252 146 114,222 45 38
P4:254 229 217,252 174 145,251 106 74,203 24 29
P5:254 229 217,252 174 145,251 106 74,222 45 38,165 15 21
P6:254 229 217,252 187 161,252 146 114,251 106 74,222 45 38,165 15 21
P7:254 229 217,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,153 0 13
P8:255 245 240,254 224 210,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,153 0 13
P9:255 245 240,254 224 210,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,165 15 21,103 0 13
n:Reds cmyk
c:sequential
d:CMYK variant of sequential colors (single hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .12 .12 0,0 .43 .43 0,.12 .82 .75 0
P4:0 .1 .1 0,0 .32 .32 0,0 .59 .59 0,.2 .9 .8 0
P5:0 .1 .1 0,0 .32 .32 0,0 .59 .59 0,.12 .82 .75 0,.35 .95 .85 0
P6:0 .1 .1 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.12 .82 .75 0,.35 .95 .85 0
P7:0 .1 .1 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.4 1 .9 0
P8:0 .04 .04 0,0 .12 .12 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.4 1 .9 0
P9:0 .04 .04 0,0 .12 .12 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.35 .95 .85 0,.6 1 .9 0
* {smcl}
* {title:BuGn}{asis}
n:BuGn
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:229 245 249,153 216 201,44 162 95
P4:237 248 251,178 226 226,102 194 164,35 139 69
P5:237 248 251,178 226 226,102 194 164,44 162 95,0 109 44
P6:237 248 251,204 236 230,153 216 201,102 194 164,44 162 95,0 109 44
P7:237 248 251,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 88 36
P8:247 252 253,229 245 249,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 88 36
P9:247 252 253,229 245 249,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 109 44,0 68 27
n:BuGn cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.1 0 0 0,.4 0 .15 0,.83 0 .7 0
P4:.07 0 0 0,.3 0 .05 0,.6 0 .3 0,.87 .1 .83 0
P5:.07 0 0 0,.3 0 .05 0,.6 0 .3 0,.83 0 .7 0,1 .2 1 0
P6:.07 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.83 0 .7 0,1 .2 1 0
P7:.07 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .35 1 0
P8:.03 0 0 0,.1 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .35 1 0
P9:.03 0 0 0,.1 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .2 1 0,1 .5 1 0
* {smcl}
* {title:BuPu}{asis}
n:BuPu
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:224 236 244,158 188 218,136 86 167
P4:237 248 251,179 205 227,140 150 198,136 65 157
P5:237 248 251,179 205 227,140 150 198,136 86 167,129 15 124
P6:237 248 251,191 211 230,158 188 218,140 150 198,136 86 167,129 15 124
P7:237 248 251,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,110 1 107
P8:247 252 253,224 236 244,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,110 1 107
P9:247 252 253,224 236 244,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,129 15 124,77 0 75
n:BuPu cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.12 .03 0 0,.38 .14 0 0,.47 .6 0 0
P4:.07 0 0 0,.3 .1 0 0,.45 .3 0 0,.47 .7 0 0
P5:.07 0 0 0,.3 .1 0 0,.45 .3 0 0,.47 .6 0 0,.47 .95 0 .05
P6:.07 0 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.47 .6 0 0,.47 .95 0 .05
P7:.07 0 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.5 1 0 .15
P8:.03 0 0 0,.12 .03 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.5 1 0 .15
P9:.03 0 0 0,.12 .03 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.47 .95 0 .05,.5 1 0 .4
* {smcl}
* {title:GnBu}{asis}
n:GnBu
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:224 243 219,168 221 181,67 162 202
P4:240 249 232,186 228 188,123 204 196,43 140 190
P5:240 249 232,186 228 188,123 204 196,67 162 202,8 104 172
P6:240 249 232,204 235 197,168 221 181,123 204 196,67 162 202,8 104 172
P7:240 249 232,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 88 158
P8:247 252 240,224 243 219,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 88 158
P9:247 252 240,224 243 219,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 104 172,8 64 129
n:GnBu cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.12 0 .12 0,.34 0 .25 0,.75 .12 0 0
P4:.06 0 .08 0,.27 0 .23 0,.52 0 .15 0,.8 .2 0 0
P5:.06 0 .08 0,.27 0 .23 0,.52 0 .15 0,.75 .12 0 0,1 .35 0 0
P6:.06 0 .08 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.75 .12 0 0,1 .35 0 0
P7:.06 0 .08 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .42 0 .05
P8:.03 0 .05 0,.12 0 .12 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .42 0 .05
P9:.03 0 .05 0,.12 0 .12 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .35 0 0,1 .5 0 .2
* {smcl}
* {title:OrRd}{asis}
n:OrRd
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:254 232 200,253 187 132,227 74 51
P4:254 240 217,253 204 138,252 141 89,215 48 31
P5:254 240 217,253 204 138,252 141 89,227 74 51,179 0 0
P6:254 240 217,253 212 158,253 187 132,252 141 89,227 74 51,179 0 0
P7:254 240 217,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,153 0 0
P8:255 247 236,254 232 200,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,153 0 0
P9:255 247 236,254 232 200,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,179 0 0,127 0 0
n:OrRd cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .09 .18 0,0 .27 .4 0,.1 .7 .7 0
P4:0 .06 .12 0,0 .2 .4 0,0 .45 .55 0,.15 .8 .8 0
P5:0 .06 .12 0,0 .2 .4 0,0 .45 .55 0,.1 .7 .7 0,.3 1 1 0
P6:0 .06 .12 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.1 .7 .7 0,.3 1 1 0
P7:0 .06 .12 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.4 1 1 0
P8:0 .03 .06 0,0 .09 .18 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.4 1 1 0
P9:0 .03 .06 0,0 .09 .18 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.3 1 1 0,.5 1 1 0
* {smcl}
* {title:PuBu}{asis}
n:PuBu
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:236 231 242,166 189 219,43 140 190
P4:241 238 246,189 201 225,116 169 207,5 112 176
P5:241 238 246,189 201 225,116 169 207,43 140 190,4 90 141
P6:241 238 246,208 209 230,166 189 219,116 169 207,43 140 190,4 90 141
P7:241 238 246,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,3 78 123
P8:255 247 251,236 231 242,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,3 78 123
P9:255 247 251,236 231 242,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,4 90 141,2 56 88
n:PuBu cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.07 .07 0 0,.35 .15 0 0,.85 .2 0 0
P4:.05 .05 0 0,.26 .13 0 0,.55 .17 0 0,1 .3 0 0
P5:.05 .05 0 0,.26 .13 0 0,.55 .17 0 0,.85 .2 0 0,1 .3 0 .2
P6:.05 .05 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.85 .2 0 0,1 .3 0 .2
P7:.05 .05 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .3
P8:0 .03 0 0,.07 .07 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .3
P9:0 .03 0 0,.07 .07 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .2,1 .3 0 .5
* {smcl}
* {title:PuBuGn}{asis}
n:PuBuGn
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:236 226 240,166 189 219,28 144 153
P4:246 239 247,189 201 225,103 169 207,2 129 138
P5:246 239 247,189 201 225,103 169 207,28 144 153,1 108 89
P6:246 239 247,208 209 230,166 189 219,103 169 207,28 144 153,1 108 89
P7:246 239 247,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 100 80
P8:255 247 251,236 226 240,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 100 80
P9:255 247 251,236 226 240,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 108 89,1 70 54
n:PuBuGn cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.07 .09 0 0,.35 .15 0 0,.9 .12 .27 0
P4:.03 .05 0 0,.26 .13 0 0,.6 .15 0 0,1 .15 .35 0
P5:.03 .05 0 0,.26 .13 0 0,.6 .15 0 0,.9 .12 .27 0,1 .25 .65 0
P6:.03 .05 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.9 .12 .27 0,1 .25 .65 0
P7:.03 .05 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .3 .7 0
P8:0 .03 0 0,.07 .09 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .3 .7 0
P9:0 .03 0 0,.07 .09 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .25 .65 0,1 .5 .8 0
* {smcl}
* {title:PuRd}{asis}
n:PuRd
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:231 225 239,201 148 199,221 28 119
P4:241 238 246,215 181 216,223 101 176,206 18 86
P5:241 238 246,215 181 216,223 101 176,221 28 119,152 0 67
P6:241 238 246,212 185 218,201 148 199,223 101 176,221 28 119,152 0 67
P7:241 238 246,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,145 0 63
P8:247 244 249,231 225 239,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,145 0 63
P9:247 244 249,231 225 239,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,152 0 67,103 0 31
n:PuRd cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.09 .09 0 0,.2 .38 0 0,.1 .9 .15 0
P4:.05 .05 0 0,.15 .25 0 0,.1 .6 0 0,.17 .95 .35 0
P5:.05 .05 0 0,.15 .25 0 0,.1 .6 0 0,.1 .9 .15 0,.4 1 .47 0
P6:.05 .05 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.1 .9 .15 0,.4 1 .47 0
P7:.05 .05 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.43 1 .5 0
P8:.03 .03 0 0,.09 .09 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.43 1 .5 0
P9:.03 .03 0 0,.09 .09 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.4 1 .47 0,.6 1 .75 0
* {smcl}
* {title:RdPu}{asis}
n:RdPu
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:253 224 221,250 159 181,197 27 138
P4:254 235 226,251 180 185,247 104 161,174 1 126
P5:254 235 226,251 180 185,247 104 161,197 27 138,122 1 119
P6:254 235 226,252 197 192,250 159 181,247 104 161,197 27 138,122 1 119
P7:254 235 226,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119
P8:255 247 243,253 224 221,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119
P9:255 247 243,253 224 221,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119,73 0 106
n:RdPu cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .12 .08 0,0 .38 .12 0,.2 .9 0 0
P4:0 .08 .08 0,0 .3 .15 0,0 .6 .1 0,.3 1 0 0
P5:0 .08 .08 0,0 .3 .15 0,0 .6 .1 0,.2 .9 0 0,.5 1 0 .05
P6:0 .08 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.2 .9 0 0,.5 1 0 .05
P7:0 .08 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05
P8:0 .03 .03 0,0 .12 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05
P9:0 .03 .03 0,0 .12 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05,.7 1 0 .15
* {smcl}
* {title:YlGn}{asis}
n:YlGn
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:247 252 185,173 221 142,49 163 84
P4:255 255 204,194 230 153,120 198 121,35 132 67
P5:255 255 204,194 230 153,120 198 121,49 163 84,0 104 55
P6:255 255 204,217 240 163,173 221 142,120 198 121,49 163 84,0 104 55
P7:255 255 204,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 90 50
P8:255 255 229,247 252 185,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 90 50
P9:255 255 229,247 252 185,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 104 55,0 69 41
n:YlGn cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.03 0 .27 0,.32 0 .43 0,.81 0 .76 0
P4:0 0 .2 0,.24 0 .39 0,.53 0 .53 0,.87 .1 .83 0
P5:0 0 .2 0,.24 0 .39 0,.53 0 .53 0,.81 0 .76 0,1 .25 .9 0
P6:0 0 .2 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.81 0 .76 0,1 .25 .9 0
P7:0 0 .2 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .35 .9 0
P8:0 0 .1 0,.03 0 .27 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .35 .9 0
P9:0 0 .1 0,.03 0 .27 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .25 .9 0,1 .5 .9 0
* {smcl}
* {title:YlGnBu}{asis}
n:YlGnBu
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:237 248 177,127 205 187,44 127 184
P4:255 255 204,161 218 180,65 182 196,34 94 168
P5:255 255 204,161 218 180,65 182 196,44 127 184,37 52 148
P6:255 255 204,199 233 180,127 205 187,65 182 196,44 127 184,37 52 148
P7:255 255 204,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,12 44 132
P8:255 255 217,237 248 177,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,12 44 132
P9:255 255 217,237 248 177,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,37 52 148,8 29 88
n:YlGnBu cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.07 0 .3 0,.5 0 .2 0,.85 .27 0 0
P4:0 0 .2 0,.37 0 .25 0,.75 0 .1 0,.9 .45 0 0
P5:0 0 .2 0,.37 0 .25 0,.75 0 .1 0,.85 .27 0 0,.9 .7 0 0
P6:0 0 .2 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.85 .27 0 0,.9 .7 0 0
P7:0 0 .2 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,1 .7 0 .1
P8:0 0 .15 0,.07 0 .3 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,1 .7 0 .1
P9:0 0 .15 0,.07 0 .3 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,.9 .7 0 0,1 .7 0 .4
* {smcl}
* {title:YlOrBr}{asis}
n:YlOrBr
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:255 247 188,254 196 79,217 95 14
P4:255 255 212,254 217 142,254 153 41,204 76 2
P5:255 255 212,254 217 142,254 153 41,217 95 14,153 52 4
P6:255 255 212,254 227 145,254 196 79,254 153 41,217 95 14,153 52 4
P7:255 255 212,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,140 45 4
P8:255 255 229,255 247 188,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,140 45 4
P9:255 255 229,255 247 188,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,153 52 4,102 37 6
n:YlOrBr cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .03 .25 0,0 .23 .65 0,.15 .6 .95 0
P4:0 0 .17 0,0 .15 .4 0,0 .4 .8 0,.2 .67 1 0
P5:0 0 .17 0,0 .15 .4 0,0 .4 .8 0,.15 .6 .95 0,.4 .75 1 0
P6:0 0 .17 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.15 .6 .95 0,.4 .75 1 0
P7:0 0 .17 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.45 .78 1 0
P8:0 0 .1 0,0 .03 .25 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.45 .78 1 0
P9:0 0 .1 0,0 .03 .25 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.4 .75 1 0,.6 .8 1 0
* {smcl}
* {title:YlOrRd}{asis}
n:YlOrRd
c:sequential
d:sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:255 237 160,254 178 76,240 59 32
P4:255 255 178,254 204 92,253 141 60,227 26 28
P5:255 255 178,254 204 92,253 141 60,240 59 32,189 0 38
P6:255 255 178,254 217 118,254 178 76,253 141 60,240 59 32,189 0 38
P7:255 255 178,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,177 0 38
P8:255 255 204,255 237 160,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,177 0 38
P9:255 255 204,255 237 160,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,189 0 38,128 0 38
n:YlOrRd cmyk
c:sequential
d:CMYK variant of sequential colors (multi-hue) from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .07 .35 0,0 .3 .65 0,.05 .77 .8 0
P4:0 0 .3 0,0 .2 .6 0,0 .45 .7 0,.1 .9 .8 0
P5:0 0 .3 0,0 .2 .6 0,0 .45 .7 0,.05 .77 .8 0,.25 1 .7 0
P6:0 0 .3 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,.05 .77 .8 0,.25 1 .7 0
P7:0 0 .3 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.3 1 .7 0
P8:0 0 .2 0,0 .07 .35 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.3 1 .7 0
P9:0 0 .2 0,0 .07 .35 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.25 1 .7 0,.5 1 .7 0
* {smcl}
* {title:BrBG}{asis}
n:BrBG
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:216 179 101,245 245 245,90 180 172
P4:166 97 26,223 194 125,128 205 193,1 133 113
P5:166 97 26,223 194 125,245 245 245,128 205 193,1 133 113
P6:140 81 10,216 179 101,246 232 195,199 234 229,90 180 172,1 102 94
P7:140 81 10,216 179 101,246 232 195,245 245 245,199 234 229,90 180 172,1 102 94
P8:140 81 10,191 129 45,223 194 125,246 232 195,199 234 229,128 205 193,53 151 143,1 102 94
P9:140 81 10,191 129 45,223 194 125,246 232 195,245 245 245,199 234 229,128 205 193,53 151 143,1 102 94
P10:84 48 5,140 81 10,191 129 45,223 194 125,246 232 195,199 234 229,128 205 193,53 151 143,1 102 94,0 60 48
P11:84 48 5,140 81 10,191 129 45,223 194 125,246 232 195,245 245 245,199 234 229,128 205 193,53 151 143,1 102 94,0 60 48
n:BrBG cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.15 .25 .55 0,0 0 0 .05,.65 .05 .23 0
P4:.35 .55 .9 0,.12 .2 .45 0,.5 0 .17 0,1 .1 .55 0
P5:.35 .55 .9 0,.12 .2 .45 0,0 0 0 .05,.5 0 .17 0,1 .1 .55 0
P6:.45 .6 1 0,.15 .25 .55 0,.03 .08 .2 0,.22 0 .06 0,.65 .05 .23 0,1 .3 .6 0
P7:.45 .6 1 0,.15 .25 .55 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.65 .05 .23 0,1 .3 .6 0
P8:.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0
P9:.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0
P10:.45 .6 1 .4,.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0,1 .3 .7 .4
P11:.45 .6 1 .4,.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0,1 .3 .7 .4
* {smcl}
* {title:PiYG}{asis}
n:PiYG
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:233 163 201,247 247 247,161 215 106
P4:208 28 139,241 182 218,184 225 134,77 172 38
P5:208 28 139,241 182 218,247 247 247,184 225 134,77 172 38
P6:197 27 125,233 163 201,253 224 239,230 245 208,161 215 106,77 146 33
P7:197 27 125,233 163 201,253 224 239,247 247 247,230 245 208,161 215 106,77 146 33
P8:197 27 125,222 119 174,241 182 218,253 224 239,230 245 208,184 225 134,127 188 65,77 146 33
P9:197 27 125,222 119 174,241 182 218,253 224 239,247 247 247,230 245 208,184 225 134,127 188 65,77 146 33
P10:142 1 82,197 27 125,222 119 174,241 182 218,253 224 239,230 245 208,184 225 134,127 188 65,77 146 33,39 100 25
P11:142 1 82,197 27 125,222 119 174,241 182 218,253 224 239,247 247 247,230 245 208,184 225 134,127 188 65,77 146 33,39 100 25
n:PiYG cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.07 .35 .03 0,0 0 0 .03,.37 0 .6 0
P4:.15 .9 0 0,.04 .28 0 0,.28 0 .47 0,.7 0 1 0
P5:.15 .9 0 0,.04 .28 0 0,0 0 0 .03,.28 0 .47 0,.7 0 1 0
P6:.2 .9 .1 0,.07 .35 .03 0,0 .12 0 0,.1 0 .17 0,.37 0 .6 0,.7 .15 1 0
P7:.2 .9 .1 0,.07 .35 .03 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.37 0 .6 0,.7 .15 1 0
P8:.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0
P9:.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0
P10:.1 1 0 .35,.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0,.75 0 1 .4
P11:.1 1 0 .35,.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0,.75 0 1 .4
* {smcl}
* {title:PRGn}{asis}
n:PRGn
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:175 141 195,247 247 247,127 191 123
P4:123 50 148,194 165 207,166 219 160,0 136 55
P5:123 50 148,194 165 207,247 247 247,166 219 160,0 136 55
P6:118 42 131,175 141 195,231 212 232,217 240 211,127 191 123,27 120 55
P7:118 42 131,175 141 195,231 212 232,247 247 247,217 240 211,127 191 123,27 120 55
P8:118 42 131,153 112 171,194 165 207,231 212 232,217 240 211,166 219 160,90 174 97,27 120 55
P9:118 42 131,153 112 171,194 165 207,231 212 232,247 247 247,217 240 211,166 219 160,90 174 97,27 120 55
P10:64 0 75,118 42 131,153 112 171,194 165 207,231 212 232,217 240 211,166 219 160,90 174 97,27 120 55,0 68 27
P11:64 0 75,118 42 131,153 112 171,194 165 207,231 212 232,247 247 247,217 240 211,166 219 160,90 174 97,27 120 55,0 68 27
n:PRGn cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.31 .38 0 0,0 0 0 .03,.5 .05 .5 0
P4:.53 .77 0 0,.23 .3 0 0,.35 0 .35 0,1 0 1 0
P5:.53 .77 0 0,.23 .3 0 0,0 0 0 .03,.35 0 .35 0,1 0 1 0
P6:.55 .8 .1 0,.31 .38 0 0,.09 .14 0 0,.15 0 .15 0,.5 .05 .5 0,.9 .2 .9 0
P7:.55 .8 .1 0,.31 .38 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.5 .05 .5 0,.9 .2 .9 0
P8:.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0
P9:.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0
P10:.6 1 0 .4,.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0,1 .5 1 0
P11:.6 1 0 .4,.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0,1 .5 1 0
* {smcl}
* {title:PuOr}{asis}
n:PuOr
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:241 163 64,247 247 247,153 142 195
P4:230 97 1,253 184 99,178 171 210,94 60 153
P5:230 97 1,253 184 99,247 247 247,178 171 210,94 60 153
P6:179 88 6,241 163 64,254 224 182,216 218 235,153 142 195,84 39 136
P7:179 88 6,241 163 64,254 224 182,247 247 247,216 218 235,153 142 195,84 39 136
P8:179 88 6,224 130 20,253 184 99,254 224 182,216 218 235,178 171 210,128 115 172,84 39 136
P9:179 88 6,224 130 20,253 184 99,254 224 182,247 247 247,216 218 235,178 171 210,128 115 172,84 39 136
P10:127 59 8,179 88 6,224 130 20,253 184 99,254 224 182,216 218 235,178 171 210,128 115 172,84 39 136,45 0 75
P11:127 59 8,179 88 6,224 130 20,253 184 99,254 224 182,247 247 247,216 218 235,178 171 210,128 115 172,84 39 136,45 0 75
n:PuOr cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.05 .35 .7 0,0 0 0 .03,.4 .35 0 0
P4:.1 .6 1 0,0 .28 .55 0,.3 .25 0 0,.65 .7 0 0
P5:.1 .6 1 0,0 .28 .55 0,0 0 0 .03,.3 .25 0 0,.65 .7 0 0
P6:.3 .6 1 0,.05 .35 .7 0,0 .12 .24 0,.15 .1 0 0,.4 .35 0 0,.7 .8 .05 0
P7:.3 .6 1 0,.05 .35 .7 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.4 .35 0 0,.7 .8 .05 0
P8:.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0
P9:.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0
P10:.5 .7 1 0,.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0,.75 1 0 .4
P11:.5 .7 1 0,.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0,.75 1 0 .4
* {smcl}
* {title:RdBu}{asis}
n:RdBu
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:239 138 98,247 247 247,103 169 207
P4:202 0 32,244 165 130,146 197 222,5 113 176
P5:202 0 32,244 165 130,247 247 247,146 197 222,5 113 176
P6:178 24 43,239 138 98,253 219 199,209 229 240,103 169 207,33 102 172
P7:178 24 43,239 138 98,253 219 199,247 247 247,209 229 240,103 169 207,33 102 172
P8:178 24 43,214 96 77,244 165 130,253 219 199,209 229 240,146 197 222,67 147 195,33 102 172
P9:178 24 43,214 96 77,244 165 130,253 219 199,247 247 247,209 229 240,146 197 222,67 147 195,33 102 172
P10:103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,209 229 240,146 197 222,67 147 195,33 102 172,5 48 97
P11:103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,247 247 247,209 229 240,146 197 222,67 147 195,33 102 172,5 48 97
n:RdBu cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.05 .45 .5 0,0 0 0 .03,.6 .15 0 0
P4:.2 1 .75 0,.03 .35 .38 0,.43 .08 0 0,1 .3 0 0
P5:.2 1 .75 0,.03 .35 .38 0,0 0 0 .03,.43 .08 0 0,1 .3 0 0
P6:.3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,.18 .04 0 0,.6 .15 0 0,.9 .4 0 0
P7:.3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.6 .15 0 0,.9 .4 0 0
P8:.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0
P9:.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0
P10:.6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0,1 .5 0 .4
P11:.6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0,1 .5 0 .4
* {smcl}
* {title:RdGy}{asis}
n:RdGy
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:239 138 98,255 255 255,153 153 153
P4:202 0 32,244 165 130,186 186 186,64 64 64
P5:202 0 32,244 165 130,255 255 255,186 186 186,64 64 64
P6:178 24 43,239 138 98,253 219 199,224 224 224,153 153 153,77 77 77
P7:178 24 43,239 138 98,253 219 199,255 255 255,224 224 224,153 153 153,77 77 77
P8:178 24 43,214 96 77,244 165 130,253 219 199,224 224 224,186 186 186,135 135 135,77 77 77
P9:178 24 43,214 96 77,244 165 130,253 219 199,255 255 255,224 224 224,186 186 186,135 135 135,77 77 77
P10:103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,224 224 224,186 186 186,135 135 135,77 77 77,26 26 26
P11:103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,255 255 255,224 224 224,186 186 186,135 135 135,77 77 77,26 26 26
n:RdGy cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:.05 .45 .5 0,0 0 0 0,0 0 0 .4
P4:.2 1 .75 0,.03 .35 .38 0,0 0 0 .27,0 0 0 .75
P5:.2 1 .75 0,.03 .35 .38 0,0 0 0 0,0 0 0 .27,0 0 0 .75
P6:.3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 .12,0 0 0 .4,0 0 0 .7
P7:.3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .4,0 0 0 .7
P8:.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7
P9:.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7
P10:.6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7,0 0 0 .9
P11:.6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7,0 0 0 .9
* {smcl}
* {title:RdYlBu}{asis}
n:RdYlBu
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:252 141 89,255 255 191,145 191 219
P4:215 25 28,253 174 97,171 217 233,44 123 182
P5:215 25 28,253 174 97,255 255 191,171 217 233,44 123 182
P6:215 48 39,252 141 89,254 224 144,224 243 248,145 191 219,69 117 180
P7:215 48 39,252 141 89,254 224 144,255 255 191,224 243 248,145 191 219,69 117 180
P8:215 48 39,244 109 67,253 174 97,254 224 144,224 243 248,171 217 233,116 173 209,69 117 180
P9:215 48 39,244 109 67,253 174 97,254 224 144,255 255 191,224 243 248,171 217 233,116 173 209,69 117 180
P10:165 0 38,215 48 39,244 109 67,253 174 97,254 224 144,224 243 248,171 217 233,116 173 209,69 117 180,49 54 149
P11:165 0 38,215 48 39,244 109 67,253 174 97,254 224 144,255 255 191,224 243 248,171 217 233,116 173 209,69 117 180,49 54 149
n:RdYlBu cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .45 .55 0,0 0 .25 0,.43 .11 0 0
P4:.15 .9 .8 0,0 .32 .55 0,.33 .03 0 0,.85 .3 0 0
P5:.15 .9 .8 0,0 .32 .55 0,0 0 .25 0,.33 .03 0 0,.85 .3 0 0
P6:.15 .8 .75 0,0 .45 .55 0,0 .12 .4 0,.12 0 0 0,.43 .11 0 0,.75 .37 0 0
P7:.15 .8 .75 0,0 .45 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.43 .11 0 0,.75 .37 0 0
P8:.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0
P9:.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0
P10:.35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0,.85 .7 0 0
P11:.35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .35 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0,.85 .7 0 0
* {smcl}
* {title:RdYlGn}{asis}
n:RdYlGn
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:252 141 89,255 255 191,145 207 96
P4:215 25 28,253 174 97,166 217 106,26 150 65
P5:215 25 28,253 174 97,255 255 191,166 217 106,26 150 65
P6:215 48 39,252 141 89,254 224 139,217 239 139,145 207 96,26 152 80
P7:215 48 39,252 141 89,254 224 139,255 255 191,217 239 139,145 207 96,26 152 80
P8:215 48 39,244 109 67,253 174 97,254 224 139,217 239 139,166 217 106,102 189 99,26 152 80
P9:215 48 39,244 109 67,253 174 97,254 224 139,255 255 191,217 239 139,166 217 106,102 189 99,26 152 80
P10:165 0 38,215 48 39,244 109 67,253 174 97,254 224 139,217 239 139,166 217 106,102 189 99,26 152 80,0 104 55
P11:165 0 38,215 48 39,244 109 67,253 174 97,254 224 139,255 255 191,217 239 139,166 217 106,102 189 99,26 152 80,0 104 55
n:RdYlGn cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .45 .55 0,0 0 .25 0,.43 0 .65 0
P4:.15 .9 .8 0,0 .32 .55 0,.35 0 .6 0,.9 0 .9 0
P5:.15 .9 .8 0,0 .35 .55 0,0 0 .25 0,.35 0 .6 0,.9 0 .9 0
P6:.15 .8 .75 0,0 .45 .55 0,0 .12 .42 0,.15 0 .45 0,.43 0 .65 0,.9 0 .9 0
P7:.15 .8 .75 0,0 .45 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.43 0 .65 0,.9 0 .8 0
P8:.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0
P9:.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0
P10:.35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0,1 .25 .9 0
P11:.35 1 .75 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0,1 .25 .9 0
* {smcl}
* {title:Spectral}{asis}
n:Spectral
c:diverging
d:diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_all_schemes_RGBonly3.XLS
P3:252 141 89,255 255 191,153 213 148
P4:215 25 28,253 174 97,171 221 164,43 131 186
P5:215 25 28,253 174 97,255 255 191,171 221 164,43 131 186
P6:213 62 79,252 141 89,254 224 139,230 245 152,153 213 148,50 136 189
P7:213 62 79,252 141 89,254 224 139,255 255 191,230 245 152,153 213 148,50 136 189
P8:213 62 79,244 109 67,253 174 97,254 224 139,230 245 152,171 221 164,102 194 165,50 136 189
P9:213 62 79,244 109 67,253 174 97,254 224 139,255 255 191,230 245 152,171 221 164,102 194 165,50 136 189
P10:158 1 66,213 62 79,244 109 67,253 174 97,254 224 139,230 245 152,171 221 164,102 194 165,50 136 189,94 79 162
P11:158 1 66,213 62 79,244 109 67,253 174 97,254 224 139,255 255 191,230 245 152,171 221 164,102 194 165,50 136 189,94 79 162
n:Spectral cmyk
c:diverging
d:CMYK variant of diverging colors from colorbrewer2.org (Brewer et al. 2003); licensed under Apache License Version 2.0 (see http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html)
s:https://github.com/axismaps/colorbrewer/blob/master/cb.csv
P3:0 .45 .55 0,0 0 .25 0,.4 0 .4 0
P4:.15 .9 .8 0,0 .32 .55 0,.33 0 .33 0,.85 .25 0 0
P5:.15 .9 .8 0,0 .32 .55 0,0 0 .25 0,.33 0 .33 0,.85 .25 0 0
P6:.15 .75 .5 0,0 .45 .55 0,0 .12 .42 0,.1 0 .4 0,.4 0 .4 0,.82 .23 0 0
P7:.15 .75 .5 0,0 .45 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.4 0 .4 0,.82 .23 0 0
P8:.15 .75 .5 0,.03 .57 .53 0,0 .32 .55 0,0 .12 .42 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0
P9:.15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0
P10:0 1 .2 .35,.15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0,.65 .6 0 0
P11:0 1 .2 .35,.15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0,.65 .6 0 0
* {smcl}
* {title:ptol qualitative}{asis}
n:ptol qualitative
c:qualitative
d:qualitative colors by Tol (2012)
s:https://personal.sron.nl/~pault/colourschemes.pdf
P1:68 119 170
P2:68 119 170,204 102 119
P3:68 119 170,221 204 119,204 102 119
P4:68 119 170,17 119 51,221 204 119,204 102 119
P5:51 34 136,136 204 238,17 119 51,221 204 119,204 102 119
P6:51 34 136,136 204 238,17 119 51,221 204 119,204 102 119,170 68 153
P7:51 34 136,136 204 238,68 170 153,17 119 51,221 204 119,204 102 119,170 68 153
P8:51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,204 102 119,170 68 153
P9:51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,204 102 119,136 34 85,170 68 153
P10:51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,136 34 85,170 68 153
P11:51 34 136,102 153 204,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,136 34 85,170 68 153
P12:51 34 136,102 153 204,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,170 68 102,136 34 85,170 68 153
* {smcl}
* {title:ptol rainbow}{asis}
n:ptol rainbow
c:sequential
d:rainbow colors by Tol (2012)
s:https://personal.sron.nl/~pault/colourschemes.pdf
P4:64 64 150,87 163 173,222 167 58,217 33 32
P5:64 64 150,82 157 183,125 184 116,227 156 55,217 33 32
P6:64 64 150,73 140 194,99 173 153,190 188 72,230 139 51,217 33 32
P7:120 28 129,63 96 174,83 158 182,109 179 136,202 184 67,231 133 50,217 33 32
P8:120 28 129,63 86 167,75 145 192,95 170 159,145 189 97,216 175 61,231 124 48,217 33 32
P9:120 28 129,63 78 161,70 131 193,87 163 173,109 179 136,177 190 78,223 165 58,231 116 47,217 33 32
P10:120 28 129,63 71 155,66 119 189,82 157 183,98 172 155,134 187 106,199 185 68,227 156 55,231 109 46,217 33 32
P11:120 28 129,64 64 150,65 108 183,77 149 190,91 167 167,110 179 135,161 190 86,211 179 63,229 148 53,230 104 45,217 33 32
P12:120 28 129,65 59 147,64 101 177,72 139 194,85 161 177,99 173 153,127 185 114,181 189 76,217 173 60,230 142 52,230 100 44,217 33 32
* {smcl}
* {title:ptol diverging}{asis}
n:ptol diverging
c:diverging
d:diverging colors by Tol (2012)
s:https://personal.sron.nl/~pault/colourschemes.pdf
P3:153 199 236,255 250 210,245 162 117
P4:0 139 206,180 221 247,249 189 126,208 50 50
P5:0 139 206,180 221 247,255 250 210,249 189 126,208 50 50
P6:58 137 201,153 199 236,230 245 254,255 227 170,245 162 117,210 77 62
P7:58 137 201,153 199 236,230 245 254,255 250 210,255 227 170,245 162 117,210 77 62
P8:58 137 201,119 183 229,180 221 247,230 245 254,255 227 170,249 189 126,237 135 94,210 77 62
P9:58 137 201,119 183 229,180 221 247,230 245 254,255 250 210,255 227 170,249 189 126,237 135 94,210 77 62
P10:61 82 161,58 137 201,119 183 229,180 221 247,230 245 254,255 227 170,249 189 126,237 135 94,210 77 62,174 28 62
P11:61 82 161,58 137 201,119 183 229,180 221 247,230 245 254,255 250 210,255 227 170,249 189 126,237 135 94,210 77 62,174 28 62
* {smcl}
* {title:d3 10}{asis}
n:d3 10
c:qualitative
d:categorical colors from d3js.org
s:https://github.com/d3/d3-scale
P:#1f77b4,#ff7f0e,#2ca02c,#d62728,#9467bd,#8c564b,#e377c2,#7f7f7f,#bcbd22,#17becf
* {smcl}
* {title:d3 20}{asis}
n:d3 20
c:qualitative
d:categorical colors from d3js.org
s:https://github.com/d3/d3-scale
P:#1f77b4,#aec7e8,#ff7f0e,#ffbb78,#2ca02c,#98df8a,#d62728,#ff9896,#9467bd,#c5b0d5,#8c564b,#c49c94,#e377c2,#f7b6d2,#7f7f7f,#c7c7c7,#bcbd22,#dbdb8d,#17becf,#9edae5
* {smcl}
* {title:d3 20b}{asis}
n:d3 20b
c:qualitative
d:categorical colors from d3js.org
s:https://github.com/d3/d3-scale
P:#393b79,#5254a3,#6b6ecf,#9c9ede,#637939,#8ca252,#b5cf6b,#cedb9c,#8c6d31,#bd9e39,#e7ba52,#e7cb94,#843c39,#ad494a,#d6616b,#e7969c,#7b4173,#a55194,#ce6dbd,#de9ed6
* {smcl}
* {title:d3 20c}{asis}
n:d3 20c
c:qualitative
d:categorical colors from d3js.org
s:https://github.com/d3/d3-scale
P:#3182bd,#6baed6,#9ecae1,#c6dbef,#e6550d,#fd8d3c,#fdae6b,#fdd0a2,#31a354,#74c476,#a1d99b,#c7e9c0,#756bb1,#9e9ac8,#bcbddc,#dadaeb,#636363,#969696,#bdbdbd,#d9d9d9
* {smcl}
* {title:lin carcolor}{asis}
n:lin carcolor
c:qualitative
d:Turkers-selected car colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:214 39 40,199 199 199,127 127 127,44 160 44,140 86 75,31 119 180
N:Red,Silver,Black,Green,Brown,Blue
* {smcl}
* {title:lin carcolor algorithm}{asis}
n:lin carcolor algorithm
c:qualitative
d:algorithm-selected car colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:214 39 40,199 199 199,127 127 127,44 160 44,140 86 75,31 119 180
N:Red,Silver,Black,Green,Brown,Blue
* {smcl}
* {title:lin food}{asis}
n:lin food
c:qualitative
d:Turkers-selected food colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:199 199 199,31 119 180,140 86 75,152 223 138,219 219 141,196 156 148,214 39 40
N:Sour cream,Blue cheese dressing,Porterhouse steak,Iceberg lettuce,Onions (raw),Potato (baked),Tomato
* {smcl}
* {title:lin food algorithm}{asis}
n:lin food algorithm
c:qualitative
d:algorithm-selected food colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:31 119 180,255 127 14,140 86 75,44 160 44,255 187 120,219 219 141,214 39 40
N:Sour cream,Blue cheese dressing,Porterhouse steak,Iceberg lettuce,Onions (raw),Potato (baked),Tomato
* {smcl}
* {title:lin features}{asis}
n:lin features
c:qualitative
d:Turkers-selected feature colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:214 39 40,31 119 180,174 119 232,44 160 44,152 223 138
N:Speed,Reliability,Comfort,Safety,Efficiency
* {smcl}
* {title:lin features algorithm}{asis}
n:lin features algorithm
c:qualitative
d:algorithm-selected feature colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:214 39 40,31 119 180,140 86 75,255 127 14,44 160 44
N:Speed,Reliability,Comfort,Safety,Efficiency
* {smcl}
* {title:lin activities}{asis}
n:lin activities
c:qualitative
d:Turkers-selected activity colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:31 119 180,214 39 40,152 223 138,44 160 44,127 127 127
N:Sleeping,Working,Leisure,Eating,Driving
* {smcl}
* {title:lin activities algorithm}{asis}
n:lin activities algorithm
c:qualitative
d:algorithm-selected activity colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:140 86 75,255 127 14,31 119 180,227 119 194,214 39 40
N:Sleeping,Working,Leisure,Eating,Driving
* {smcl}
* {title:lin fruits}{asis}
n:lin fruits
c:qualitative
d:expert-selected fruit colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:146 195 51,251 222 6,64 105 166,200 0 0,127 34 147,251 162 127,255 86 29
N:Apple,Banana,Blueberry,Cherry,Grape,Peach,Tangerine
* {smcl}
* {title:lin fruits algorithm}{asis}
n:lin fruits algorithm
c:qualitative
d:algorithm-selected fruit colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:44 160 44,188 189 34,31 119 180,214 39 40,148 103 189,255 187 120,255 127 14
N:Apple,Banana,Blueberry,Cherry,Grape,Peach,Tangerine
* {smcl}
* {title:lin vegetables}{asis}
n:lin vegetables
c:qualitative
d:expert-selected vegetable colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:255 141 61,157 212 105,245 208 64,104 59 101,239 197 143,139 129 57,255 26 34
N:Carrot,Celery,Corn,Eggplant,Mushroom,Olive,Tomato
* {smcl}
* {title:lin vegetables algorithm}{asis}
n:lin vegetables algorithm
c:qualitative
d:algorithm-selected vegetable colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:255 127 14,44 160 44,188 189 34,148 103 189,140 86 75,152 223 138,214 39 40
N:Carrot,Celery,Corn,Eggplant,Mushroom,Olive,Tomato
* {smcl}
* {title:lin drinks}{asis}
n:lin drinks
c:qualitative
d:expert-selected drinks colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:119 67 6,254 0 0,151 37 63,1 106 171,1 159 76,254 115 20,104 105 169
N:A&W Root Beer,Coca-Cola,Dr. Pepper,Pepsi,Sprite,Sunkist,Welch's Grape
* {smcl}
* {title:lin drinks algorithm}{asis}
n:lin drinks algorithm
c:qualitative
d:algorithm-selected drinks colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:140 86 75,214 39 40,227 119 194,31 119 180,44 160 44,255 127 14,148 103 189
N:A&W Root Beer,Coca-Cola,Dr. Pepper,Pepsi,Sprite,Sunkist,Welch's Grape
* {smcl}
* {title:lin brands}{asis}
n:lin brands
c:qualitative
d:expert-selected brands colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:161 165 169,44 163 218,242 99 33,255 183 0,0 112 66,204 0 0,123 0 153
N:Apple,AT&T,Home Depot,Kodak,Starbucks,Target,Yahoo!
* {smcl}
* {title:lin brands algorithm}{asis}
n:lin brands algorithm
c:qualitative
d:algorithm-selected brands colors by Lin et al. (2013)
s:brewextra.ado from https://ideas.repec.org/c/boc/bocode/s458050.html
P:152 223 138,31 119 180,255 127 14,140 86 75,44 160 44,214 39 40,148 103 189
N:Apple,AT&T,Home Depot,Kodak,Starbucks,Target,Yahoo!
* {smcl}
* {title:spmap heat}{asis}
n:spmap heat
c:sequential
d:heat color scheme from the spmap package by Pisati (2007)
s:spmap_color.ado from https://ideas.repec.org/c/boc/bocode/s456812.html
P2:255 255 0,255 0 0
P3:255 255 0,255 128 0,255 0 0
P4:255 255 128,255 255 0,255 128 0,255 0 0
P5:255 255 128,255 255 0,255 170 0,255 85 0,255 0 0
P6:255 255 128,255 255 0,255 191 0,255 128 0,255 64 0,255 0 0
P7:255 255 128,255 255 0,255 204 0,255 153 0,255 102 0,255 51 0,255 0 0
P8:255 255 191,255 255 64,255 255 0,255 204 0,255 153 0,255 102 0,255 51 0,255 0 0
P9:255 255 191,255 255 64,255 255 0,255 213 0,255 170 0,255 128 0,255 85 0,255 42 0,255 0 0
P10:255 255 191,255 255 64,255 255 0,255 219 0,255 182 0,255 146 0,255 109 0,255 73 0,255 36 0,255 0 0
P11:255 255 191,255 255 64,255 255 0,255 223 0,255 191 0,255 159 0,255 128 0,255 96 0,255 64 0,255 32 0,255 0 0
P12:255 255 213,255 255 128,255 255 42,255 255 0,255 223 0,255 191 0,255 159 0,255 128 0,255 96 0,255 64 0,255 32 0,255 0 0
P13:255 255 213,255 255 128,255 255 42,255 255 0,255 227 0,255 198 0,255 170 0,255 142 0,255 113 0,255 85 0,255 57 0,255 28 0,255 0 0
P14:255 255 213,255 255 128,255 255 42,255 255 0,255 229 0,255 204 0,255 178 0,255 153 0,255 128 0,255 102 0,255 77 0,255 51 0,255 26 0,255 0 0
P15:255 255 213,255 255 128,255 255 42,255 255 0,255 232 0,255 209 0,255 185 0,255 162 0,255 139 0,255 116 0,255 93 0,255 70 0,255 46 0,255 23 0,255 0 0
P16:255 255 223,255 255 159,255 255 96,255 255 32,255 255 0,255 232 0,255 209 0,255 185 0,255 162 0,255 139 0,255 116 0,255 93 0,255 70 0,255 46 0,255 23 0,255 0 0
* {smcl}
* {title:spmap terrain}{asis}
n:spmap terrain
c:sequential
d:terrain color scheme from the spmap package by Pisati (2007)
s:spmap_color.ado from https://ideas.repec.org/c/boc/bocode/s456812.html
P2:0 166 0,242 242 242
P3:0 166 0,236 177 118,242 242 242
P4:0 166 0,230 230 0,236 177 118,242 242 242
P5:0 166 0,230 230 0,234 182 78,238 185 159,242 242 242
P6:0 166 0,99 198 0,230 230 0,234 182 78,238 185 159,242 242 242
P7:0 166 0,99 198 0,230 230 0,233 189 58,236 177 118,239 194 179,242 242 242
P8:0 166 0,62 187 0,139 208 0,230 230 0,233 189 58,236 177 118,239 194 179,242 242 242
P9:0 166 0,62 187 0,139 208 0,230 230 0,232 195 46,235 178 94,237 180 142,240 201 192,242 242 242
P10:0 166 0,45 182 0,99 198 0,160 214 0,230 230 0,232 195 46,235 178 94,237 180 142,240 201 192,242 242 242
P11:0 166 0,45 182 0,99 198 0,160 214 0,230 230 0,232 199 39,234 182 78,236 177 118,238 185 159,240 207 200,242 242 242
P12:0 166 0,36 179 0,76 191 0,122 204 0,173 217 0,230 230 0,232 199 39,234 182 78,236 177 118,238 185 159,240 207 200,242 242 242
P13:0 166 0,36 179 0,76 191 0,122 204 0,173 217 0,230 230 0,231 203 33,233 186 67,235 177 101,237 179 135,239 190 170,240 211 206,242 242 242
P14:0 166 0,29 176 0,62 187 0,99 198 0,139 208 0,182 219 0,230 230 0,231 203 33,233 186 67,235 177 101,237 179 135,239 190 170,240 211 206,242 242 242
P15:0 166 0,29 176 0,62 187 0,99 198 0,139 208 0,182 219 0,230 230 0,231 206 29,233 189 58,234 179 88,236 177 118,237 182 148,239 194 179,241 214 211,242 242 242
P16:0 166 0,25 175 0,53 184 0,83 193 0,116 202 0,151 211 0,189 220 0,230 230 0,231 206 29,233 189 58,234 179 88,236 177 118,237 182 148,239 194 179,241 214 211,242 242 242
* {smcl}
* {title:spmap topological}{asis}
n:spmap topological
c:sequential
d:topological color scheme from the spmap package by Pisati (2007)
s:spmap_color.ado from https://ideas.repec.org/c/boc/bocode/s456812.html
P2:76 0 255,0 229 255
P3:76 0 255,0 255 77,255 255 0
P4:76 0 255,0 229 255,0 255 77,255 255 0
P5:76 0 255,0 76 255,0 229 255,0 255 77,255 255 0
P6:76 0 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
P7:76 0 255,0 76 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
P8:76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
P9:76 0 255,0 76 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
P10:76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
P11:76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
P12:76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
P13:76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
P14:76 0 255,15 0 255,0 46 255,0 107 255,0 168 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
P15:76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,0 255 0,77 255 0,153 255 0,230 255 0,255 255 0,255 234 45,255 222 89,255 219 134,255 224 178
P16:76 0 255,15 0 255,0 46 255,0 107 255,0 168 255,0 229 255,0 255 77,0 255 0,77 255 0,153 255 0,230 255 0,255 255 0,255 234 45,255 222 89,255 219 134,255 224 178
* {smcl}
* {title:sfso blue}{asis}
n:sfso blue
c:sequential
d:dark blue to light blue color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#1c3259,#374a83,#6473aa,#8497cf,#afbce2,#d8def2,#e8eaf7
N:,,,BFS-Blau,,,BFS-Blau 20%
n:sfso blue cmyk
c:sequential
d:CMYK variant of dark blue to light blue color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.83 .45 0 .7,.85 .55 0 .4,.7 .45 0 .2,.63 .36 0 0,.43 .22 0 0,.22 .1 0 0,.13 .07 0 0
N:,,,BFS-Blau,,,BFS-Blau 20%
* {smcl}
* {title:sfso brown}{asis}
n:sfso brown
c:sequential
d:dark brown to light brown color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#6b0616,#a1534e,#b67d6c,#cca58f,#ddc3a8,#eee3cd
n:sfso brown cmyk
c:sequential
d:CMYK variant of dark brown to light brown color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:0 1 .7 .6,0 .74 .57 .32,0 .56 .5 .24,0 .4 .4 .16,0 .27 .35 .1,0 .12 .22 .05
* {smcl}
* {title:sfso orange}{asis}
n:sfso orange
c:sequential
d:dark orange to light orange color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#92490d,#ce6725,#d68c25,#e2b224,#eccf76,#f6e7be
n:sfso orange cmyk
c:sequential
d:CMYK variant of dark orange to light orange color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:0 .75 1 .4,0 .75 1 0,0 .59 1 0,0 .4 1 0,0 .26 .68 0,0 .13 .35 0
* {smcl}
* {title:sfso red}{asis}
n:sfso red
c:sequential
d:dark red to light red color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#6d0724,#a61346,#c62a4f,#d17477,#dea49f,#efd6d1
n:sfso red cmyk
c:sequential
d:CMYK variant of dark red to light red color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.1 1 .6 .55,.1 1 .6 .15,0 .95 .64 0,0 .71 .48 0,0 .5 .34 0,0 .25 .16 0
* {smcl}
* {title:sfso pink}{asis}
n:sfso pink
c:sequential
d:dark pink to light pink color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#7c0051,#a4006f,#c0007c,#cc669d,#da9dbf,#efd7e5
n:sfso pink cmyk
c:sequential
d:CMYK variant of dark pink to light pink color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.12 1 .12 .45,.09 1 .09 .18,0 1 .09 .04,0 .75 .07 .03,0 .53 .04 .02,0 .25 .02 0
* {smcl}
* {title:sfso purple}{asis}
n:sfso purple
c:sequential
d:dark purple to light purple color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#5e0059,#890883,#a23392,#bf64a6,#d79dc5,#efd7e8
n:sfso purple cmyk
c:sequential
d:CMYK variant of dark purple to light purple color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.45 1 0 .45,.45 1 0 .05,.32 .9 0 0,.15 .75 0 0,.05 .53 0 0,0 .25 0 0
* {smcl}
* {title:sfso violet}{asis}
n:sfso violet
c:sequential
d:dark violet to light violet color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#3a0054,#682b86,#8c58a3,#a886bc,#c5b0d5,#e1d7eb
n:sfso violet cmyk
c:sequential
d:CMYK variant of dark violet to light violet color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.75 1 0 .5,.65 .9 0 .12,.51 .75 0 0,.38 .56 0 0,.25 .38 0 0,.12 .2 0 0
* {smcl}
* {title:sfso ltblue}{asis}
n:sfso ltblue
c:sequential
d:lighter version of blue color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#076e8d,#1b9dc9,#76b8da,#abd0e7,#c8e0f2,#edf5fd
n:sfso ltblue cmyk
c:sequential
d:CMYK variant of lighter version of blue color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.98 0 .14 .45,.98 0 .14 .05,.72 0 .1 .03,.49 0 .07 .02,.35 0 .04 0,.12 0 0 0
* {smcl}
* {title:sfso turquoise}{asis}
n:sfso turquoise
c:sequential
d:dark turquoise to light turquoise color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#005046,#107a6d,#3aa59a,#95c6c3,#cbe1df,#e9f2f5
n:sfso turquoise cmyk
c:sequential
d:CMYK variant of dark turquoise to light turquoise color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:1 0 .55 .65,1 0 .55 .35,.94 0 .5 0,.6 0 .3 0,.33 0 .17 0,.15 0 .05 0
* {smcl}
* {title:sfso green}{asis}
n:sfso green
c:sequential
d:dark green to light green color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#3b6519,#68a239,#95c15b,#b3d17f,#d3e3af,#ecf2d1
n:sfso green cmyk
c:sequential
d:CMYK variant of dark green to light green color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.75 0 1 .6,.75 0 1 .15,.6 0 .85 0,.45 0 .68 0,.28 0 .45 0,.12 0 .28 0
* {smcl}
* {title:sfso olive}{asis}
n:sfso olive
c:sequential
d:dark olive to light olive color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#6f6f02,#a3a20a,#c5c00c,#e3df86,#eeecbc,#fefde6
n:sfso olive cmyk
c:sequential
d:CMYK variant of dark olive to light olive color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.05 0 1 .7,.05 0 1 .45,0 0 1 .3,0 0 .6 .15,0 0 .35 .09,0 0 .17 0
* {smcl}
* {title:sfso black}{asis}
n:sfso black
c:sequential
d:dark gray to light gray color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#3f3f3e,#838382,#b2b3b3,#d4d5d5,#e6e6e7,#f7f7f7
n:sfso black cmyk
c:sequential
d:CMYK variant of dark gray to light gray color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:0 0 0 .9,0 0 0 .65,0 0 0 .43,0 0 0 .25,0 0 0 .15,0 0 0 .05
* {smcl}
* {title:sfso parties}{asis}
n:sfso parties
c:qualitative
d:Swiss parties color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#6268af,#f39f5e,#ea546f,#547d34,#cbd401,#ffff00,#26b300,#792a8f,#9fabd9,#f0da9d,#bebebe
N:FDP,CVP,SP,SVP,GLP,BDP,Grüne,small leftwing parties,small middle parties,small rightwing parties,other parties
n:sfso parties cmyk
c:qualitative
d:CMYK variant of Swiss parties color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.76 .6 .02 0,0 .57 .78 0,0 .85 .58 0,.8 .3 1 .2,.28 .01 .96 0,.01 0 .96 0,.72 0 1 0,.6 .92 0 0,.5 .29 0 0,0 .2 .5 0,0 0 0 .35
N:FDP,CVP,SP,SVP,GLP,BDP,Grüne,small leftwing parties,small middle parties,small rightwing parties,other parties
* {smcl}
* {title:sfso languages}{asis}
n:sfso languages
c:qualitative
d:Swiss language region color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#c73e31,#4570ba,#4ca767,#ecce42,#7f5fa9
N:German,French,Italian,Rhaeto-Romanic,English
n:sfso languages cmyk
c:qualitative
d:CMYK variant of Swiss language region color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:0 .9 .9 0,.9 .5 0 0,.9 0 .8 0,0 .25 .9 0,.6 .7 0 0
N:German,French,Italian,Rhaeto-Romanic,English
* {smcl}
* {title:sfso votes}{asis}
n:sfso votes
c:diverging
d:vote share color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:#6d2a83,#6d2a83*.8,#6d2a83*.6,#6d2a83*.4,#6d2a83*.2,#45974d*.2,#45974d*.4,#45974d*.6,#45974d*.8,#45974d
N:No,,,,,,,,,Yes
n:sfso votes cmyk
c:diverging
d:CMYK variant of vote share color scheme by the Swiss Federal Statistical Office
s:Bundesamt für Statistik 2017. Layoutrichtlinien. Gestaltungs und Redaktionsrichtlinien für Publikationen, Tabellen und grafische Assets. Version 1.1.1. Neuchâtel
P:.6 .9 0 .15,.6 .9 0 .15*.8,.6 .9 0 .15*.6,.6 .9 0 .15*.4,.6 .9 0 .15*.2,.9 0 .9 .15*.2,.9 0 .9 .15*.4,.9 0 .9 .15*.6,.9 0 .9 .15*.8,.9 0 .9 .15
N:No,,,,,,,,,Yes
* {smcl}
* {title:w3 amber}{asis}
n:w3 amber
c:sequential
d:Color Theme Amber from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-amber.css
P:#fffbf0,#fff3cd,#ffe69b,#ffda69,#ffcd37,#ffc107,#eab000,#d09c00,#b68900,#9c7500,#826200
N:w3-amber-l5,w3-amber-l4,w3-amber-l3,w3-amber-l2,w3-amber-l1,w3-amber,w3-amber-d1,w3-amber-d2,w3-amber-d3,w3-amber-d4,w3-amber-d5
* {smcl}
* {title:w3 black}{asis}
n:w3 black
c:sequential
d:Color Theme Black from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-black.css
P:#f0f0f0,#cccccc,#999999,#666666,#333333,#000000,#000000,#000000,#000000,#000000,#000000
N:w3-black-l5,w3-black-l4,w3-black-l3,w3-black-l2,w3-black-l1,w3-black,w3-black-d1,w3-black-d2,w3-black-d3,w3-black-d4,w3-black-d5
* {smcl}
* {title:w3 blue}{asis}
n:w3 blue
c:sequential
d:Color Theme Blue from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-blue.css
P:#f2f9fe,#d2eafd,#a6d4fa,#79bff8,#4daaf6,#2196f3,#0c87eb,#0b78d1,#0a69b7,#085a9d,#074b83
N:w3-blue-l5,w3-blue-l4,w3-blue-l3,w3-blue-l2,w3-blue-l1,w3-blue,w3-blue-d1,w3-blue-d2,w3-blue-d3,w3-blue-d4,w3-blue-d5
* {smcl}
* {title:w3 blue-grey}{asis}
n:w3 blue-grey
c:sequential
d:Color Theme Blue Grey from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-blue-grey.css
P:#f5f7f8,#dfe5e8,#becbd2,#9eb1bb,#7d97a5,#607d8b,#57707d,#4d636f,#435761,#3a4b53,#303e45
N:w3-blue-grey-l5,w3-blue-grey-l4,w3-blue-grey-l3,w3-blue-grey-l2,w3-blue-grey-l1,w3-blue-grey,w3-blue-grey-d1,w3-blue-grey-d2,w3-blue-grey-d3,w3-blue-grey-d4,w3-blue-grey-d5
* {smcl}
* {title:w3 brown}{asis}
n:w3 brown
c:sequential
d:Color Theme Brown from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-brown.css
P:#f8f4f3,#e7dcd7,#d0b8b0,#b89588,#a07261,#795548,#6d4d41,#61443a,#553c33,#49332c,#3d2b24
N:w3-brown-l5,w3-brown-l4,w3-brown-l3,w3-brown-l2,w3-brown-l1,w3-brown,w3-brown-d1,w3-brown-d2,w3-brown-d3,w3-brown-d4,w3-brown-d5
* {smcl}
* {title:w3 cyan}{asis}
n:w3 cyan
c:sequential
d:Color Theme Cyan from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-cyan.css
P:#edfdff,#c4f8ff,#89f1ff,#4eeaff,#12e3ff,#00bcd4,#00aac1,#0097ab,#008496,#007281,#005f6b
N:w3-cyan-l5,w3-cyan-l4,w3-cyan-l3,w3-cyan-l2,w3-cyan-l1,w3-cyan,w3-cyan-d1,w3-cyan-d2,w3-cyan-d3,w3-cyan-d4,w3-cyan-d5
* {smcl}
* {title:w3 dark-grey}{asis}
n:w3 dark-grey
c:sequential
d:Color Theme Dark Grey from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-dark-grey.css
P:#f6f6f6,#dfdfdf,#c0c0c0,#a0a0a0,#818181,#616161,#575757,#4e4e4e,#444444,#3a3a3a,#303030
N:w3-dark-grey-l5,w3-dark-grey-l4,w3-dark-grey-l3,w3-dark-grey-l2,w3-dark-grey-l1,w3-dark-grey,w3-dark-grey-d1,w3-dark-grey-d2,w3-dark-grey-d3,w3-dark-grey-d4,w3-dark-grey-d5
* {smcl}
* {title:w3 deep-orange}{asis}
n:w3 deep-orange
c:sequential
d:Color Theme Deep Orange from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-deep-orange.css
P:#fff5f2,#ffddd3,#ffbca7,#ff9a7b,#ff7850,#ff5722,#ff4107,#e93600,#cb2f00,#ae2900,#912200
N:w3-deep-orange-l5,w3-deep-orange-l4,w3-deep-orange-l3,w3-deep-orange-l2,w3-deep-orange-l1,w3-deep-orange,w3-deep-orange-d1,w3-deep-orange-d2,w3-deep-orange-d3,w3-deep-orange-d4,w3-deep-orange-d5
* {smcl}
* {title:w3 deep-purple}{asis}
n:w3 deep-purple
c:sequential
d:Color Theme Deep Purple from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-deep-purple.css
P:#f6f3fb,#e0d6f2,#c1ade5,#a384d8,#845bcb,#673ab7,#5d34a4,#532e92,#482880,#3e236d,#341d5b
N:w3-deep-purple-l5,w3-deep-purple-l4,w3-deep-purple-l3,w3-deep-purple-l2,w3-deep-purple-l1,w3-deep-purple,w3-deep-purple-d1,w3-deep-purple-d2,w3-deep-purple-d3,w3-deep-purple-d4,w3-deep-purple-d5
* {smcl}
* {title:w3 green}{asis}
n:w3 green
c:sequential
d:Color Theme Green from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-green.css
P:#f4faf4,#dbefdc,#b7dfb8,#93cf95,#6ec071,#4caf50,#459c48,#3d8b40,#357a38,#2e6830,#265728
N:w3-green-l5,w3-green-l4,w3-green-l3,w3-green-l2,w3-green-l1,w3-green,w3-green-d1,w3-green-d2,w3-green-d3,w3-green-d4,w3-green-d5
* {smcl}
* {title:w3 grey}{asis}
n:w3 grey
c:sequential
d:Color Theme Grey from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-grey.css
P:#f9f9f9,#ececec,#d8d8d8,#c5c5c5,#b1b1b1,#9e9e9e,#8e8e8e,#7e7e7e,#6f6f6f,#5f5f5f,#4f4f4f
N:w3-grey-l5,w3-grey-l4,w3-grey-l3,w3-grey-l2,w3-grey-l1,w3-grey,w3-grey-d1,w3-grey-d2,w3-grey-d3,w3-grey-d4,w3-grey-d5
* {smcl}
* {title:w3 indigo}{asis}
n:w3 indigo
c:sequential
d:Color Theme Indigo from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-indigo.css
P:#f3f4fb,#d8dcf1,#b1b8e3,#8995d6,#6271c8,#3f51b5,#3949a3,#334191,#2d397f,#26316d,#20295b
N:w3-indigo-l5,w3-indigo-l4,w3-indigo-l3,w3-indigo-l2,w3-indigo-l1,w3-indigo,w3-indigo-d1,w3-indigo-d2,w3-indigo-d3,w3-indigo-d4,w3-indigo-d5
* {smcl}
* {title:w3 khaki}{asis}
n:w3 khaki
c:sequential
d:Color Theme Khaki from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-khaki.css
P:#fefef8,#fcfae8,#f9f5d2,#f6f0bb,#f3eba5,#f0e68c,#ecdf6c,#e8d84a,#e3d029,#cbb91a,#a99b16
N:w3-khaki-l5,w3-khaki-l4,w3-khaki-l3,w3-khaki-l2,w3-khaki-l1,w3-khaki,w3-khaki-d1,w3-khaki-d2,w3-khaki-d3,w3-khaki-d4,w3-khaki-d5
* {smcl}
* {title:w3 light-blue}{asis}
n:w3 light-blue
c:sequential
d:Color Theme Light Blue from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-light-blue.css
P:#f8fcfe,#e7f5fb,#d0ecf7,#b8e2f3,#a1d9ef,#87ceeb,#69c2e6,#4ab6e0,#2aa9db,#2092bf,#1b7a9f
N:w3-light-blue-l5,w3-light-blue-l4,w3-light-blue-l3,w3-light-blue-l2,w3-light-blue-l1,w3-light-blue,w3-light-blue-d1,w3-light-blue-d2,w3-light-blue-d3,w3-light-blue-d4,w3-light-blue-d5
* {smcl}
* {title:w3 light-green}{asis}
n:w3 light-green
c:sequential
d:Color Theme Light Green from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-light-green.css
P:#f8fbf4,#e8f3db,#d1e7b7,#b9db93,#a2cf6f,#8bc34a,#7eb63d,#70a236,#628e2f,#547a29,#466522
N:w3-light-green-l5,w3-light-green-l4,w3-light-green-l3,w3-light-green-l2,w3-light-green-l1,w3-light-green,w3-light-green-d1,w3-light-green-d2,w3-light-green-d3,w3-light-green-d4,w3-light-green-d5
* {smcl}
* {title:w3 lime}{asis}
n:w3 lime
c:sequential
d:Color Theme Lime from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-lime.css
P:#fcfdf3,#f5f8d7,#eaf1af,#e0ea87,#d6e35f,#cddc39,#c1d325,#acbb21,#96a41d,#818c19,#6b7515
N:w3-lime-l5,w3-lime-l4,w3-lime-l3,w3-lime-l2,w3-lime-l1,w3-lime,w3-lime-d1,w3-lime-d2,w3-lime-d3,w3-lime-d4,w3-lime-d5
* {smcl}
* {title:w3 orange}{asis}
n:w3 orange
c:sequential
d:Color Theme Orange from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-orange.css
P:#fff9f0,#ffebcc,#ffd699,#ffc266,#ffad33,#ff9800,#e68a00,#cc7a00,#b36b00,#995c00,#804d00
N:w3-orange-l5,w3-orange-l4,w3-orange-l3,w3-orange-l2,w3-orange-l1,w3-orange,w3-orange-d1,w3-orange-d2,w3-orange-d3,w3-orange-d4,w3-orange-d5
* {smcl}
* {title:w3 pink}{asis}
n:w3 pink
c:sequential
d:Color Theme Pink from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-pink.css
P:#fef2f6,#fbd2e0,#f6a6c1,#f279a1,#ed4d82,#e91e63,#d91557,#c1134d,#a91143,#910e3a,#790c30
N:w3-pink-l5,w3-pink-l4,w3-pink-l3,w3-pink-l2,w3-pink-l1,w3-pink,w3-pink-d1,w3-pink-d2,w3-pink-d3,w3-pink-d4,w3-pink-d5
* {smcl}
* {title:w3 purple}{asis}
n:w3 purple
c:sequential
d:Color Theme Purple from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-purple.css
P:#faf0fc,#efcef4,#de9eea,#ce6ddf,#be3dd4,#9c27b0,#8c239e,#7c1f8d,#6d1b7b,#5d1769,#4e1358
N:w3-purple-l5,w3-purple-l4,w3-purple-l3,w3-purple-l2,w3-purple-l1,w3-purple,w3-purple-d1,w3-purple-d2,w3-purple-d3,w3-purple-d4,w3-purple-d5
* {smcl}
* {title:w3 red}{asis}
n:w3 red
c:sequential
d:Color Theme Red from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-red.css
P:#fef4f3,#fdd9d6,#fbb3ae,#f98d85,#f6665c,#f44336,#f32617,#e11a0c,#c5170a,#a91409,#8d1007
N:w3-red-l5,w3-red-l4,w3-red-l3,w3-red-l2,w3-red-l1,w3-red,w3-red-d1,w3-red-d2,w3-red-d3,w3-red-d4,w3-red-d5
* {smcl}
* {title:w3 teal}{asis}
n:w3 teal
c:sequential
d:Color Theme Teal from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-teal.css
P:#e9fffd,#b7fff8,#6efff1,#26ffe9,#00dcc6,#009688,#008578,#00766a,#00685d,#005950,#004a43
N:w3-teal-l5,w3-teal-l4,w3-teal-l3,w3-teal-l2,w3-teal-l1,w3-teal,w3-teal-d1,w3-teal-d2,w3-teal-d3,w3-teal-d4,w3-teal-d5
* {smcl}
* {title:w3 yellow}{asis}
n:w3 yellow
c:sequential
d:Color Theme Yellow from www.w3schools.com/w3css/w3css_color_themes.asp
s:https://www.w3schools.com/lib/w3-theme-yellow.css
P:#fffef3,#fffbd8,#fff7b1,#fff38b,#ffef64,#ffeb3b,#ffe81e,#fde400,#ddc700,#beab00,#9e8e00
N:w3-yellow-l5,w3-yellow-l4,w3-yellow-l3,w3-yellow-l2,w3-yellow-l1,w3-yellow,w3-yellow-d1,w3-yellow-d2,w3-yellow-d3,w3-yellow-d4,w3-yellow-d5
* {smcl}
* {title:BottleRocket1}{asis}
n:BottleRocket1
c:qualitative
d:Bottle Rocket (1996)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#A42820,#5F5647,#9B110E,#3F5151,#4E2A1E,#550307,#0C1707
* {smcl}
* {title:BottleRocket2}{asis}
n:BottleRocket2
c:qualitative
d:Bottle Rocket (1996)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#FAD510,#CB2314,#273046,#354823,#1E1E1E
* {smcl}
* {title:Rushmore1}{asis}
n:Rushmore1
c:qualitative
d:Rushmore (1998)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#E1BD6D,#EABE94,#0B775E,#35274A,#F2300F
* {smcl}
* {title:Royal1}{asis}
n:Royal1
c:qualitative
d:The Royal Tenenbaums (2001)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#899DA4,#C93312,#FAEFD1,#DC863B
* {smcl}
* {title:Royal2}{asis}
n:Royal2
c:qualitative
d:The Royal Tenenbaums (2001)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#9A8822,#F5CDB4,#F8AFA8,#FDDDA0,#74A089
* {smcl}
* {title:Zissou1}{asis}
n:Zissou1
c:diverging
d:The Life Aquatic with Steve Zissou (2004)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#3B9AB2,#78B7C5,#EBCC2A,#E1AF00,#F21A00
* {smcl}
* {title:Darjeeling1}{asis}
n:Darjeeling1
c:qualitative
d:The Darjeeling Limited (2007)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#FF0000,#00A08A,#F2AD00,#F98400,#5BBCD6
* {smcl}
* {title:Darjeeling2}{asis}
n:Darjeeling2
c:qualitative
d:The Darjeeling Limited (2007)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#ECCBAE,#046C9A,#D69C4E,#ABDDDE,#000000
* {smcl}
* {title:Chevalier1}{asis}
n:Chevalier1
c:qualitative
d:Hotel Chevalier (2007)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#446455,#FDD262,#D3DDDC,#C7B19C
* {smcl}
* {title:FantasticFox1}{asis}
n:FantasticFox1
c:qualitative
d:Fantastic Mr. Fox (2009)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#DD8D29,#E2D200,#46ACC8,#E58601,#B40F20
* {smcl}
* {title:Moonrise1}{asis}
n:Moonrise1
c:qualitative
d:Moonrise Kingdom (2012)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#F3DF6C,#CEAB07,#D5D5D3,#24281A
* {smcl}
* {title:Moonrise2}{asis}
n:Moonrise2
c:qualitative
d:Moonrise Kingdom (2012)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#798E87,#C27D38,#CCC591,#29211F
* {smcl}
* {title:Moonrise3}{asis}
n:Moonrise3
c:qualitative
d:Moonrise Kingdom (2012)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#85D4E3,#F4B5BD,#9C964A,#CDC08C,#FAD77B
* {smcl}
* {title:Cavalcanti1}{asis}
n:Cavalcanti1
c:qualitative
d:Castello Cavalcanti (2013)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#D8B70A,#02401B,#A2A475,#81A88D,#972D15
* {smcl}
* {title:GrandBudapest1}{asis}
n:GrandBudapest1
c:qualitative
d:The Grand Budapest Hotel (2014)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#F1BB7B,#FD6467,#5B1A18,#D67236
* {smcl}
* {title:GrandBudapest2}{asis}
n:GrandBudapest2
c:qualitative
d:The Grand Budapest Hotel (2014)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#E6A0C4,#C6CDF7,#D8A499,#7294D4
* {smcl}
* {title:IsleofDogs1}{asis}
n:IsleofDogs1
c:qualitative
d:The Isle of Dogs (2018)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#9986A5,#79402E,#CCBA72,#0F0D0E,#D9D0D3,#8D8680
* {smcl}
* {title:IsleofDogs2}{asis}
n:IsleofDogs2
c:qualitative
d:The Isle of Dogs (2018)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#EAD3BF,#AA9486,#B6854D,#39312F,#1C1718
* {smcl}
* {title:FrenchDispatch1}{asis}
n:FrenchDispatch1
c:qualitative
d:The French Dispatch (2021)
s:colors.R from https://github.com/karthik/wesanderson, retrieved 23feb2022
P:#90D4CC,#BD3027,#B0AFA2,#7FC0C6,#9D9C85
* {smcl}
* {title:sb deep}{asis}
n:sb deep
c:qualitative
d:deep palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#4C72B0,#DD8452,#55A868,#C44E52,#8172B3,#937860,#DA8BC3,#8C8C8C,#CCB974,#64B5CD
* {smcl}
* {title:sb deep6}{asis}
n:sb deep6
c:qualitative
d:deep6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#4C72B0,#55A868,#C44E52,#8172B3,#CCB974,#64B5CD
* {smcl}
* {title:sb muted}{asis}
n:sb muted
c:qualitative
d:muted palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#4878D0,#EE854A,#6ACC64,#D65F5F,#956CB4,#8C613C,#DC7EC0,#797979,#D5BB67,#82C6E2
* {smcl}
* {title:sb muted6}{asis}
n:sb muted6
c:qualitative
d:muted6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#4878D0,#6ACC64,#D65F5F,#956CB4,#D5BB67,#82C6E2
* {smcl}
* {title:sb pastel}{asis}
n:sb pastel
c:qualitative
d:pastel palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#A1C9F4,#FFB482,#8DE5A1,#FF9F9B,#D0BBFF,#DEBB9B,#FAB0E4,#CFCFCF,#FFFEA3,#B9F2F0
* {smcl}
* {title:sb pastel6}{asis}
n:sb pastel6
c:qualitative
d:pastel6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#A1C9F4,#8DE5A1,#FF9F9B,#D0BBFF,#FFFEA3,#B9F2F0
* {smcl}
* {title:sb bright}{asis}
n:sb bright
c:qualitative
d:bright palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#023EFF,#FF7C00,#1AC938,#E8000B,#8B2BE2,#9F4800,#F14CC1,#A3A3A3,#FFC400,#00D7FF
* {smcl}
* {title:sb bright6}{asis}
n:sb bright6
c:qualitative
d:bright6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#023EFF,#1AC938,#E8000B,#8B2BE2,#FFC400,#00D7FF
* {smcl}
* {title:sb dark}{asis}
n:sb dark
c:qualitative
d:dark palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#001C7F,#B1400D,#12711C,#8C0800,#591E71,#592F0D,#A23582,#3C3C3C,#B8850A,#006374
* {smcl}
* {title:sb dark6}{asis}
n:sb dark6
c:qualitative
d:dark6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#001C7F,#12711C,#8C0800,#591E71,#B8850A,#006374
* {smcl}
* {title:sb colorblind}{asis}
n:sb colorblind
c:qualitative
d:colorblind palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#0173B2,#DE8F05,#029E73,#D55E00,#CC78BC,#CA9161,#FBAFE4,#949494,#ECE133,#56B4E9
* {smcl}
* {title:sb colorblind6}{asis}
n:sb colorblind6
c:qualitative
d:colorblind6 palette from seaborn.pydata.org
s:https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py, retrieved 23feb2022
P:#0173B2,#029E73,#D55E00,#CC78BC,#ECE133,#56B4E9
* {smcl}
* {title:tol bright}{asis}
n:tol bright
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#4477AA,#EE6677,#228833,#CCBB44,#66CCEE,#AA3377,#BBBBBB,#000000
N:blue,red,green,yellow,cyan,purple,grey,black
* {smcl}
* {title:tol high-contrast}{asis}
n:tol high-contrast
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#004488,#DDAA33,#BB5566,#000000
N:blue,yellow,red,black
* {smcl}
* {title:tol vibrant}{asis}
n:tol vibrant
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#EE7733,#0077BB,#33BBEE,#EE3377,#CC3311,#009988,#BBBBBB,#000000
N:orange,blue,cyan,magenta,red,teal,grey,black
* {smcl}
* {title:tol muted}{asis}
n:tol muted
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#CC6677,#332288,#DDCC77,#117733,#88CCEE,#882255,#44AA99,#999933,#AA4499,#DDDDDD,#000000
N:rose,indigo,sand,green,cyan,wine,teal,olive,purple,pale_grey,black
* {smcl}
* {title:tol medium-contrast}{asis}
n:tol medium-contrast
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#6699CC,#004488,#EECC66,#994455,#997700,#EE99AA,#000000
N:light_blue,dark_blue,light_yellow,dark_red,dark_yellow,light_red,black
* {smcl}
* {title:tol light}{asis}
n:tol light
c:qualitative
d:qualitative palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#77AADD,#EE8866,#EEDD88,#FFAABB,#99DDFF,#44BB99,#BBCC33,#AAAA00,#DDDDDD,#000000
N:light_blue,orange,light_yellow,pink,light_cyan,mint,pear,olive,pale_grey,black
* {smcl}
* {title:tol sunset}{asis}
n:tol sunset
c:diverging
d:diverging palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#364B9A,#4A7BB7,#6EA6CD,#98CAE1,#C2E4EF,#EAECCC,#FEDA8B,#FDB366,#F67E4B,#DD3D2D,#A50026
* {smcl}
* {title:tol BuRd}{asis}
n:tol BuRd
c:diverging
d:diverging palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#2166AC,#4393C3,#92C5DE,#D1E5F0,#F7F7F7,#FDDBC7,#F4A582,#D6604D,#B2182B
* {smcl}
* {title:tol PRGn}{asis}
n:tol PRGn
c:diverging
d:diverging palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#762A83,#9970AB,#C2A5CF,#E7D4E8,#F7F7F7,#D9F0D3,#ACD39E,#5AAE61,#1B7837
* {smcl}
* {title:tol YlOrBr}{asis}
n:tol YlOrBr
c:sequential
d:sequential palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#FFFFE5,#FFF7BC,#FEE391,#FEC44F,#FB9A29,#EC7014,#CC4C02,#993404,#662506
* {smcl}
* {title:tol iridescent}{asis}
n:tol iridescent
c:sequential
d:sequential palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#FEFBE9,#FCF7D5,#F5F3C1,#EAF0B5,#DDECBF,#D0E7CA,#C2E3D2,#B5DDD8,#A8D8DC,#9BD2E1,#8DCBE4,#81C4E7,#7BBCE7,#7EB2E4,#88A5DD,#9398D2,#9B8AC4,#9D7DB2,#9A709E,#906388,#805770,#684957,#46353A
* {smcl}
* {title:tol PuRd}{asis}
n:tol PuRd
c:sequential
d:rainbow palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#6F4C9B,#6059A9,#5568B8,#4E79C5,#4D8AC6,#4E96BC,#549EB3,#59A5A9,#60AB9E,#69B190,#77B77D,#8CBC68,#A6BE54,#BEBC48,#D1B541,#DDAA3C,#E49C39,#E78C35,#E67932,#E4632D,#DF4828,#DA2222
* {smcl}
* {title:tol PuBr}{asis}
n:tol PuBr
c:sequential
d:rainbow palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#6F4C9B,#6059A9,#5568B8,#4E79C5,#4D8AC6,#4E96BC,#549EB3,#59A5A9,#60AB9E,#69B190,#77B77D,#8CBC68,#A6BE54,#BEBC48,#D1B541,#DDAA3C,#E49C39,#E78C35,#E67932,#E4632D,#DF4828,#DA2222,#B8221E,#95211B,#721E17,#521A13
* {smcl}
* {title:tol WhRd}{asis}
n:tol WhRd
c:sequential
d:rainbow palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#E8ECFB,#DDD8EF,#D1C1E1,#C3A8D1,#B58FC2,#A778B4,#9B62A7,#8C4E99,#6F4C9B,#6059A9,#5568B8,#4E79C5,#4D8AC6,#4E96BC,#549EB3,#59A5A9,#60AB9E,#69B190,#77B77D,#8CBC68,#A6BE54,#BEBC48,#D1B541,#DDAA3C,#E49C39,#E78C35,#E67932,#E4632D,#DF4828,#DA2222
* {smcl}
* {title:tol WhBr}{asis}
n:tol WhBr
c:sequential
d:rainbow palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P:#E8ECFB,#DDD8EF,#D1C1E1,#C3A8D1,#B58FC2,#A778B4,#9B62A7,#8C4E99,#6F4C9B,#6059A9,#5568B8,#4E79C5,#4D8AC6,#4E96BC,#549EB3,#59A5A9,#60AB9E,#69B190,#77B77D,#8CBC68,#A6BE54,#BEBC48,#D1B541,#DDAA3C,#E49C39,#E78C35,#E67932,#E4632D,#DF4828,#DA2222,#B8221E,#95211B,#721E17,#521A13
* {smcl}
* {title:tol rainbow}{asis}
n:tol rainbow
c:sequential
d:rainbow palette by Paul Tol (https://personal.sron.nl/~pault/)
s:https://personal.sron.nl/~pault/data/tol_colors.py, retrieved 30mar2022
P1:#1965B0
P2:#1965B0,#DC050C
P3:#1965B0,#F7F056,#DC050C
P4:#1965B0,#4EB265,#F7F056,#DC050C
P5:#1965B0,#7BAFDE,#4EB265,#F7F056,#DC050C
P6:#1965B0,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#DC050C
P7:#882E72,#1965B0,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#DC050C
P8:#882E72,#1965B0,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#EE8026,#DC050C
P9:#882E72,#1965B0,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#EE8026,#DC050C,#72190E
P10:#882E72,#1965B0,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#F4A736,#E8601C,#DC050C,#72190E
P11:#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#F4A736,#E8601C,#DC050C,#72190E
P12:#D1BBD7,#AE76A3,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#CAE0AB,#F7F056,#F4A736,#E8601C,#DC050C
P13:#D1BBD7,#AE76A3,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F4A736,#E8601C,#DC050C
P14:#D1BBD7,#AE76A3,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C
P15:#D1BBD7,#AE76A3,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#72190E
P16:#D1BBD7,#BA8DB4,#AA6F9E,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#72190E
P17:#D1BBD7,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#72190E
P18:#D1BBD7,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#A5170E,#72190E
P19:#D9CCE3,#CAACCB,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#5289C7,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#A5170E,#72190E
P20:#D9CCE3,#CAACCB,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#437DBF,#6195CF,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F6C141,#F1932D,#E8601C,#DC050C,#A5170E,#72190E
P21:#D9CCE3,#CAACCB,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#437DBF,#6195CF,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F7CB45,#F4A736,#EE8026,#E65518,#DC050C,#A5170E,#72190E
P22:#D9CCE3,#CAACCB,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#437DBF,#6195CF,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F7CB45,#F4A736,#EE8026,#E65518,#DC050C,#A5170E,#72190E,#42150A
P23:#E8ECFB,#D9CCE3,#CAACCB,#BA8DB4,#AA6F9E,#994F88,#882E72,#1965B0,#437DBF,#6195CF,#7BAFDE,#4EB265,#90C987,#CAE0AB,#F7F056,#F7CB45,#F4A736,#EE8026,#E65518,#DC050C,#A5170E,#72190E,#42150A
* {smcl}
* {title:carto Burg}{asis}
n:carto Burg
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#ffc6c4,#672044
P3:#ffc6c4,#cc607d,#672044
P4:#ffc6c4,#e38191,#ad466c,#672044
P5:#ffc6c4,#ee919b,#cc607d,#9e3963,#672044
P6:#ffc6c4,#f29ca3,#da7489,#b95073,#93345d,#672044
P7:#ffc6c4,#f4a3a8,#e38191,#cc607d,#ad466c,#8b3058,#672044
* {smcl}
* {title:carto BurgYl}{asis}
n:carto BurgYl
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#fbe6c5,#70284a
P3:#fbe6c5,#dc7176,#70284a
P4:#fbe6c5,#ee8a82,#c8586c,#70284a
P5:#fbe6c5,#f2a28a,#dc7176,#b24b65,#70284a
P6:#fbe6c5,#f4b191,#e7807d,#d06270,#a44360,#70284a
P7:#fbe6c5,#f5ba98,#ee8a82,#dc7176,#c8586c,#9c3f5d,#70284a
* {smcl}
* {title:carto RedOr}{asis}
n:carto RedOr
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f6d2a9,#b13f64
P3:#f6d2a9,#ea8171,#b13f64
P4:#f6d2a9,#f19c7c,#dd686c,#b13f64
P5:#f6d2a9,#f3aa84,#ea8171,#d55d6a,#b13f64
P6:#f6d2a9,#f4b28a,#ef9177,#e3726d,#cf5669,#b13f64
P7:#f6d2a9,#f5b78e,#f19c7c,#ea8171,#dd686c,#ca5268,#b13f64
* {smcl}
* {title:carto OrYel}{asis}
n:carto OrYel
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#ecda9a,#ee4d5a
P3:#ecda9a,#f7945d,#ee4d5a
P4:#ecda9a,#f3ad6a,#f97b57,#ee4d5a
P5:#ecda9a,#f1b973,#f7945d,#f86f56,#ee4d5a
P6:#ecda9a,#f0c079,#f5a363,#f98558,#f76856,#ee4d5a
P7:#ecda9a,#efc47e,#f3ad6a,#f7945d,#f97b57,#f66356,#ee4d5a
* {smcl}
* {title:carto Peach}{asis}
n:carto Peach
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#fde0c5,#eb4a40
P3:#fde0c5,#f59e72,#eb4a40
P4:#fde0c5,#f8b58b,#f2855d,#eb4a40
P5:#fde0c5,#f9c098,#f59e72,#f17854,#eb4a40
P6:#fde0c5,#fac7a1,#f7ac80,#f38f65,#f0704f,#eb4a40
P7:#fde0c5,#facba6,#f8b58b,#f59e72,#f2855d,#ef6a4c,#eb4a40
* {smcl}
* {title:carto PinkYl}{asis}
n:carto PinkYl
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#fef6b5,#e15383
P3:#fef6b5,#ffa679,#e15383
P4:#fef6b5,#ffc285,#fa8a76,#e15383
P5:#fef6b5,#ffd08e,#ffa679,#f67b77,#e15383
P6:#fef6b5,#ffd795,#ffb77f,#fd9576,#f37378,#e15383
P7:#fef6b5,#ffdd9a,#ffc285,#ffa679,#fa8a76,#f16d7a,#e15383
* {smcl}
* {title:carto Mint}{asis}
n:carto Mint
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#e4f1e1,#0d585f
P3:#e4f1e1,#63a6a0,#0d585f
P4:#e4f1e1,#89c0b6,#448c8a,#0d585f
P5:#E4F1E1,#9CCDC1,#63A6A0,#337F7F,#0D585F
P6:#e4f1e1,#abd4c7,#7ab5ad,#509693,#2c7778,#0d585f
P7:#e4f1e1,#b4d9cc,#89c0b6,#63a6a0,#448c8a,#287274,#0d585f
* {smcl}
* {title:carto BluGrn}{asis}
n:carto BluGrn
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#c4e6c3,#1d4f60
P3:#c4e6c3,#4da284,#1d4f60
P4:#c4e6c3,#6dbc90,#36877a,#1d4f60
P5:#c4e6c3,#80c799,#4da284,#2d7974,#1d4f60
P6:#c4e6c3,#8dce9f,#5fb28b,#3e927e,#297071,#1d4f60
P7:#c4e6c3,#96d2a4,#6dbc90,#4da284,#36877a,#266b6e,#1d4f60
* {smcl}
* {title:carto DarkMint}{asis}
n:carto DarkMint
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#d2fbd4,#123f5a
P3:#d2fbd4,#559c9e,#123f5a
P4:#d2fbd4,#7bbcb0,#3a7c89,#123f5a
P5:#d2fbd4,#8eccb9,#559c9e,#2b6c7f,#123f5a
P6:#d2fbd4,#9cd5be,#6cafa9,#458892,#266377,#123f5a
P7:#d2fbd4,#a5dbc2,#7bbcb0,#559c9e,#3a7c89,#235d72,#123f5a
* {smcl}
* {title:carto Emrld}{asis}
n:carto Emrld
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#d3f2a3,#074050
P3:#d3f2a3,#4c9b82,#074050
P4:#d3f2a3,#6cc08b,#217a79,#074050
P5:#d3f2a3,#82d091,#4c9b82,#19696f,#074050
P6:#d3f2a3,#8fda94,#60b187,#35877d,#145f69,#074050
P7:#d3f2a3,#97e196,#6cc08b,#4c9b82,#217a79,#105965,#074050
* {smcl}
* {title:carto ag_GrnYl}{asis}
n:carto ag_GrnYl
c:sequential
d:aggregation palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#245668,#EDEF5D
P3:#245668,#39AB7E,#EDEF5D
P4:#245668,#0D8F81,#6EC574,#EDEF5D
P5:#245668,#04817E,#39AB7E,#8BD16D,#EDEF5D
P6:#245668,#09787C,#1D9A81,#58BB79,#9DD869,#EDEF5D
P7:#245668,#0F7279,#0D8F81,#39AB7E,#6EC574,#A9DC67,#EDEF5D
* {smcl}
* {title:carto BluYl}{asis}
n:carto BluYl
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f7feae,#045275
P3:#f7feae,#46aea0,#045275
P4:#f7feae,#7ccba2,#089099,#045275
P5:#f7feae,#9bd8a4,#46aea0,#058092,#045275
P6:#f7feae,#ace1a4,#68bfa1,#2a9c9c,#02778e,#045275
P7:#f7feae,#b7e6a5,#7ccba2,#46aea0,#089099,#00718b,#045275
* {smcl}
* {title:carto Teal}{asis}
n:carto Teal
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#d1eeea,#2a5674
P3:#d1eeea,#68abb8,#2a5674
P4:#d1eeea,#85c4c9,#4f90a6,#2a5674
P5:#d1eeea,#96d0d1,#68abb8,#45829b,#2a5674
P6:#d1eeea,#a1d7d6,#79bbc3,#599bae,#3f7994,#2a5674
P7:#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674
* {smcl}
* {title:carto TealGrn}{asis}
n:carto TealGrn
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#b0f2bc,#257d98
P3:#b0f2bc,#4cc8a3,#257d98
P4:#b0f2bc,#67dba5,#38b2a3,#257d98
P5:#b0f2bc,#77e2a8,#4cc8a3,#31a6a2,#257d98
P6:#b0f2bc,#82e6aa,#5bd4a4,#3fbba3,#2e9ea1,#257d98
P7:#b0f2bc,#89e8ac,#67dba5,#4cc8a3,#38b2a3,#2c98a0,#257d98
* {smcl}
* {title:carto Purp}{asis}
n:carto Purp
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f3e0f7,#63589f
P3:#f3e0f7,#b998dd,#63589f
P4:#f3e0f7,#d1afe8,#9f82ce,#63589f
P5:#f3e0f7,#dbbaed,#b998dd,#9178c4,#63589f
P6:#f3e0f7,#e0c2ef,#c8a5e4,#aa8bd4,#8871be,#63589f
P7:#f3e0f7,#e4c7f1,#d1afe8,#b998dd,#9f82ce,#826dba,#63589f
* {smcl}
* {title:carto PurpOr}{asis}
n:carto PurpOr
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f9ddda,#573b88
P3:#f9ddda,#ce78b3,#573b88
P4:#f9ddda,#e597b9,#ad5fad,#573b88
P5:#f9ddda,#eda8bd,#ce78b3,#9955a8,#573b88
P6:#f9ddda,#f0b2c1,#dd8ab6,#bb69b0,#8c4fa4,#573b88
P7:#f9ddda,#f2b9c4,#e597b9,#ce78b3,#ad5fad,#834ba0,#573b88
* {smcl}
* {title:carto Sunset}{asis}
n:carto Sunset
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f3e79b,#5c53a5
P3:#f3e79b,#eb7f86,#5c53a5
P4:#f3e79b,#f8a07e,#ce6693,#5c53a5
P5:#f3e79b,#fab27f,#eb7f86,#b95e9a,#5c53a5
P6:#f3e79b,#fabc82,#f59280,#dc6f8e,#ab5b9e,#5c53a5
P7:#f3e79b,#fac484,#f8a07e,#eb7f86,#ce6693,#a059a0,#5c53a5
* {smcl}
* {title:carto Magenta}{asis}
n:carto Magenta
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#f3cbd3,#6c2167
P3:#f3cbd3,#ca699d,#6c2167
P4:#f3cbd3,#dd88ac,#b14d8e,#6c2167
P5:#f3cbd3,#e498b4,#ca699d,#a24186,#6c2167
P6:#f3cbd3,#e7a2b9,#d67ba5,#bc5894,#983a81,#6c2167
P7:#f3cbd3,#eaa9bd,#dd88ac,#ca699d,#b14d8e,#91357d,#6c2167
* {smcl}
* {title:carto SunsetDark}{asis}
n:carto SunsetDark
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#fcde9c,#7c1d6f
P3:#fcde9c,#e34f6f,#7c1d6f
P4:#fcde9c,#f0746e,#dc3977,#7c1d6f
P5:#fcde9c,#f58670,#e34f6f,#d72d7c,#7c1d6f
P6:#fcde9c,#f89872,#ec666d,#df4273,#c5287b,#7c1d6f
P7:#fcde9c,#faa476,#f0746e,#e34f6f,#dc3977,#b9257a,#7c1d6f
* {smcl}
* {title:carto ag_Sunset}{asis}
n:carto ag_Sunset
c:sequential
d:aggregation palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#4b2991,#edd9a3
P3:#4b2991,#ea4f88,#edd9a3
P4:#4b2991,#c0369d,#fa7876,#edd9a3
P5:#4b2991,#a52fa2,#ea4f88,#fa9074,#edd9a3
P6:#4b2991,#932da3,#d43f96,#f7667c,#f89f77,#edd9a3
P7:#4b2991,#872ca2,#c0369d,#ea4f88,#fa7876,#f6a97a,#edd9a3
* {smcl}
* {title:carto BrwnYl}{asis}
n:carto BrwnYl
c:sequential
d:quantitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#ede5cf,#541f3f
P3:#ede5cf,#c1766f,#541f3f
P4:#ede5cf,#d39c83,#a65461,#541f3f
P5:#ede5cf,#daaf91,#c1766f,#95455a,#541f3f
P6:#ede5cf,#ddba9b,#cd8c7a,#b26166,#8a3c56,#541f3f
P7:#ede5cf,#e0c2a2,#d39c83,#c1766f,#a65461,#813753,#541f3f
* {smcl}
* {title:carto ArmyRose}{asis}
n:carto ArmyRose
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#929b4f,#db8195
P3:#a3ad62,#fdfbe4,#df91a3
P4:#929b4f,#d9dbaf,#f3d1ca,#db8195
P5:#879043,#c1c68c,#fdfbe4,#ebb4b8,#d8758b
P6:#7f883b,#b0b874,#e3e4be,#f6ddd1,#e4a0ac,#d66d85
P7:#798234,#a3ad62,#d0d3a2,#fdfbe4,#f0c6c3,#df91a3,#d46780
* {smcl}
* {title:carto Fall}{asis}
n:carto Fall
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#3d5941,#ca562c
P3:#3d5941,#f6edbd,#ca562c
P4:#3d5941,#b5b991,#edbb8a,#ca562c
P5:#3d5941,#96a07c,#f6edbd,#e6a272,#ca562c
P6:#3d5941,#839170,#cecea2,#f1cf9e,#e19464,#ca562c
P7:#3d5941,#778868,#b5b991,#f6edbd,#edbb8a,#de8a5a,#ca562c
* {smcl}
* {title:carto Geyser}{asis}
n:carto Geyser
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#008080,#ca562c
P3:#008080,#f6edbd,#ca562c
P4:#008080,#b4c8a8,#edbb8a,#ca562c
P5:#008080,#92b69e,#f6edbd,#e6a272,#ca562c
P6:#008080,#7eab98,#ced7b1,#f1cf9e,#e19464,#ca562c
P7:#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c
* {smcl}
* {title:carto Temps}{asis}
n:carto Temps
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#009392,#cf597e
P3:#009392,#e9e29c,#cf597e
P4:#009392,#9ccb86,#eeb479,#cf597e
P5:#009392,#71be83,#e9e29c,#ed9c72,#cf597e
P6:#009392,#52b684,#bcd48c,#edc783,#eb8d71,#cf597e
P7:#009392,#39b185,#9ccb86,#e9e29c,#eeb479,#e88471,#cf597e
* {smcl}
* {title:carto TealRose}{asis}
n:carto TealRose
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#009392,#d0587e
P3:#009392,#f1eac8,#d0587e
P4:#009392,#91b8aa,#f1eac8,#dfa0a0,#d0587e
P5:#009392,#91b8aa,#f1eac8,#dfa0a0,#d0587e
P6:#009392,#72aaa1,#b1c7b3,#e5b9ad,#d98994,#d0587e
P7:#009392,#72aaa1,#b1c7b3,#f1eac8,#e5b9ad,#d98994,#d0587e
* {smcl}
* {title:carto Tropic}{asis}
n:carto Tropic
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#009B9E,#C75DAB
P3:#009B9E,#F1F1F1,#C75DAB
P4:#009B9E,#A7D3D4,#E4C1D9,#C75DAB
P5:#009B9E,#7CC5C6,#F1F1F1,#DDA9CD,#C75DAB
P6:#009B9E,#5DBCBE,#C6DFDF,#E9D4E2,#D99BC6,#C75DAB
P7:#009B9E,#42B7B9,#A7D3D4,#F1F1F1,#E4C1D9,#D691C1,#C75DAB
* {smcl}
* {title:carto Earth}{asis}
n:carto Earth
c:diverging
d:diverging palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P2:#A16928,#2887a1
P3:#A16928,#edeac2,#2887a1
P4:#A16928,#d6bd8d,#b5c8b8,#2887a1
P5:#A16928,#caa873,#edeac2,#98b7b2,#2887a1
P6:#A16928,#c29b64,#e0cfa2,#cbd5bc,#85adaf,#2887a1
P7:#A16928,#bd925a,#d6bd8d,#edeac2,#b5c8b8,#79a7ac,#2887a1
* {smcl}
* {title:carto Antique}{asis}
n:carto Antique
c:qualitative
d:qualitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#855C75,#D9AF6B,#7C7C7C
P4:#855C75,#D9AF6B,#AF6458,#7C7C7C
P5:#855C75,#D9AF6B,#AF6458,#736F4C,#7C7C7C
P6:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#7C7C7C
P7:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#7C7C7C
P8:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#7C7C7C
P9:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#9C9C5E,#7C7C7C
P10:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#9C9C5E,#A06177,#7C7C7C
P11:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#9C9C5E,#A06177,#8C785D,#7C7C7C
P12:#855C75,#D9AF6B,#AF6458,#736F4C,#526A83,#625377,#68855C,#9C9C5E,#A06177,#8C785D,#467378,#7C7C7C
* {smcl}
* {title:carto Bold}{asis}
n:carto Bold
c:qualitative
d:qualitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#7F3C8D,#11A579,#A5AA99
P4:#7F3C8D,#11A579,#3969AC,#A5AA99
P5:#7F3C8D,#11A579,#3969AC,#F2B701,#A5AA99
P6:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#A5AA99
P7:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#A5AA99
P8:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#A5AA99
P9:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#A5AA99
P10:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#CF1C90,#A5AA99
P11:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#CF1C90,#f97b72,#A5AA99
P12:#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#CF1C90,#f97b72,#4b4b8f,#A5AA99
* {smcl}
* {title:carto Pastel}{asis}
n:carto Pastel
c:qualitative
d:qualitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#66C5CC,#F6CF71,#B3B3B3
P4:#66C5CC,#F6CF71,#F89C74,#B3B3B3
P5:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#B3B3B3
P6:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#B3B3B3
P7:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#B3B3B3
P8:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#B3B3B3
P9:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#B3B3B3
P10:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#8BE0A4,#B3B3B3
P11:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#8BE0A4,#B497E7,#B3B3B3
P12:#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#8BE0A4,#B497E7,#D3B484,#B3B3B3
* {smcl}
* {title:carto Prism}{asis}
n:carto Prism
c:qualitative
d:qualitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#5F4690,#1D6996,#666666
P4:#5F4690,#1D6996,#38A6A5,#666666
P5:#5F4690,#1D6996,#38A6A5,#0F8554,#666666
P6:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#666666
P7:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#666666
P8:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#666666
P9:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#CC503E,#666666
P10:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#CC503E,#94346E,#666666
P11:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#CC503E,#94346E,#6F4070,#666666
P12:#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#CC503E,#94346E,#6F4070,#994E95,#666666
* {smcl}
* {title:carto Safe}{asis}
n:carto Safe
c:qualitative
d:qualitative, colorblind palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#88CCEE,#CC6677,#888888
P4:#88CCEE,#CC6677,#DDCC77,#888888
P5:#88CCEE,#CC6677,#DDCC77,#117733,#888888
P6:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#888888
P7:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#888888
P8:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#888888
P9:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#888888
P10:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#888888
P11:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#888888
P12:#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#6699CC,#888888
* {smcl}
* {title:carto Vivid}{asis}
n:carto Vivid
c:qualitative
d:qualitative palette from carto.com/carto-colors
s:https://github.com/CartoDB/CartoColor/blob/master/cartocolor.js, retrieved 31mar2022
P3:#E58606,#5D69B1,#A5AA99
P4:#E58606,#5D69B1,#52BCA3,#A5AA99
P5:#E58606,#5D69B1,#52BCA3,#99C945,#A5AA99
P6:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#A5AA99
P7:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#A5AA99
P8:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#A5AA99
P9:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#A5AA99
P10:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#A5AA99
P11:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#A5AA99
P12:#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99
* {smcl}
* {title:tab 10}{asis}
n:tab 10
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#4E79A7,#F28E2B,#E15759,#76B7B2,#59A14F,#EDC948,#B07AA1,#FF9DA7,#9C755F,#BAB0AC
N:Blue,Orange,Red,Light Teal,Green,Yellow,Purple,Pink,Brown,Light Gray
* {smcl}
* {title:tab 20}{asis}
n:tab 20
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#4E79A7,#A0CBE8,#F28E2B,#FFBE7D,#59A14F,#8CD17D,#B6992D,#F1CE63,#499894,#86BCB6,#E15759,#FF9D9A,#79706E,#BAB0AC,#D37295,#FABFD2,#B07AA1,#D4A6C8,#9D7660,#D7B5A6
N:Blue,Light Blue,Orange,Light Orange,Green,Light Green,Yellow-Green,Yellow,Teal,Light Teal,Red,Pink,Dark Gray,Light Gray,Pink,Light Pink,Purple,Light Purple,Brown,Light Orange
* {smcl}
* {title:tab Color Blind}{asis}
n:tab Color Blind
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#1170aa,#fc7d0b,#a3acb9,#57606c,#5fa2ce,#c85200,#7b848f,#a3cce9,#ffbc79,#c8d0d9
N:Blue,Orange,Light Gray,Dark Gray,Blue,Brown,Gray,Light Blue,Light Orange,Very Light Gray
* {smcl}
* {title:tab Seattle Grays}{asis}
n:tab Seattle Grays
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#767f8b,#b3b7b8,#5c6068,#d3d3d3,#989ca3
N:gray,Light Gray,Dark Gray,Light Gray,Gray
* {smcl}
* {title:tab Traffic}{asis}
n:tab Traffic
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#b60a1c,#e39802,#309143,#e03531,#f0bd27,#51b364,#ff684c,#ffda66,#8ace7e
N:Red,Yellow,Green,Red,Yellow,Green,Red,Yellow,Light Green
* {smcl}
* {title:tab Miller Stone}{asis}
n:tab Miller Stone
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#4f6980,#849db1,#a2ceaa,#638b66,#bfbb60,#f47942,#fbb04e,#b66353,#d7ce9f,#b9aa97,#7e756d
N:Blue,Blue,Light Green,Green,Yellow-Green,Orange,Orange,Brown,Light Yellow,Light Brown,Gray
* {smcl}
* {title:tab Superfishel Stone}{asis}
n:tab Superfishel Stone
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#6388b4,#ffae34,#ef6f6a,#8cc2ca,#55ad89,#c3bc3f,#bb7693,#baa094,#a9b5ae,#767676
N:Blue,Orange,Red,Light Blue,Teal,Yellow-Green,Pink,Light Brown,Light Gray,Dark Gray
* {smcl}
* {title:tab Nuriel Stone}{asis}
n:tab Nuriel Stone
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#8175aa,#6fb899,#31a1b3,#ccb22b,#a39fc9,#94d0c0,#959c9e,#027b8e,#9f8f12
N:Purple,Light Teal,Blue,Yellow-Green,Light Purple,Light Teal,Gray,Blue,Yellow-Green
* {smcl}
* {title:tab Jewel Bright}{asis}
n:tab Jewel Bright
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#eb1e2c,#fd6f30,#f9a729,#f9d23c,#5fbb68,#64cdcc,#91dcea,#a4a4d5,#bbc9e5
N:Red,Orange,Orange,Yellow,Green,Light Teal,Light Blue,Light Blue,Very Light Blue
* {smcl}
* {title:tab Summer}{asis}
n:tab Summer
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#bfb202,#b9ca5d,#cf3e53,#f1788d,#00a2b3,#97cfd0,#f3a546,#f7c480
N:Yellow-Green,Yellow-Green,Red,Red,Blue,Light Blue,Orange,Light Orange
* {smcl}
* {title:tab Winter}{asis}
n:tab Winter
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#90728f,#b9a0b4,#9d983d,#cecb76,#e15759,#ff9888,#6b6b6b,#bab2ae,#aa8780,#dab6af
N:Gray,Light Gray,Yellow-Green,Light Yellow-Green,Red,Red,Dark Gray,Light Gray,Brown,Light Pink
* {smcl}
* {title:tab Green-Orange-Teal}{asis}
n:tab Green-Orange-Teal
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#4e9f50,#87d180,#ef8a0c,#fcc66d,#3ca8bc,#98d9e4,#94a323,#c3ce3d,#a08400,#f7d42a,#26897e,#8dbfa8
N:Green,Light Green,Orange,Light Orange,Blue,Light Blue,Yellow-Green,Yellow-Green,Yellow-Green,Yellow,Teal,Light Teal
* {smcl}
* {title:tab Red-Blue-Brown}{asis}
n:tab Red-Blue-Brown
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#466f9d,#91b3d7,#ed444a,#feb5a2,#9d7660,#d7b5a6,#3896c4,#a0d4ee,#ba7e45,#39b87f,#c8133b,#ea8783
N:Blue,Light Blue,Red,Pink,Brown,Light Orange,Blue,Light Blue,Brown,Light Orange,Red,Red
* {smcl}
* {title:tab Purple-Pink-Gray}{asis}
n:tab Purple-Pink-Gray
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#8074a8,#c6c1f0,#c46487,#ffbed1,#9c9290,#c5bfbe,#9b93c9,#ddb5d5,#7c7270,#f498b6,#b173a0,#c799bc
N:Purple,Light Purple,Pink,Pink,Gray,Light Gray,Light Purple,Light Purple,Dark Gray,Pink,Purple,Light Purple
* {smcl}
* {title:tab Hue Circle}{asis}
n:tab Hue Circle
c:qualitative
d:qualitative palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#1ba3c6,#2cb5c0,#30bcad,#21B087,#33a65c,#57a337,#a2b627,#d5bb21,#f8b620,#f89217,#f06719,#e03426,#f64971,#fc719e,#eb73b3,#ce69be,#a26dc2,#7873c0,#4f7cba
N:Blue,Blue,Teal,Teal,Green,Green,Yellow-Green,Yellow,Yellow,Orange,Orange,Red,Red,Pink,Pink,Purple,Purple,Purple,Blue
* {smcl}
* {title:tab Blue-Green}{asis}
n:tab Blue-Green
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#feffd9,#f2fabf,#dff3b2,#c4eab1,#94d6b7,#69c5be,#41b7c4
* {smcl}
* {title:tab Blue Light}{asis}
n:tab Blue Light
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#e5e5e5,#e0e3e8,#dbe1ea,#d5dfec,#d0dcef,#cadaf1,#c4d8f3
* {smcl}
* {title:tab Orange Light}{asis}
n:tab Orange Light
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#e5e5e5,#ebe1d9,#f0ddcd,#f5d9c2,#f9d4b6,#fdd0aa,#ffcc9e
* {smcl}
* {title:tab Blue}{asis}
n:tab Blue
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#b9ddf1,#afd6ed,#a5cfe9,#9bc7e4,#92c0df,#89b8da,#80b0d5,#79aacf,#72a3c9,#6a9bc3,#6394be,#5b8cb8,#5485b2,#4e7fac,#4878a6,#437a9f,#3d6a98,#376491,#305d8a,#2a5783
* {smcl}
* {title:tab Orange}{asis}
n:tab Orange
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ffc685,#fcbe75,#f9b665,#f7ae54,#f5a645,#f59c3c,#f49234,#f2882d,#f07e27,#ee7422,#e96b20,#e36420,#db5e20,#d25921,#ca5422,#c14f22,#b84b23,#af4623,#a64122,#9e3d22
* {smcl}
* {title:tab Green}{asis}
n:tab Green
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#b3e0a6,#a5db96,#98d687,#8ed07f,#85ca77,#7dc370,#75bc69,#6eb663,#67af5c,#61a956,#59a253,#519c51,#49964f,#428f4d,#398949,#308344,#2b7c40,#27763d,#256f3d,#24693d
* {smcl}
* {title:tab Red}{asis}
n:tab Red
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ffbeb2,#feb4a6,#fdab9b,#fca290,#fb9984,#fa8f79,#f9856e,#f77b66,#f5715d,#f36754,#f05c4d,#ec5049,#e74545,#e13b42,#da323f,#d3293d,#ca223c,#c11a3b,#b8163a,#ae123a
* {smcl}
* {title:tab Purple}{asis}
n:tab Purple
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#eec9e5,#eac1df,#e6b9d9,#e0b2d2,#daabcb,#d5a4c4,#cf9dbe,#ca96b8,#c48fb2,#be89ac,#b882a6,#b27ba1,#aa759d,#a27099,#9a6a96,#926591,#8c5f86,#865986,#81537f,#7c4d79
* {smcl}
* {title:tab Brown}{asis}
n:tab Brown
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#eedbbd,#ecd2ad,#ebc994,#eac085,#e8b777,#e5ae6c,#e2a562,#de9d5a,#d99455,#d38c54,#ce8451,#c9784d,#c47247,#c16941,#bd6036,#b85636,#b34d34,#ad4433,#a63d32,#9f3632
* {smcl}
* {title:tab Gray}{asis}
n:tab Gray
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#d5d5d5,#cdcecd,#c5c7c6,#bcbfbe,#b4b7b7,#acb0b1,#a4a9ab,#9ca3a4,#939c9e,#8b9598,#848e93,#7c878d,#758087,#6e7a81,#67737c,#616c77,#5b6570,#555f6a,#4f5864,#49525e
* {smcl}
* {title:tab Gray Warm}{asis}
n:tab Gray Warm
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#dcd4d0,#d4ccc8,#cdc4c0,#c5bdb9,#beb6b2,#b7afab,#b0a7a4,#a9a09d,#a29996,#9b938f,#948c88,#8d8481,#867e7b,#807774,#79706e,#736967,#6c6260,#665c51,#5f5654,#59504e
* {smcl}
* {title:tab Blue-Teal}{asis}
n:tab Blue-Teal
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#bce4d8,#aedcd5,#a1d5d2,#95cecf,#89c8cc,#7ec1ca,#72bac6,#66b2c2,#59acbe,#4ba5ba,#419eb6,#3b96b2,#358ead,#3586a7,#347ea1,#32779b,#316f96,#2f6790,#2d608a,#2c5985
* {smcl}
* {title:tab Orange-Gold}{asis}
n:tab Orange-Gold
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#f4d166,#f6c760,#f8bc58,#f8b252,#f7a84a,#f69e41,#f49538,#f38b2f,#f28026,#f0751e,#eb6c1c,#e4641e,#de5d1f,#d75521,#cf4f22,#c64a22,#bc4623,#b24223,#a83e24,#9e3a26
* {smcl}
* {title:tab Green-Gold}{asis}
n:tab Green-Gold
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#f4d166,#e3cd62,#d3c95f,#c3c55d,#b2c25b,#a3bd5a,#93b958,#84b457,#76af56,#67a956,#5aa355,#4f9e53,#479751,#40914f,#3a8a4d,#34844a,#2d7d45,#257740,#1c713b,#146c36
* {smcl}
* {title:tab Red-Gold}{asis}
n:tab Red-Gold
c:sequential
d:sequential palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#f4d166,#f5c75f,#f6bc58,#f7b254,#f9a750,#fa9d4f,#fa9d4f,#fb934d,#f7894b,#f47f4a,#f0774a,#eb6349,#e66549,#e15c48,#dc5447,#d64c45,#d04344,#ca3a42,#c43141,#bd273f,#b71d3e
* {smcl}
* {title:tab Orange-Blue}{asis}
n:tab Orange-Blue
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#9e3d22,#d45b21,#f69035,#d9d5c9,#77acd3,#4f81af,#2b5c8a
* {smcl}
* {title:tab Red-Green}{asis}
n:tab Red-Green
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#a3123a,#e33f43,#f8816b,#ced7c3,#73ba67,#44914e,#24693d
* {smcl}
* {title:tab Green-Blue}{asis}
n:tab Green-Blue
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#24693d,#45934d,#75bc69,#c9dad2,#77a9cf,#4e7fab,#2a5783
* {smcl}
* {title:tab Red-Blue}{asis}
n:tab Red-Blue
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#a90c38,#e03b42,#f87f69,#dfd4d1,#7eaed3,#5383af,#2e5a87
* {smcl}
* {title:tab Red-Black}{asis}
n:tab Red-Black
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ae123a,#e33e43,#f8816b,#d9d9d9,#a0a7a8,#707c83,#49525e
* {smcl}
* {title:tab Gold-Purple}{asis}
n:tab Gold-Purple
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ad9024,#c1a33b,#d4b95e,#e3d8cf,#d4a3c3,#c189b0,#ac7299
* {smcl}
* {title:tab Red-Green-Gold}{asis}
n:tab Red-Green-Gold
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#be2a3e,#e25f48,#f88f4d,#f4d166,#90b960,#4b9b5f,#22763f
* {smcl}
* {title:tab Sunset-Sunrise}{asis}
n:tab Sunset-Sunrise
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#33608c,#9768a5,#e7718a,#f6ba57,#ed7846,#d54c45,#b81840
* {smcl}
* {title:tab Orange-Blue-White}{asis}
n:tab Orange-Blue-White
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#9e3d22,#e36621,#fcad52,#ffffff,#95c5e1,#5b8fbc,#2b5c8a
* {smcl}
* {title:tab Red-Green-White}{asis}
n:tab Red-Green-White
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ae123a,#ee574d,#fdac9e,#ffffff,#91d183,#539e52,#24693d
* {smcl}
* {title:tab Green-Blue-White}{asis}
n:tab Green-Blue-White
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#24693d,#529c51,#8fd180,#ffffff,#95c1dd,#598ab5,#2a5783
* {smcl}
* {title:tab Red-Blue-White}{asis}
n:tab Red-Blue-White
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#a90c38,#ec534b,#feaa9a,#ffffff,#9ac4e1,#5c8db8,#2e5a87
* {smcl}
* {title:tab Red-Black-White}{asis}
n:tab Red-Black-White
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ae123a,#ee574d,#fdac9d,#ffffff,#bdc0bf,#7d888d,#49525e
* {smcl}
* {title:tab Orange-Blue Light}{asis}
n:tab Orange-Blue Light
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#ffcc9e,#f9d4b6,#f0dccd,#e5e5e5,#dae1ea,#cfdcef,#c4d8f3
* {smcl}
* {title:tab Temperature}{asis}
n:tab Temperature
c:diverging
d:diverging palette from Tableau 10
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#529985,#6c9e6e,#99b059,#dbcf47,#ebc24b,#e3a14f,#c26b51
* {smcl}
* {title:alphabet}{asis}
n:alphabet
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#F0A0FF,#0075DC,#993F00,#4C005C,#191919,#005C31,#2BCE48,#FFCC99,#808080,#94FFB5,#8F7C00,#9DCC00,#C20088,#003380,#FFA405,#FFA8BB,#426600,#FF0010,#5EF1F2,#00998F,#E0FF66,#740AFF,#990000,#FFFF80, #FFE100,#FF5005
N:amethyst,blue,caramel,damson,ebony,forest,green,honeydew,iron,jade,khaki,lime,magenta,navy,orange,pink,quagmire,red,sky,turquoise,uranium,violet,wine,xanthin,yellow,zinnia
* {smcl}
* {title:alphabet2}{asis}
n:alphabet2
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#AA0DFE,#3283FE,#85660D,#782AB6,#565656,#1C8356,#16FF32,#F7E1A0,#E2E2E2,#1CBE4F,#C4451C,#DEA0FD,#FE00FA,#325A9B,#FEAF16,#F8A19F,#90AD1C,#F6222E,#1CFFCE,#2ED9FF,#B10DA1,#C075A6,#FC1CBF,#B00068,#FBE426,#FA0087
N:amethyst,blue,caramel,damson,ebony,forest,green,honey,iron,jade,kingcrab,lavender,magenta,navy,orange,pink,quagmire,red,sea,turquoise,ultraviolet,violet,wine,xanthin,yellow,zinnia
* {smcl}
* {title:cols25}{asis}
n:cols25
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#1F78C8,#ff0000,#33a02c,#6A33C2,#ff7f00,#565656,#FFD700,#a6cee3,#FB6496,#b2df8a,#CAB2D6,#FDBF6F,#999999,#EEE685,#C8308C,#FF83FA,#C814FA,#0000FF,#36648B,#00E2E5,#00FF00,#778B00,#BEBE00,#8B3B00,#A52A3C
* {smcl}
* {title:polychrome}{asis}
n:polychrome
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:#5A5156,#E4E1E3,#F6222E,#FE00FA,#16FF32,#3283FE,#FEAF16,#B00068,#1CFFCE,#90AD1C,#2ED9FF,#DEA0FD,#AA0DFE,#F8A19F,#325A9B,#C4451C,#1C8356,#85660D,#B10DA1,#FBE426,#1CBE4F,#FA0087,#FC1CBF,#F7E1A0,#C075A6,#782AB6,#AAF400,#BDCDFF,#822E1C,#B5EFB5,#7ED7D1,#1C7F93,#D85FF7,#683B79,#66B0FF,#3B00FB
N:Dark_Purplish_Gray,Purplish_White,Vivid_Red,Vivid_Purple,Vivid_Yellowish_Green,Strong_Purplish_Blue,Vivid_Orange_Yellow,Vivid_Purplish_Red,Brilliant_Green,Vivid_Yellow_Green,Vivid_Blue,Brilliant_Purple,Vivid_Violet,Strong_Pink,Strong_Blue,Strong_Reddish_Orange,Vivid_Green,Light_Olive_Brown,Vivid_Reddish_Purple,Vivid_Greenish_Yellow,Vivid_Yellowish_Green,Vivid_Red,Vivid_Purplish_Red,Pale_Yellow,Strong_Reddish_Purple,Vivid_Violet,Vivid_Yellow_Green,Very_Light_Blue,Strong_Reddish_Brown,Very_Light_Yellowish_Green,Very_Light_Bluish_Green,Deep_Greenish_Blue,Vivid_Purple,Deep_Purple,Brilliant_Blue,Vivid_Violet
* {smcl}
* {title:glasbey}{asis}
n:glasbey
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:0 0 255,255 0 0,0 255 0,0 0 51,255 0 182,0 83 0,255 211 0,0 159 255,154 77 66,0 255 190,120 63 193,31 150 152,255 172 253,177 204 113,241 8 92,254 143 66,221 0 255,32 26 1,114 0 85,118 108 149,2 173 36,200 255 0,136 108 0,255 183 159,133 133 103,161 3 0,20 249 255,0 71 158,220 94 147,147 212 255,0 76 255,242 243 24
* {smcl}
* {title:kelly}{asis}
n:kelly
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:242 243 244,34 34 34,243 195 0,135 86 146,243 132 0,161 202 241,190 0 50,194 178 128,132 132 130,0 136 86,230 143 172,0 103 165,249 147 121,96 78 151,246 166 0,179 68 108,220 211 0,136 45 23,141 182 0,101 69 34,226 88 34,43 61 38
* {smcl}
* {title:watlington}{asis}
n:watlington
c:qualitative
d:qualitative palette from pals package in R (kwstat.github.io/pals)
s:https://raw.githubusercontent.com/jrnold/ggthemes/main/data-raw/theme-data/tableau.yml, retrieved 31mar2022
P:0 0 0,87 87 87,173 35 35,42 75 215,29 105 20,129 74 25,129 38 192,160 160 160,129 197 122,157 175 255,41 208 208,255 146 51,255 238 51,233 222 187,255 205 243,242 243 244
* {smcl}
* {title:stc}{asis}
n:st
c:qualitative
d:colors used for p1 to p15 in Stata's stcolor scheme
P:stc1,stc2,stc3,stc4,stc5,stc6,stc7,stc8,stc9,stc10,stc11,stc12,stc13,stc14,stc15
