*! version 1.0.0  29mar2022  Ben Jann
* {smcl}
* {title:ColrSpace lsmaps library}
* 
* This file contains linear segmented colormaps. The syntax is:
*     n:<name of the colormap>
*    [c:<class>]
*    [d:<description>]
*    [s:<source>]
*     r:#r #g #b
*     <R anchor 1>
*     ...
*     <R anchor #r>
*     <G anchor 1>
*     ...
*     <G anchor #g>
*     <B anchor 1>
*     ...
*     <B anchor #b>
* where r:#r #g #b specifies the number of anchor points per channel. An anchor
* must be specified as a space-separated list of three numbers in [0,1]. See
*    {browse "https://matplotlib.org/tutorials/colors/colormap-manipulation.html#creating-linear-segmented-colormaps"}
* Lines starting with * will be ignored (as long as not within a list of anchor
* points). Colormap names should be unique.
* {asis}
* {smcl}
* {title:matplotlib autumn}{asis}
n:matplotlib autumn
c:sequential
d:autumn colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:2 2 2
0 1 1
1 1 1
0 0 0
1 1 1
0 0 0
1 0 0
* {smcl}
* {title:matplotlib spring}{asis}
n:matplotlib spring
c:sequential
d:spring colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:2 2 2
0 1 1
1 1 1
0 0 0
1 1 1
0 1 1
1 0 0
* {smcl}
* {title:matplotlib summer}{asis}
n:matplotlib summer
c:sequential
d:summer colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:2 2 2
0 0 0
1 1 1
0 .5 .5
1 1 1
0 .4 .4
1 .4 .4
* {smcl}
* {title:matplotlib winter}{asis}
n:matplotlib winter
c:sequential
d:winter colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:2 2 2
0 0 0
1 0 0
0 0 0
1 1 1
0 1 1
1 .5 .5
* {smcl}
* {title:matplotlib bone}{asis}
n:matplotlib bone
c:sequential
d:bone colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:3 4 3
0 0 0
.746032 .652778 .652778
1 1 1
0 0 0
.365079 .319444 .319444
.746032 .777778 .777778
1 1 1
0 0 0
.365079 .444444 .444444
1 1 1
* {smcl}
* {title:matplotlib cool}{asis}
n:matplotlib cool
c:sequential
d:cool colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:2 2 2
0 0 0
1 1 1
0 1 1
1 0 0
0 1 1
1 1 1
* {smcl}
* {title:matplotlib copper}{asis}
n:matplotlib copper
c:sequential
d:copper colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:3 2 2
0 0 0
.809524 1 1
1 1 1
0 0 0
1 .7812 .7812
0 0 0
1 .4975 .4975
* {smcl}
* {title:matplotlib coolwarm}{asis}
* Matplotlib source note:
*    # This bipolar color map was generated from CoolWarmFloat33.csv of
*    # "Diverging Color Maps for Scientific Visualization" by Kenneth Moreland.
*    # <http://www.kennethmoreland.com/color-maps/>
n:matplotlib coolwarm
c:diverging
d:coolwarm colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:33 33 33
0 .2298057 .2298057
.03125 .26623388 .26623388
.0625 .30386891 .30386891
.09375 .342804478 .342804478
.125 .38301334 .38301334
.15625 .424369608 .424369608
.1875 .46666708 .46666708
.21875 .509635204 .509635204
.25 .552953156 .552953156
.28125 .596262162 .596262162
.3125 .639176211 .639176211
.34375 .681291281 .681291281
.375 .722193294 .722193294
.40625 .761464949 .761464949
.4375 .798691636 .798691636
.46875 .833466556 .833466556
.5 .865395197 .865395197
.53125 .897787179 .897787179
.5625 .924127593 .924127593
.59375 .944468518 .944468518
.625 .958852946 .958852946
.65625 .96732803 .96732803
.6875 .969954137 .969954137
.71875 .966811177 .966811177
.75 .958003065 .958003065
.78125 .943660866 .943660866
.8125 .923944917 .923944917
.84375 .89904617 .89904617
.875 .869186849 .869186849
.90625 .834620542 .834620542
.9375 .795631745 .795631745
.96875 .752534934 .752534934
1 .705673158 .705673158
0 .298717966 .298717966
.03125 .353094838 .353094838
.0625 .406535296 .406535296
.09375 .458757618 .458757618
.125 .50941904 .50941904
.15625 .558148092 .558148092
.1875 .604562568 .604562568
.21875 .648280772 .648280772
.25 .688929332 .688929332
.28125 .726149107 .726149107
.3125 .759599947 .759599947
.34375 .788964712 .788964712
.375 .813952739 .813952739
.40625 .834302879 .834302879
.4375 .849786142 .849786142
.46875 .860207984 .860207984
.5 .86541021 .86541021
.53125 .848937047 .848937047
.5625 .827384882 .827384882
.59375 .800927443 .800927443
.625 .769767752 .769767752
.65625 .734132809 .734132809
.6875 .694266682 .694266682
.71875 .650421156 .650421156
.75 .602842431 .602842431
.78125 .551750968 .551750968
.8125 .49730856 .49730856
.84375 .439559467 .439559467
.875 .378313092 .378313092
.90625 .312874446 .312874446
.9375 .24128379 .24128379
.96875 .157246067 .157246067
1 .01555616 .01555616
0 .753683153 .753683153
.03125 .801466763 .801466763
.0625 .84495867 .84495867
.09375 .883725899 .883725899
.125 .917387822 .917387822
.15625 .945619588 .945619588
.1875 .968154911 .968154911
.21875 .98478814 .98478814
.25 .995375608 .995375608
.28125 .999836203 .999836203
.3125 .998151185 .998151185
.34375 .990363227 .990363227
.375 .976574709 .976574709
.40625 .956945269 .956945269
.4375 .931688648 .931688648
.46875 .901068838 .901068838
.5 .865395561 .865395561
.53125 .820880546 .820880546
.5625 .774508472 .774508472
.59375 .726736146 .726736146
.625 .678007945 .678007945
.65625 .628751763 .628751763
.6875 .579375448 .579375448
.71875 .530263762 .530263762
.75 .481775914 .481775914
.78125 .434243684 .434243684
.8125 .387970225 .387970225
.84375 .343229596 .343229596
.875 .300267182 .300267182
.90625 .259301199 .259301199
.9375 .220525627 .220525627
.96875 .184115123 .184115123
1 .150232812 .150232812
* {smcl}
* {title:matplotlib jet}{asis}
n:matplotlib jet
c:sequential
d:jet colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:5 6 5
0 0 0
.35 0 0
.66 1 1
.89 1 1
1 .5 .5
0 0 0
.125 0 0
.375 1 1
.64 1 1
.91 0 0
1 0 0
0 .5 .5
.11 1 1
.34 1 1
.65 0 0
1 0 0
* {smcl}
* {title:matplotlib hot}{asis}
n:matplotlib hot
c:sequential
d:hot colormap from matplotlib.org
s:https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py
r:3 4 3
0 .0416 .0416
.365079 1 1
1 1 1
0 0 0
.365079 0 0
.746032 1 1
1 1 1
0 0 0
.746032 0 0
1 1 1
