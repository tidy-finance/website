*! version 1.0.0  26may2020  Ben Jann
* {smcl}
* {title:ColrSpace color generators library}
* 
* Generator entries have the following structure:
*     n:<name>
*    [c:<class>]
*    [d:<description>]
*    [s:<source>]
*     P:<space-separated list of parameters>
* Elements in brackets are optional.
* <name> is specified as
*     <generator> [<scheme>]
* where <generator> must be one of HUE, HCL, LCh, JMh, HSV, or HSL (case does
* not matter).
* For HUE, <class> should be qualitative.
* For HCL, LCh, JMh, HSV, or HSL, <class> can be one of the following:
*     qualitative
*     sequential
*     diverging
* The syntax of P is:
*     P:h1 h2 c1 c2 l1 l1 p1 p2
* where h stands for hue, c stands for chroma/saturation/colorfulness, l stands
* for luminance/value/lightness, and p1/2 are power coefficients.
* For HUE, parameters c2, l2, and p2 are irrelevant, and p1!=0 causes reverse
* direction.
* For HCL, LCh, JMh, HSV and HSL, parameters c2, l2, p1, and p2 are irrelevant
* if <class> is qualitative, and parameter c2 is irrelevant if <class> is diverging.
* Generator names should be unique.
* Lines starting with * will be ignored.
* {asis}
* {smcl}
* {title:hue qualitative}{asis}
n:hue
c:qualitative
P:15 375 100 . 65 . 0 .
* {smcl}
* {title:HCL qualitative}{asis}
n:HCL qualitative
c:qualitative
P:15 . 60 . 70 . . .
n:HCL intense
c:qualitative
P:15 . 100 . 65 . . .
n:HCL dark
c:qualitative
P:15 . 80 . 60 . . .
n:HCL light
c:qualitative
P:15 . 50 . 80 . . .
n:HCL pastel
c:qualitative
P:15 . 35 . 85 . . .
* {smcl}
* {title:HCL sequential}{asis}
n:HCL sequential
c:sequential
P:260 . 80 10 25 95 1 .
n:HCL blues
c:sequential
P:260 . 80 10 25 95 1 .
n:HCL greens
c:sequential
P:145 125 80 10 25 95 1 .
n:HCL grays
c:sequential
P:0 . 0 0 15 95 1 .
n:HCL oranges
c:sequential
P:40 . 100 10 50 95 1 .
n:HCL purples
c:sequential
P:280 . 70 10 20 95 1 .
n:HCL reds
c:sequential
P:10 20 80 10 25 95 1 .
n:HCL heat
c:sequential
P:0 90 100 30 50 90 0.2 1.0
n:HCL heat2
c:sequential
P:0 90 80 30 30 90 0.2 2.0
n:HCL terrain
c:sequential
P:130 0 80 0 60 95 0.1 1.0
n:HCL terrain2
c:sequential
P:130 30 65 0 45 90 0.5 1.5
n:HCL viridis
c:sequential
P:300 75 35 95 15 90 0.8 1.2
n:HCL plasma
c:sequential
P:100 100 60 100 15 95 2.0 0.9
n:HCL redblue
c:sequential
P:0 -100 80 40 40 75 1.0 1.0
* {smcl}
* {title:HCL diverging}{asis}
n:HCL diverging
c:diverging
P:260 0 80 . 30 95 1 .
n:HCL bluered
c:diverging
P:260 0 80 . 30 95 1 .
n:HCL bluered2
c:diverging
P:260 0 100 . 50 95 1 .
n:HCL bluered3
c:diverging
P:180 330 60 . 75 95 1 .
n:HCL greenorange
c:diverging
P:130 45 100 . 70 95 1 .
n:HCL browngreen
c:diverging
P:55 160 60 . 35 95 1 .
n:HCL pinkgreen
c:diverging
P:340 128 90 . 35 95 1 .
n:HCL purplegreen
c:diverging
P:300 128 60 . 30 95 1 .
* {smcl}
* {title:LCh qualitative}{asis}
n:LCh qualitative
c:qualitative
P:28 . 35 . 70 . . .
n:LCh intense
c:qualitative
P:28 . 57 . 65 . . .
n:LCh dark
c:qualitative
P:28 . 47 . 60 . . .
n:LCh light
c:qualitative
P:28 . 29 . 80 . . .
n:LCh pastel
c:qualitative
P:28 . 21 . 85 . . .
* {smcl}
* {title:LCh sequential}{asis}
n:LCh sequential
c:sequential
P:290 . 72 6 25 95 1 .
n:LCh blues
c:sequential
P:290 . 72 6 25 95 1 .
n:LCh greens
c:sequential
P:157 142 95 8 25 95 1 .
n:LCh grays
c:sequential
P:0 . 0 0 15 95 1 .
n:LCh oranges
c:sequential
P:70 . 95 6 50 95 1 .
n:LCh purples
c:sequential
P:310 . 90 7 20 95 1 .
n:LCh reds
c:sequential
P:30 . 56 6 25 95 1 .
n:LCh heat
c:sequential
P:9 112 61 21 50 90 0.2 1.0
n:LCh heat2
c:sequential
P:10 112 54 21 30 90 0.2 2.0
n:LCh terrain
c:sequential
P:140 10 72 0 60 95 0.4 1.0
n:LCh terrain2
c:sequential
P:140 40 64 0 45 90 0.5 1.5
n:LCh viridis
c:sequential
P:324 96 49 82 15 90 1.0 1.0
n:LCh plasma
c:sequential
P:115 115 68 86 15 95 2.0 0.9
n:LCh redblue
c:sequential
P:9 -74 52 25 40 75 1.0 1.0
* {smcl}
* {title:LCh diverging}{asis}
n:LCh diverging
c:diverging
P:293 10 59 . 30 95 1 .
n:LCh bluered
c:diverging
P:293 10 59 . 30 95 1 .
n:LCh bluered2
c:diverging
P:293 10 64 . 50 95 1 .
n:LCh bluered3
c:diverging
P:186 341 45 . 75 95 1 .
n:LCh greenorange
c:diverging
P:140 74 85 . 70 95 1 .
n:LCh browngreen
c:diverging
P:84 171 73 . 35 95 1 .
n:LCh pinkgreen
c:diverging
P:347 128 96 . 35 95 1 .
n:LCh purplegreen
c:diverging
P:324 132 68 . 30 95 1 .
* {smcl}
* {title:JMh qualitative}{asis}
n:JMh qualitative
c:qualitative
P:25 . 24 . 73 . . .
n:JMh intense
c:qualitative
P:25 . 34 . 67 . . .
n:JMh dark
c:qualitative
P:25 . 30 . 62 . . .
n:JMh light
c:qualitative
P:25 . 20 . 82 . . .
n:JMh pastel
c:qualitative
P:25 . 16 . 87 . . .
* {smcl}
* {title:JMh sequential}{asis}
n:JMh sequential
c:sequential
P:255 . 32 7 25 96 1 .
n:JMh blues
c:sequential
P:255 . 32 7 25 96 1 .
n:JMh greens
c:sequential
P:145 . 56 6 24 96 1 .
n:JMh grays
c:sequential
P:0 . 0 0 18 96 1 .
n:JMh oranges
c:sequential
P:70 . 57 3 54 96 1 .
n:JMh purples
c:sequential
P:290 . 31 6 23 96 1 .
n:JMh reds
c:sequential
P:25 25 32 3 29 96 1 .
n:JMh heat
c:sequential
P:8 111 32 11 56 91 0.2 1.0
n:JMh heat2
c:sequential
P:8 111 32 11 35 91 0.2 2.0
n:JMh terrain
c:sequential
P:140 8 32 0 61 96 0.3 1.0
n:JMh terrain2
c:sequential
P:140 30 30 0 46 91 0.5 1.5
n:JMh viridis
c:sequential
P:323 98 24 33 19 92 1.0 1.0
n:JMh plasma
c:sequential
P:114 116 64 35 17 95 2.0 0.9
n:JMh redblue
c:sequential
P:8 -92 39 18 45 77 1.0 1.0
* {smcl}
* {title:JMh diverging}{asis}
n:JMh diverging
c:diverging
P:256 8 31 . 33 96 1 .
n:JMh bluered
c:diverging
P:256 8 31 . 33 96 1 .
n:JMh bluered2
c:diverging
P:256 8 33 . 53 96 1 .
n:JMh bluered3
c:diverging
P:189 342 25 . 77 96 1 .
n:JMh greenorange
c:diverging
P:140 73 35 . 72 96 1 .
n:JMh browngreen
c:diverging
P:97 171 40 . 36 96 1 .
n:JMh pinkgreen
c:diverging
P:351 127 51 . 37 96 1 .
n:JMh purplegreen
c:diverging
P:322 127 39 . 32 96 1 .
* {smcl}
* {title:HSV qualitative}{asis}
n:HSV qualitative
c:qualitative
P:0 . .4 . .85 . . .
n:HSV intense
c:qualitative
P:0 . .6 . .9 . . .
n:HSV dark
c:qualitative
P:0 . .6 . .7 . . .
n:HSV light
c:qualitative
P:0 . .3 . .9 . . .
n:HSV pastel
c:qualitative
P:0 . .2 . .9 . . .
n:HSV rainbow
c:qualitative
P:0 . 1 . 1 . . .
* {smcl}
* {title:HSV sequential}{asis}
n:HSV sequential
c:sequential
P:240 . .8 .05 .6 1 1.2 .
n:HSV blues
c:sequential
P:240 . .8 .05 .6 1 1.2 .
n:HSV greens
c:sequential
P:140 120 1 .1 .3 1 1.2 .
n:HSV grays
c:sequential
P:0 . 0 0 .1 .95 1 .
n:HSV oranges
c:sequential
P:30 . 1 .1 .9 1 1.2 .
n:HSV purples
c:sequential
P:270 . 1 .1 .6 1 1.2 .
n:HSV reds
c:sequential
P:0 20 1 .1 .6 1 1.2 .
n:HSV heat
c:sequential
P:0 60 1 .2 1 1 0.3 .
n:HSV terrain
c:sequential
P:120 0 1 0 .65 .95 0.7 1.5
* {smcl}
* {title:HSV heat0}{asis}
n:HSV heat0
c:heat0
P:0 60 1 0 1 . . .
* {smcl}
* {title:HSV terrain0}{asis}
n:HSV terrain0
c:terrain0
P:120 0 1 0 .65 .9 . .
* {smcl}
* {title:HSV diverging}{asis}
n:HSV diverging
c:diverging
P:240 0 .8 . .6 .95 1.2 .
n:HSV bluered
c:diverging
P:240 0 .8 . .6 .95 1.2 .
n:HSV bluered2
c:diverging
P:240 0 .6 . .8 .95 1.2 .
n:HSV bluered3
c:diverging
P:175 320 .6 . .8 .95 1.2 .
n:HSV greenorange
c:diverging
P:130 40 1 . .8 .95 1.2 .
n:HSV browngreen
c:diverging
P:40 150 .8 . .6 .95 1.2 .
n:HSV pinkgreen
c:diverging
P:330 120 .9 . .6 .95 1.2 .
n:HSV purplegreen
c:diverging
P:290 120 .7 . .5 .95 1.2 .
* {smcl}
* {title:HSL qualitative}{asis}
n:HSL qualitative
c:qualitative
P:0 . .7 . .6 . . .
* {smcl}
* {title:HSL sequential}{asis}
n:HSL sequential
c:sequential
P:240 . .65 .65 .35 .975 1.2 .
* {smcl}
* {title:HSL diverging}{asis}
n:HSL diverging
c:diverging
P:220 350 .65 . .35 .95 1.2 .
