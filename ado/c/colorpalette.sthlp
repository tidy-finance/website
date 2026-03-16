{smcl}
{* 11dec2025}{...}
{hi:help colorpalette}{...}
{right:{browse "http://repec.sowi.unibe.ch/stata/palettes/"}}
({browse "http://ideas.repec.org/p/bss/wpaper/43.html":PDF manual}){...}
{right:{browse "http://github.com/benjann/palettes/"}}
{hline}

{title:Title}

{pstd}{hi:colorpalette} {hline 2} Color palettes


{marker syntax}{...}
{title:Syntax}

{pstd}
    Syntax 1: Retrieve/display colors from one or multiple palettes

{p 8 15 2}
    {cmd:colorpalette} [{it:argument}]
        [{cmd:,} {help colorpalette##opts:{it:palette_options}}
        {help colorpalette##macroopts:{it:macro_options}}
        {help colorpalette##gopts:{it:graph_options}} ]

{pmore}
    where {it:argument} is

{p 12 19 2}
    {help colorpalette##colors:{it:colors}}
    [[, {help colorpalette##opts:{it:palette_options}}] /
    [ {help colorpalette##colors:{it:colors}}
    [, {help colorpalette##opts:{it:palette_options}}] / ... ]]

{pstd}
    Syntax 2: Display an overview of multiple palettes

{p 8 15 2}
    {cmd:colorpalette} [{cmd:,} {help colorpalette##opts:{it:palette_options}}
        {help colorpalette##gopts:{it:graph_options}} ] {cmd::} {it:pspec} [ {cmd:/} {it:pspec} {cmd:/} ... ]

{pmore}
    where {it:pspec} is {cmd:.} to insert a gap or

{p 12 19 2}
    {help colorpalette##colors:{it:colors}} [{cmd:,} {help colorpalette##opts:{it:palette_options}}]

{marker colors}{...}
{pstd}
    In both syntax variants, argument {it:colors} either is
    a {it:{help colorpalette##colorlist:colorlist}}
    specifying a custom list of colors ({help colorpalette##colorlist:see below})
    or

{p 8 15 2}
        {it:{help colorpalette##palette:palette}}[{cmd:%}{it:#}][*{it:#}]

{pstd}
    where {it:{help colorpalette##palette:palette}} is the name of a color palette,
    {cmd:%}{it:#} sets the opacity level (in percent; 0 = fully transparent,
    100 = fully opaque; Stata 15 required), and {cmd:*}{it:#} sets the intensity
    adjustment multiplier (values between 0 and 1 make the colors
    lighter; values larger than one make the colors darker).

{pstd}
    Analyze colors after applying {cmd:colorpalette} (Syntax 1 only)

{p 8 15 2}
    {cmd:colorcheck} [{cmd:,} {help colorpalette##checkopts:{it:check_options}} ]


{synoptset 20 tabbed}{...}
{marker palette}{synopthdr:palette}
{synoptline}
{syntab:Color list}
{synopt:{it:{help colorpalette##colorlist:colorlist}}}custom list of colors{p_end}

{syntab:Stata palettes ({stata "colorpalette, lc(black): st / s2 / s1 / s1r / economist / mono":{it:view}})}
{synopt:{helpb colorpalette##st:st}}15 colors as in Stata's {helpb scheme st:stcolor} scheme; the default in Stata 18 or above{p_end}
{synopt:{helpb colorpalette##s2:s2}}15 colors as in Stata's {helpb scheme s2:s2color} scheme; the default in Stata 17 or below{p_end}
{synopt:{helpb colorpalette##s1:s1}}15 colors as in Stata's {helpb scheme s1:s1color} scheme{p_end}
{synopt:{helpb colorpalette##s1r:s1r}}15 colors as in Stata's {helpb scheme s1:s1rcolor} scheme{p_end}
{synopt:{helpb colorpalette##economist:economist}}15 colors as in Stata's {helpb scheme economist:economist} scheme{p_end}
{synopt:{helpb colorpalette##mono:mono}}15 gray scales as in Stata's monochrome schemes{p_end}

{syntab:Other palettes ({stata "colorpalette, lc(black): okabe / cblind / plottig / 538 / mrc / tfl / burd / lean / tableau":{it:view}})}
{synopt:{helpb colorpalette##cblind:okabe}}8 colorblind-friendly colors by {browse "http://jfly.iam.u-tokyo.ac.jp/color/":Okabe and Ito (2002)}{p_end}
{synopt:{helpb colorpalette##cblind:cblind}}like {cmd:okabe}, but including gray on second position{p_end}
{synopt:{helpb colorpalette##plottig:plottig}}15 colors as in {cmd:plottig} by {browse "http://www.stata-journal.com/article.html?article=gr0070":Bischof (2017b)}{p_end}
{synopt:{helpb colorpalette##538:538}}6 + 7 colors as in {cmd:538} by {browse "http://ideas.repec.org/c/boc/bocode/s458404.html":Bischof (2017a)}{p_end}
{synopt:{helpb colorpalette##mrc:mrc}}7 colors as in {cmd:mrc} by {browse "http://ideas.repec.org/c/boc/bocode/s458103.html":Morris (2015)}{p_end}
{synopt:{helpb colorpalette##tfl:tfl}}8 colors as in {cmd:tfl} by {browse "http://ideas.repec.org/c/boc/bocode/s457703.html":Morris (2013)}{p_end}
{synopt:{helpb colorpalette##burd:burd}}9 + 4 colors as in {cmd:burd} by {browse "http://ideas.repec.org/c/boc/bocode/s457623.html":Briatte (2013)}{p_end}
{synopt:{helpb colorpalette##lean:lean}}15 gray scales as in {cmd:lean} by {browse "http://www.stata-journal.com/article.html?article=gr0002":Juul (2003)}{p_end}
{synopt:{helpb colorpalette##tableau:tableau}}20 categorical colors by {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)}{p_end}

{syntab:Color generators}
{synopt:{helpb colorpalette##hue:hue}}evenly spaced HCL hues{p_end}
{synopt:{helpb colorpalette##hcl:HCL} | {helpb colorpalette##hcl:LCh} | {helpb colorpalette##hcl:JMh}}HCL, LCh, or J'M'h color generator{p_end}
{synopt:{helpb colorpalette##hsv:HSV} | {helpb colorpalette##hsv:HSL}}HSV or HSL color generator{p_end}

{syntab:Collections}
{synopt:{it:{help colorpalette##pals:pals}}}categorical palettes from the {browse "http://github.com/kwstat/pals":{bf:pals}} package in R{p_end}
{synopt:{helpb colorpalette##d3:d3}}categorical palettes from {browse "http://d3js.org/":D3.js}{p_end}
{synopt:{helpb colorpalette##sb:sb}}categorical palettes from {browse "http://seaborn.pydata.org/":seaborn.pydata.org}{p_end}
{synopt:{helpb colorpalette##tab:tab}}color schemes from {browse "http://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782":Tableau 10}{p_end}
{synopt:{helpb colorpalette##tol:tol}}color schemes by Paul Tol from {browse "http://personal.sron.nl/~pault/":personal.sron.nl/~pault}{p_end}
{synopt:{helpb colorpalette##ptol:ptol}}color schemes from {browse "http://personal.sron.nl/~pault/colourschemes.pdf":Tol (2012)}{p_end}
{synopt:{it:{help colorpalette##cbrew:colorbrewer}}}{browse "http://colorbrewer2.org":ColorBrewer} palettes{p_end}
{synopt:{helpb colorpalette##carto:carto}}color schemes from {browse "http://carto.com/carto-colors/":carto.com/carto-colors}{p_end}
{synopt:{it:{help colorpalette##viridis:viridis}}}perceptually uniform colormaps from {browse "http://matplotlib.org":matplotlib.org}{p_end}
{synopt:{it:{help colorpalette##seaborn:seaborn}}}perceptually uniform colormaps from {browse "http://seaborn.pydata.org/":seaborn.pydata.org}{p_end}
{synopt:{helpb colorpalette##matplotlib:matplotlib}}further colormaps from {browse "http://matplotlib.org":matplotlib.org}{p_end}
{synopt:{helpb colorpalette##CET:CET}}perceptually uniform colormaps by {browse "http://arxiv.org/abs/1509.03700":Kovesi (2015)}{p_end}
{synopt:{helpb colorpalette##scico:scico}}Scientific color maps by {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)} ({browse "http://doi.org/10.5281/zenodo.5501399":version 7.0.1}){p_end}
{synopt:{helpb colorpalette##scico8:scico8}}Scientific color maps by {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)} ({browse "http://doi.org/10.5281/zenodo.8409685":version 8.0.1}){p_end}
{synopt:{helpb colorpalette##lin:lin}}semantic colors schemes by {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)}{p_end}
{synopt:{helpb colorpalette##spmap:spmap}}color schemes from {cmd:spmap} by {browse "http://ideas.repec.org/c/boc/bocode/s456812.html":Pisati (2007)}{p_end}
{synopt:{helpb colorpalette##sfso:sfso}}color schemes used by the Swiss Federal
    Statistical Office{p_end}
{synopt:{helpb colorpalette##html:HTML}}categorical HTML colors from {browse "http://www.w3schools.com/colors/colors_names.asp":www.w3schools.com}{p_end}
{synopt:{helpb colorpalette##w3:w3}}W3.CSS colors from {browse "http://www.w3schools.com/w3css/w3css_colors.asp":www.w3schools.com}{p_end}
{synopt:{help colorpalette##wes:{it:wesanderson}}}Wes Anderson palettes from {browse "http://wesandersonpalettes.tumblr.com/":wesandersonpalettes.tumblr.com}{p_end}

{syntab:Custom}
{synopt:{it:{help colorpalette##myname:myname}}}custom palette provided by program {cmd:colorpalette_}{it:myname}{p_end}
{synopt:{help colorpalette##mata:{bf:{ul:m}ata(}{it:name}{bf:)}}}custom palette provided by {helpb colrspace:ColrSpace} object {it:name}{p_end}
{synoptline}
{pstd}
Palette names can be abbreviated and typed in lowercase letters; if abbreviation is ambiguous, the first
matching palette in the sorted will be used.


{synoptset 20 tabbed}{...}
{marker opts}{synopthdr:palette_options}
{synoptline}
{syntab:Main}
{synopt:{helpb colorpalette##n:n({it:#})}}size of the palette (number of colors)
    {p_end}
{synopt:{helpb colorpalette##select:{ul:s}elect({it:numlist})}}select (and order) the colors
    {p_end}
{synopt:{helpb colorpalette##drop:drop({it:numlist})}}drop individual colors
    {p_end}
{synopt:{helpb colorpalette##order:order({it:numlist})}}order the colors
    {p_end}
{synopt:{helpb colorpalette##reverse:{ul:rev}erse}}arrange palette in reverse order
    {p_end}
{synopt:{helpb colorpalette##order:shift({it:#})}}shift the positions of the colors
    {p_end}
{synopt:{helpb colorpalette##opacity:{ul:op}acity({it:numlist})}}set opacity, in percent
    {p_end}
{synopt:{helpb colorpalette##intensity:{ul:in}tensity({it:numlist})}}set color intensity multipliers
    {p_end}
{synopt:{help colorpalette##other:{it:other}}}palette-specific options
    {p_end}

{syntab:Transformation}
{synopt:{helpb colorpalette##ipolate:{ul:ip}olate({it:spec})}}interpolate the colors
    {p_end}
{synopt:{helpb colorpalette##intensify:intensify({it:numlist})}}modify intensity
    {p_end}
{synopt:{helpb colorpalette##saturate:saturate({it:spec})}}modify saturation (colorfulness)
    {p_end}
{synopt:{helpb colorpalette##luminate:luminate({it:spec})}}modify luminance (brightness)
    {p_end}
{synopt:{helpb colorpalette##gscale:{ul:gs}cale{sf:[}({it:spec}){sf:]}}}convert to grayscale
    {p_end}
{synopt:{helpb colorpalette##cblnd:{ul:cb}lind{sf:[}({it:spec}){sf:]}}}simulate color vision deficiency
    {p_end}
{synopt:{helpb colorpalette##torder:{ul:tord}er({it:list})}}set order of transformation options
    {p_end}

{syntab:Technical}
{synopt:{helpb colorpalette##iconvert:{ul:ic}onvert{sf:[}({it:pos}){sf:]}}}resolve intensity multipliers
    {p_end}
{synopt:{helpb colorpalette##forcergb:{ul:force}rgb}}enforce translation to RGB
    {p_end}
{synopt:{helpb colorpalette##noexpand:noexpand}}omit automatic interpolation/recycling
    {p_end}
{synopt:{helpb colorpalette##class:class({it:class})}}set palette class
    {p_end}
{synopt:{helpb colorpalette##pname:name({it:str})}}assign a palette name
    {p_end}
{synoptline}


{synoptset 20 tabbed}{...}
{marker macroopts}{synopthdr:macro_options}
{synoptline}
{synopt:{helpb colorpalette##macrooptions:{ul:gl}obals{sf:[}({it:spec}){sf:]}}}store color codes in global macros
    {p_end}
{synopt:{helpb colorpalette##macrooptions:{ul:loc}als{sf:[}({it:spec}){sf:]}}}store color codes in local macros
    {p_end}
{synopt:{helpb colorpalette##stylefiles:{ul:stylef}iles{sf:[}({it:spec}){sf:]}}}store color codes in style files
    {p_end}
{synopt:{helpb colorpalette##return:{ul:r}eturn({it:spec})}}convert colors and store in macro {cmd:r(c)}
    {p_end}
{synoptline}


{synoptset 20 tabbed}{...}
{marker gopts}{synopthdr:graph_options}
{synoptline}
{syntab:Main}
{synopt:{helpb colorpalette##title:{ul:ti}tle({it:string})}}custom graph title
    {p_end}
{synopt:{helpb colorpalette##nonumbers:{ul:nonum}bers}}omit color IDs
    {p_end}
{synopt:{helpb colorpalette##gropts:{ul:gr}opts({it:options})}}options
    passed through to graph command
    {p_end}

{syntab:Syntax 1 only}
{synopt:{helpb colorpalette##rows:rows({it:#})}}minimum number of rows; default is 5
    {p_end}
{synopt:{helpb colorpalette##names:names}}print names instead of codes
    (if available)
    {p_end}
{synopt:{helpb colorpalette##noinfo:noinfo}}suppress additional color info
    {p_end}
{synopt:{helpb colorpalette##nograph:{ul:nogr}aph}}do not generate a graph
    {p_end}
{synopt:{helpb colorpalette##graph:graph}}enforce crating the graph
    {p_end}

{syntab:Syntax 2 only}
{synopt:{helpb colorpalette##horizontal:{ul:hor}izontal}}horizontal plot; the default
    {p_end}
{synopt:{helpb colorpalette##vertical:{ul:vert}ical}}vertical plot
    {p_end}
{synopt:{helpb colorpalette##span:span}}use all available space for each palette
    {p_end}
{synopt:{helpb colorpalette##barwidth:{ul:barw}idth({it:#})}}width of bars
    {p_end}
{synopt:{helpb colorpalette##labels:{ul:lab}els({it:strlist})}}custom palette labels
    {p_end}
{synopt:{helpb colorpalette##lcolor:{ul:lc}olor({it:colorstyle})}}custom outline color
    {p_end}
{synopt:{helpb colorpalette##lwidth:{ul:lw}idth({it:lwstyle})}}custom outline thickness
    {p_end}
{synoptline}


{synoptset 20 tabbed}{...}
{marker checkopts}{synopthdr:check_options}
{synoptline}
{syntab:Main}
{synopt:{helpb colorpalette##metric:{ul:m}etric({it:metric})}}color difference metric
    {p_end}
{synopt:{helpb colorpalette##monoc:mono({it:spec})}}grayscale conversion settings
    {p_end}
{synopt:{helpb colorpalette##cvd:cvd({it:#})}}severity of color vision deficiency
    {p_end}

{syntab:Graph}
{synopt:{helpb colorpalette##nogrcheck:{ul:nogr}aph}}do not generate a graph
    {p_end}
{synopt:{helpb colorpalette##sort:sort{sf:[}({it:spec}){sf:]}}}sort the colors in the graph
    {p_end}
{synopt:{help colorpalette##gopts:{it:graph_options}}}syntax 2 graph option
    {p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:colorpalette} is a utility command to retrieve or display color
    palettes. A large collection of named colors, palettes, colormaps, and color
    generators is provided and color codes can be entered various formats
    (including, e.g., hex colors). Furthermore, {cmd:colorpalette} provides
    advances features such as color interpolation, grayscale
    conversion, or color vision deficiency simulation. Palette entries are
    returned in {helpb colorpalette##returns:r()} or, optionally, as global
    or local {helpb colorpalette##macrooptions:macros} or as
    {help colorpalette##stylefiles:style files}.

{pstd}
    {cmd:colorcheck} analyzes the colors returned by {cmd:colorpalette}
    by applying grayscale conversion and color vision deficiency transformation,
    and by computing minimum color differences among the converted colors
    (intensity multipliers will be resolved before converting and analyzing the
    colors; opacity settings will be ignored). The
    purpose of the command is to evaluate whether the colors will be
    distinguishable by people who suffer from color vision deficiency and also
    whether the colors will be distinguishable in (non-color) print. The smallest
    noticeable difference between two colors has a color difference value
    (Delta E) of 1.0. A value of, say, 10 seems a reasonable minimum difference
    for colors used to display different features of data. {cmd:colorcheck}
    operates on the results returned by {cmd:colorpalette} (Syntax 1 only) and
    can be applied repeatedly (see {help colorpalette##excheck:example}).

{pstd}
    {cmd:colorpalette} requires version 14.2 of Stata or newer. Users of older
    Stata versions can use command {helpb colorpalette9}, which has limited
    functionality but runs under Stata 9.2 or newer.

{pstd}
    {cmd:colorpalette} requires the {helpb colrspace} package to be installed;
    type {stata ssc install colrspace}.


{marker options}{...}
{title:Options}

{dlgtab:Palette options}

{marker n}{...}
{phang}
    {opt n(#)} specifies the size of the palette (the number of colors). Many
    palettes, such as, e.g., the color generators or the sequential and
    diverging ColorBrewer palettes, are adaptive to {opt n()} in the sense that
    they return different colors depending on {opt n()}. Other palettes, such
    as {cmd:s2}, contain a fixed set of colors. If {opt n()} is different from
    the (maximum or minimum) number of colors defined by a palette, the colors
    are either recycled or interpolated, depending on the class of the palette; see
    option {helpb colorpalette##class:class()} below. To prevent automatic
    recycling/interpolation, specify option {helpb colorpalette##noexpand:noexpand}
    (see below).

{marker select}{...}
{phang}
    {opth select(numlist)} selects (and reorders) the colors retrieved from
    the palette. Positive numbers refer to positions from the start;
    negative numbers refer to positions from the end. Only one of {cmd:select()}
    and {cmd:order()} is allowed; only one of {cmd:select()}
    and {cmd:drop()} is allowed. {cmd:select()} is applied after {cmd:n()} has
    taken effect.

{marker drop}{...}
{phang}
    {opth drop(numlist)} drops individual colors retrieved from
    the palette. Positive numbers refer to positions from the start;
    negative numbers refer to positions from the end. Only one of {cmd:select()}
    and {cmd:drop()} is allowed. {cmd:drop()} is applied after {cmd:n()} has
    taken effect.

{marker order}{...}
{phang}
    {opth order(numlist)} reorders the colors. Positive numbers refer to positions
    from the start; negative numbers refer to positions from the end. Colors
    not covered in {it:numlist} will be placed last, in their original order. Only
    one of {cmd:order()} and {cmd:select()} is allowed. {cmd:order()} is
    applied after {cmd:n()} and {cmd:drop()} have taken effect.

{marker reverse}{...}
{phang}
    {opt reverse} returns the colors in reverse order. {cmd:reverse} is
    applied after {cmd:n()}, {cmd:select()}, {cmd:drop()}, and {cmd:order()}
    have taken effect.

{marker shift}{...}
{phang}
    {opth shift(#)} shifts the positions of the colors up (if #>0) or down (if #<0),
    wrapping positions around at the end. If # is in (-1,1),
    the colors are shifted by trunc(#*n) positions, where n is the size of the
    palette (proportional shift); if abs(#)>=1, the colors are shifted by
    trunc(#) positions. {cmd:shift()} is
    applied after {cmd:n()}, {cmd:select()}, {cmd:drop()},
    {cmd:order()}, and {cmd:reverse} have taken effect.

{marker opacity}{...}
{phang}
    {opth opacity(numlist)} sets the opacity level(s) (Stata 15 required). The
    values in {it:numlist} must be between 0 (fully transparent) and 100
    (fully opaque). Specify multiple values to use
    different opacity levels across the colors. If the number of specified
    opacity levels is smaller than the number of colors, the levels will be
    recycled; if the number of opacity levels is larger than the number of
    colors, the colors will be recycled. To skip assigning opacity to a
    particular color, you may set the corresponding element to
    {cmd:.} (missing). {cmd:opacity()} is applied after
    {cmd:n()}, {cmd:select()}, {cmd:drop()}, {cmd:order()}, {cmd:reverse},
    and {cmd:shift()} have taken effect. Opacity levels specified in
    {it:{help colorpalette##colors:colors}} take precedence over {cmd:opacity()}.

{marker intensity}{...}
{phang}
    {opth intensity(numlist)} sets the color intensity adjustment multipliers.
    The values in {it:numlist} must be between 0 and 255. Values below 1 make
    the colors lighter; values larger than 1 make the colors darker (although
    the allowed scale goes up to 255, values as low as 5 or 10 may already make
    a color black). Specify multiple values to assign different
    multipliers across the colors. If the number of specified multipliers is
    smaller than the number of colors, the multipliers will be recycled; if the
    number of multipliers is larger than the number of colors, the colors will
    be recycled. To skip assigning an intensity multiplier to a particular
    color, you may set the corresponding element to {cmd:.} (missing).
    {cmd:intensity()} is applied after
    {cmd:n()}, {cmd:select()}, {cmd:drop()}, {cmd:order()}, {cmd:reverse}, and
    {cmd:shift()} have taken effect. Intensity multipliers specified in
    {it:{help colorpalette##colors:colors}} take precedence over {cmd:intensity()}.

{pmore}
    Example: {stata `"colorpalette "189 30 36", intensity(0.1(.05)1) name(Reds)"'}

{marker other}{...}
{phang}
    {it:other} are additional palette-specific options. See the descriptions of
    the palettes below. Palette-specific options have to be unique, that is, they
    can only be specified once per palette. When collecting results from multiple
    palettes, palette options can be specified at the global level, to define
    default settings for all palettes, or at the local level of an individual
    palette. For general palette options, defaults set at the global
    level can be overridden by repeating an option at the local level. Such
    repetitions are not allowed for palette-specific options.

{marker ipolate}{...}
{phang}
    {cmd:ipolate(}{it:n}[{cmd:,} {it:suboptions}]{cmd:)}
    interpolates the colors to a total of {it:n} colors (intensity multipliers
    and opacity levels, if defined, will also be interpolated).

{pmore}
    Example: {stata colorpalette cranberry dkorange, ipolate(5) name(red to orange)}

{pmore}
    Suboptions are as follows.

{phang2}
    {it:cspace} selects the color space in which the colors are
    interpolated. The default space is {cmd:Jab} (perceptually uniform
    CIECAM02-based J'a'b'). Other possibilities are, for example, {cmd:RGB}, {cmd:lRGB},
    {cmd:Lab}, {cmd:LCh}, {cmd:Luv}, {cmd:HCL}, {cmd:JMh}, or {cmd:HSV}; see
    help {helpb colrspace##ipolate:colrspace} for details.

{pmore2}
    Example: {stata "colorpalette: #fafa6e #2a4858, ipolate(10) name(Jab) / #fafa6e #2a4858, ipolate(10, JMh) name(JMh)"}

{phang2}
    {opt ra:nge(lb [ub])} sets the interpolation range, where {it:lb} and
    {it:ub} are the lower and upper bounds. The default is {cmd:range(0 1)}. If
    {it:lb} is larger than {it:ub}, the colors are returned in reverse
    order. Extrapolation will be applied if the specified range exceeds [0,1].

{pmore2}
    Example: {stata "colorpalette: #fafa6e #2a4858, ipolate(11, HCL) name([0,1]) / #fafa6e #2a4858, ipolate(6, HCL range(0 .5)) name([0,.5])"}

{phang2}
    {opt pow:er(#)}, with # > 0, determines how the destination colors are
    distributed across the interpolation range. The default is to distribute
    them evenly; this is equivalent to {cmd:power(1)}. A power value larger
    than 1 squishes the positions towards the lower bound. If interpolating
    between two colors, this means that the first color will dominate most of
    the interpolation range (slow to fast transition). A value between 0 and 1
    squishes the positions towards the upper bound, thus making the second
    color the dominant color for most of the range (fast to slow
    transition). Another way to think of the effect of {cmd:power()} is that
    it moves the center of the color gradient up (if # > 1) or down (if 0 < # < 1).

{pmore2}
    Example: {stata "colorpalette: #fafa6e #2a4858, ipolate(30, HCL power(1.5)) name(slow) / #fafa6e #2a4858, ipolate(30, HCL power(0.7)) name(fast)"}

{phang2}
    {opth pos:itions(numlist)} specifies the positions of the origin colors
    with respect to the interpolation range. The default is to arrange them on
    a regular grid from 0 and 1. If the number of specified positions is smaller
    than the number of origin colors, default positions are used for the
    remaining colors. If the same position is specified for multiple
    colors, these colors will be averaged before applying interpolation.

{pmore2}
    Example: {stata "colorpalette: Black Crimson Gold, ipolate(30, position(0 .3 1)) name(left) / Black Crimson Gold, ipolate(30, position(0 .7 1)) name(right)"}

{phang2}
    {opt pad:ded} requests padded interpolation. By
    default, the first color and the last color are taken as the end points of
    the interpolation range; these colors thus remain unchanged. Alternatively,
    if you specify option {cmd:padded}, the first and last colors will be interpreted as
    interval midpoints on an equally-spaced grid. This increases the
    interpolation range by half an interval on each side and causes the
    first color and the last color to be affected by the interpolation.

{pmore2}
    Example (the difference is subtle, but the padded variant is a bit darker at the
    bottom and the top, which represents the original 11-color scale better
    than the unpadded variant):

{p 16 18 2}
    . {stata "colorpalette, span vertical: BrBG / BrBG, ipolate(30) name(ipolate) / BrBG, ipolate(30, padded) name(padded ipolate)"}

{pmore}
    Circular interpolation will be used for palettes declared as {cmd:cyclic}
    or {cmd:circular} (see option {helpb colorpalette##class:class()}). In this
    case, options {cmd:range()}, {cmd:power()}, {cmd:positions()}, and
    {cmd:padded} have no effect.

{marker intensify}{...}
{phang}
    {opth intensify(numlist)} modifies the intensity of the colors. The values
    in {it:numlist} must be between 0 and 255. Values below 1 make the colors
    lighter; values larger than 1 make the colors darker (although the allowed
    scale goes up to 255, values as low as 5 or 10 may already make a color
    black). Specify multiple values to apply different adjustments
    across the colors. If the number of specified values is smaller than
    the number of colors, the values will be recycled; if the number of
    values is larger than the number of colors, the colors will be
    recycled. To skip adjusting the intensity of a particular color, you may
    set the corresponding element to {cmd:.} (missing).

{pmore}
    {cmd:intensify()} applies the same kind of intensity adjustment as
    implemented by the intensity adjustment multipliers set by {cmd:intensity()}.
    The difference between {cmd:intensify()} and {cmd:intensity()} is that
    {cmd:intensity()} only records the intensity multipliers (which are then
    returned as part of the color definitions), whereas {cmd:intensify()}
    directly applies intensity adjustment by transforming the RGB values.

{pmore}
    Example: {stata `"colorpalette "189 30 36", intensify(0.1(.05)1) name(Reds)"'}

{marker saturate}{...}
{phang}
    {cmd:saturate(}{it:{help numlist}}[{cmd:,} {it:cspace} {cmd:level}]{cmd:)}
    modifies the saturation (colorfulness) of the colors. Positive numbers will
    increase the chroma channel of the colors by the specified amount, negative
    numbers will reduce chroma. Specify multiple values to apply different
    adjustments across the colors. If the number of values is smaller than the
    number of colors, the values will be recycled; if the number of values is
    larger than the number of colors, the colors will be recycled. To skip
    adjusting the saturation of a particular color, you may set the
    corresponding element to {cmd:.} (missing). Suboptions are as
    follows.

{phang2}
    {it:cspace} specifies the color space in which the colors are manipulated.
    Possible spaces are {cmd:LCh} (cylindrical representation of CIE L*a*b*),
    {cmd:HCL} (cylindrical representation of CIE L*u*v*), {cmd:JCh} (CIECAM02
    JCh), and {cmd:JMh} (CIECAM02-based J'M'h). The default is {cmd:LCh}.

{phang2}
    {cmd:level} specifies that the provided numbers are levels, not
    differences. The default is to adjust the chroma values of the colors by
    adding/subtracting the specified amounts. Alternatively, if {cmd:levels} is
    specified, the chroma values of the colors will be set to the specified
    levels. Chroma values of typical colors lie between 0 and 100 or maybe 150.

{pmore}
    Example: {stata "colorpalette: RdYlGn, saturate(25) name(+25) / RdYlGn / RdYlGn, saturate(-25) name(-25)"}

{marker luminate}{...}
{phang}
    {cmd: luminate(}{it:{help numlist}}[{cmd:,} {it:cspace} {cmd:level}]{cmd:)}
    modifies the luminance (brightness) of the colors. Positive numbers will
    increase the luminance of the colors by the specified amount, negative
    numbers will reduce luminance. Specify multiple values to apply different
    adjustments across the colors. If the number of values is smaller than the
    number of colors, the values will be recycled; if the number of values is
    larger than the number of colors, the colors will be recycled. To skip
    adjusting the luminance of a particular color, you may set the
    corresponding element to {cmd:.} (missing). Suboptions are as follows.

{phang2}
    {it:cspace} specifies the color space in which the colors are manipulated.
    Possible spaces are {cmd:Lab} (CIE L*a*b*), {cmd:Luv} (CIE L*u*v*),
    {cmd:JCh} (CIECAM02 JCh), and {cmd:JMh} (CIECAM02-based J'M'h) ({cmd:LCh},
    {cmd:HCL}, and {cmd:Jab} are also allowed, but result in the same colors as
    {cmd:Lab}, {cmd:Luv}, and {cmd:JMh}, respectively). The
    default is {cmd:JMh}.

{phang2}
    {cmd:level} specifies that the provided numbers are levels, not
    differences. The default is to adjust the luminance values of the colors by
    adding/subtracting the specified amounts. Alternatively, if {cmd:levels} is
    specified, the luminance values of the colors will be set to the specified
    levels. Luminance values of typical colors lie between 0 and 100.

{pmore}
    Examples:

{p 12 16 2}
    . {stata "colorpalette: ptol, luminate(15) name(+15) / ptol / ptol, luminate(-15) name(-25)"}
    {p_end}
{p 12 16 2}
    . {stata "colorpalette: blue red green, select(1 1 2 2 3 3) luminate(. 40)"}

{marker gscale}{...}
{phang}
    {cmd:gscale}[{cmd:(}[{it:{help numlist}}] [{cmd:,} {it:cspace} {cmdab:noic:onvert}]{cmd:)}]
    converts the colors to gray, where {it:numlist} in [0,1] specifies the
    proportion of gray. The default is {cmd:1} (full conversion). Specify
    multiple values to apply different adjustments across the colors. If the
    number of values is smaller than the number of colors, the values will be
    recycled; if the number of values is larger than the number of colors, the
    colors will be recycled. To skip adjusting a particular color, you may set the
    corresponding element to {cmd:.} (missing). Suboption {it:cspace} specifies the
    color space in which the conversion is performed; it may be {cmd:LCh}
    (cylindrical representation of CIE L*a*b*), {cmd:HCL} (cylindrical
    representation of CIE L*u*v*), {cmd:JCh} (CIECAM02 JCh), and {cmd:JMh}
    (CIECAM02-based J'M'h). The default is {cmd:LCh}. Subption {cmdab:noiconvert}
    omits resolving intensity multipliers before applying grayscale
    conversion. Example:

{p 12 16 2}
    . {stata "colorpalette: s2 / s2, gscale(.5) name(50% gray) / s2, gscale name(100% gray)"}

{marker cblnd}{...}
{phang}
    {cmd:cblind}[{cmd:(}[{it:{help numlist}}] [{cmd:,} {it:type} {cmdab:noic:onvert}]{cmd:)}]
    simulates color vision deficiency (CVD), where {it:numlist} in [0,1] specifies
    the severity of the deficiency. The default is {cmd:1} (maximum severity,
    i.e. deuteranopia, protanopia, or tritanopia, respectively). Specify
    multiple values to apply different adjustments across the colors. If the
    number of values is smaller than the number of colors, the values will be
    recycled; if the number of values is larger than the number of colors, the
    colors will be recycled. To skip adjusting a particular color, you may set
    the corresponding element to {cmd:.} (missing). Suboption {it:type}
    specifies type of CVD, which may be {cmdab:d:euteranomaly} (the default),
    {cmdab:p:rotanomaly}, or {cmdab:t:ritanomaly}. See
    {browse "http://en.wikipedia.org/wiki/Color_blindness":Wikipedia} for basic
    information on color blindness. Subption {cmdab:noiconvert}
    omits resolving intensity multipliers before applying CVD conversion. Example:

{p 12 16 2}
    . {stata "colorpalette: Set1 / Set1, cblind(.5, deut) name(50% deut) / Set1, cblind(.5, prot) name(50% prot) / Set1, cblind(1, trit) name(tritanopia)"}

{marker torder}{...}
{phang}
    {opt torder(list)} specifies the order in which the transformation options
    {helpb colorpalette##ipolate:ipolate()},
    {helpb colorpalette##intensify:intensify()},
    {helpb colorpalette##saturate:saturate()},
    {helpb colorpalette##luminate:luminate()},
    {helpb colorpalette##gscale:gscale()}, and
    {helpb colorpalette##cblnd:cblind()} are applied. The default order is
    {cmdab:ip:olate} {cmdab:in:tensify} {cmdab:sat:urate} {cmdab:lum:inate}
    {cmdab:gs:cale} {cmdab:cb:lind}. If {it:list} does not contain all keywords,
    the remaining options will be applied last, in their default order. In any case,
    the transformation options will be applied after {cmd:n()},
    {cmd:select()}, {cmd:drop()}, {cmd:order()},
    {cmd:reverse}, {cmd:shift()}, {cmd:opacity()}, and {cmd:intensity()}
    have taken effect.

{marker iconvert}{...}
{phang}
    {cmd:iconvert}[{cmd:(}{it:pos}{cmd:)}] resolves intensity adjustment
    multipliers that may have been specified as part of the color definitions
    or in option {helpb colorpalette##intensity:intensity()}. The RGB values of the colors will be adjusted
    and the intensity multipliers will be removed. Argument {it:pos} specifies
    the position at which the conversion is applied; it can be one of the
    following.

{p2colset 13 25 30 2}{...}
{p2col:{cmdab:f:irst}}at the beginning, directly after {helpb colorpalette##intensity:intensity()}{p_end}
{p2col:{cmdab:ip:olate}}before {helpb colorpalette##ipolate:ipolate()}{p_end}
{p2col:{cmdab:in:tensify}}before {helpb colorpalette##intensify:intensify()}{p_end}
{p2col:{cmdab:sat:urate}}before {helpb colorpalette##saturate:saturate()}{p_end}
{p2col:{cmdab:lum:inate}}before {helpb colorpalette##luminate:luminate()}{p_end}
{p2col:{cmdab:l:ast}}at the very end{p_end}

{pmore}
    Default is {cmd:first}. Note that {helpb colorpalette##gscale:gscale()}
    and {helpb colorpalette##cblnd:cblind()} will automatically call
    {cmd:iconvert} (unless suboption {cmd:noiconvert} is applied).

{marker forcergb}{...}
{phang}
    {opt forcergb} enforces translation of all colors to RGB. By default,
    {cmd:colorpalette} does not translate colors specified as Stata color
    names. Specify {opt forcergb} to return these colors as RGB values.

{marker noexpand}{...}
{phang}
    {opt noexpand} omits recycling or interpolating colors if the number of
    requested colors is larger than the maximum number of colors defined in a palette.

{marker class}{...}
{phang}
    {opt class(class)} declares the class of the the palette, where {it:class}
    may be {cmdab:q:ualitative} (or {cmdab:cat:egorical}), {cmdab:s:equential},
    {cmdab:d:iverging}, {cmdab:cyc:lic} (or {cmdab:circ:ular}), or any other
    string. Palettes declared as {cmd:qualitative} or {cmd:categoprical} will be
    recycled, all other palettes will be interpolated (if recycling or
    interpolation is necessary). Specifying {cmd:class()} only affects
    palettes that do not set the class as part of their definition.

{marker pname}{...}
{phang}
    {opt name(str)} assigns a custom name to the palette.

{marker macrooptions}{...}
{dlgtab:Macro options (syntax 1 only)}

{phang}
    {cmd:globals}[{cmd:(}{it:spec}{cmd:)}] stores the color codes as
    {helpb global} macros (syntax 1 only). Use this option as an alternative to
    obtaining the color codes from {helpb colorpalette##returns:r()}; see the
    {help colorpalette##exglobals:example} below. The syntax of {it:spec} is

{p 12 16 2}
    [{it:namelist}] [{it:stub}{cmd:*}] [{cmd:,}
    {opt p:refix(prefix)}
    {opt s:uffix(suffix)}
    {opt non:ames} ]

{pmore}
    where {it:namelist} provides custom names for the colors and
    {it:stub}{cmd:*} provides a stub for automatic names. If no name is found
    for a color in the palette definition and no custom name is provided, an
    automatic name defined as {it:stub}{it:#}{it:suffix} will be used, where {it:#}
    is the number of the color in the palette. The default {it:stub} is {cmd:p}
    or as set by {cmd:prefix()}. Options are as follows:

{phang2}
    {cmd:prefix()} specifies a common prefix to be added to the names.

{phang2}
    {cmd:suffix()} specifies a common suffix to be added to the names.

{phang2}
    {cmd:nonames} prevents {cmd:colorpalette} from using the names found in the
    palette definition.

{pmore}
    Graph display will be disabled unless option {cmd:graph} is specified.

{phang}
    {cmd:locals}[{cmd:(}{it:spec}{cmd:)}] stores the color codes as
    {helpb local} macros (syntax 1 only). Syntax and functionality is
    as described for option {cmd:globals()}, with the exception that
    {it:stub} defaults to empty string. Graph display will be disabled unless
    option {cmd:graph} is specified.

{marker stylefiles}{...}
{phang}
    {cmd:stylefiles}[{cmd:(}{it:spec}{cmd:)}] stores the color codes in style
    files on disk (syntax 1 only). This makes the colors permanently available
    by their name, just like official Stata's color names; see the
    {help colorpalette##exstylefiles:example} below (you may need to
    flush graph's working memory using {helpb discard} or {helpb clear all}
    before the new colors become available). Style files will only be
    created for colors that are represented by a simple RGB code; codes that
    include an intensity-adjustment or opacity operator and colors
    that are referred to by their Stata name will be skipped. The syntax of
    {it:spec} is

{p 12 16 2}
    [{it:namelist}] [{it:stub}{cmd:*}] [{cmd:,}
    {opt p:refix(prefix)}
    {opt s:uffix(suffix)}
    {opt non:ames}
    {opt pers:onal}
    {opt path(path)}
    {opt replace} ]

{pmore}
    where {it:namelist} provides custom names for the colors and
    {it:stub}{cmd:*} provides a stub for automatic names. If no name is found
    for a color in the palette definition and no custom name is provided, an
    automatic name defined as {it:stub}{it:#}{it:suffix} will be used, where {it:#}
    is the number of the color in the palette. The default {it:stub} is empty string
    or as set by {cmd:prefix()}. Options are as follows:

{phang2}
    {cmd:prefix()} specifies a common prefix to be added to the names.

{phang2}
    {cmd:suffix()} specifies a common suffix to be added to the names.

{phang2}
    {cmd:nonames} prevents {cmd:colorpalette} from using the names found in the
    palette definition.

{phang2}
    {cmd:personal} causes the style files to be stored in folder 'style' within
    the {cmd:PERSONAL} ado-file directory; see help {helpb sysdir}. The default
    is to store the style files in folder 'style' within the
    current working directory; see help {helpb pwd}.

{phang2}
    {opt path(path)} provides a custom path for the style files. The default is to
    store the style files in folder 'style' within the current working
    directory. {cmd:path()} and {cmd:personal} are not both allowed.

{phang2}
    {opt replace} permits {cmd:colorpalette} to overwrite existing files.

{pmore}
    Graph display will be disabled unless option {cmd:graph} is specified.

{marker return}{...}
{phang}
    {cmd:return(}{it:spec}{cmd:)} converts the colors to the specified color
    space and stores them as a space-separated list in macro {cmd:r(c)}. The
    syntax of {it:spec} is

{p 12 16 2}
    {it:cspace} [{cmd:,} {cmdab:f:ormat(}{it:{help format:%fmt}}{cmd:)} ]

{pmore}
    where {it:cspace} is one of {cmd:HEX}, {cmd:RGB}, {cmd:RGB1}, {cmd:lRGB},
    {cmd:HSV}, {cmd:HSL}, {cmd:CMYK}, {cmd:CMYK1}, {cmd:XYZ}, {cmd:XYZ1},
    {cmd:xyY}, {cmd:xyY1}, {cmd:Lab}, {cmd:LCh}, {cmd:Luv}, {cmd:HCL},
    {cmd:CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:}, 
    {cmd:JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:},
    {cmd:Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:}, {cmd:RGBA}, or
    {cmd:RGBA1} (lowercase spelling allowed); see help
    {helpb colrspace##cspace:colrspace} for details. Intensity adjustment
    multipliers and opacity level will be ignored when converting the colors
    (except {cmd:RGBA} and {cmd:RGBA1} that include opacity). Option {cmd:format()}
    sets the {help format:display format} for numeric values (default is
    {cmd:%9.0g}). In addition to {cmd:r(c)}, {it:cspace} is stored in
    {cmd:r(cspace)}.

{dlgtab:Graph options}

{marker title}{...}
{phang}
    {opt title(string)} specifies a custom title for the graph.

{marker nonumbers}{...}
{phang}
    {opt nonumbers} suppresses the numbers identifying the colors in the graph.

{marker gropts}{...}
{phang}
    {cmd:gropts(}{it:{help twoway_options}}{cmd:)} provides options to
    be passed through to the graph command.

{marker rows}{...}
{phang}
    {opt rows(#)} specifies the minimum number of rows in the graph (syntax 1
    only). The default is 5.

{marker names}{...}
{phang}
    {opt names} replaces the RGB values in the graph by the information found in
    {cmd:r(p#name)} (i.e. the color names), if such information is available (syntax 1 only).

{marker noinfo}{...}
{phang}
    {opt noinfo} suppresses the additional color information that is sometimes
    printed below the RGB values or color names.

{marker nograph}{...}
{phang}
    {opt nograph} suppresses the graph (syntax 1 only).

{marker graph}{...}
{phang}
    {opt graph} enforces drawing a graph even though
    {help colorpalette##macrooptions:{it:macro_options}} have been specified
    (syntax 1 only).

{marker horizontal}{...}
{phang}
    {opt horizontal} displays the palettes horizontally (syntax 2 only). This
    is the default.

{marker vertical}{...}
{phang}
    {opt vertical} displays the palettes vertically (syntax 2 only).

{marker span}{...}
{phang}
    {opt span} adjusts the size of the color fields such that each palette
    spans the full plot region, irrespective of the
    number of colors (syntax 2 only).

{marker barwidth}{...}
{phang}
    {opt barwidth(#)} sets the width of the color bars (syntax 2 only). The default is
    {cmd:barwidth(0.7)}. The available space per bar is 1 unit; specifying
    {cmd:barwidth(1)} will remove the gap between bars.

{marker labels}{...}
{phang}
    {opt labels(strlist)} provides custom labels for the palettes (syntax 2
    only). Enclose labels with spaces in double quotes.

{marker lcolor}{...}
{phang}
    {opth lcolor(colorstyle)} specifies a custom outline color (syntax 2 only).
    The default is to use the same color for the outline as for the fill.

{marker lwidth}{...}
{phang}
    {opth lwidth(linewidthstyle)} specifies a custom outline thickness (syntax 2
    only). The default is {cmd:lwidth(vthin)}.

{dlgtab:Colorcheck options}

{marker metric}{...}
{phang}
    {opt metric(metric)} selects the color difference metric to be used. {it:metric}
    can be {cmd:E76}, {cmd:E94}, {cmd:E2000}, or {cmd:Jab}; see help
    {helpb colrspace##delta:colrspace} for detail. Default is {cmd:metric(Jab)}.

{marker monoc}{...}
{phang}
    {cmd:mono}{cmd:(}[{it:#}] [{cmd:,} {it:method}]{cmd:)} determines the
    settings for grayscale conversion, where {it:#} in [0,1] specifies the
    proportion of gray (default is {cmd:mono(1)}, i.e. full conversion)
    and {it:method} selects the conversion method (default is {cmd:LCh}, see
    see help {helpb colrspace##gray:colrspace} for available methods).

{marker cvd}{...}
{phang}
    {opt cvd(#)}, with {it:#} in [0,1], sets the severity of color vision
    deficiency. Default is {cmd:cvd(1)} (maximum severity).

{marker nogrcheck}{...}
{phang}
    {cmd:nograph} suppresses the graph.

{marker sort}{...}
{phang}
    {cmd:sort}[{cmd:(}{it:spec}{cmd:)}] sorts the colors in the graph, where
    {it:spec} may be {cmdab:n:ormal} (by hue of normal vision),
    {cmdab:m:ono} (by shading of monochromacy vision), {cmdab:d:euter}
    (by hue of deuteranomaly vision), {cmdab:p:rot}
    (by hue of protanomaly vision), or {cmdab:t:rit}
    (by hue of tritanomaly vision). {cmd:sort} without argument
    is equivalent to {cmd:sort(normal)}. Sort only has an effect on how the colors
    are ordered in the graph, but not on how the colors are stored in {cmd:r()}.

{phang}
    {help colorpalette##gopts:{it:graph_options}} are graph options as for
    {cmd:colorpalette} (Syntax 2).


{marker palettes}{...}
{title:Palettes}

{marker colorlist}{...}
{dlgtab:colorlist}

{pstd}
    Instead of selecting a named color palette you can specify a
    custom list of color specifications using syntax

        [{cmd:(}]{it:colorspec} [{it:colorspec} {it:...}][{cmd:)}]

    where {it:colorspec} is

{p 8 15 2}
    [{cmd:"}]{it:color}[{cmd:%}{it:#}][*{it:#}][{cmd:"}]

{pstd}
    Parentheses around the list may be used to prevent name conflict with palette
    specifications. Color specifications containing spaces must be included in
    double quotes. Argument {cmd:%}{it:#} in {it:colorspec} sets the opacity level
    (in percent; 0 = fully transparent, 100 = fully opaque; Stata 15 required),
    {cmd:*}{it:#} sets the intensity adjustment multiplier (values between 0 and 1 make the color
    lighter; values larger than one make the color darker), and {it:color} is
    one of the following:

{p2colset 9 28 30 2}{...}
{p2col:{it:name}}color name; this includes
        official Stata's color names as listed in {help colorstyle##colorstyle:{it:colorstyle}},
        possible user additions provided through style files, as well as a large collection
        of {help colorpalette##htmlcolors:named colors} provided by {helpb colrspace}{p_end}
{p2col:{cmd:#}{it:rrggbb}}6-digit hex RGB value; white = {cmd:#FFFFFF} or {cmd:#ffffff}, navy = {cmd:#1A476F} or {cmd:#1a476f} {p_end}
{p2col:{cmd:#}{it:rgb}}3-digit abbreviated hex RGB value; white = {cmd:#FFF} or {cmd:#fff}{p_end}
{p2col:{it:# # #}}RGB value in 0-255 scaling; navy = {cmd:"26 71 111"}{p_end}
{p2col:{it:# # # #}}CMYK value in 0-255 or 0-1 scaling; navy = {cmd:"85 40 0 144"} or {cmd:".333 .157 0 .565"}{p_end}
{p2col:{it:cspace ...}}color value in one of the color spaces supported by {helpb colrspace##strinput:colrspace}; e.g., navy =
    {cmd:"XYZ 5.55 5.87 15.9"} or {cmd:"Lab 29 -.4 -27.5"} or {cmd:"Jab 30.1 -8.9 -19"} (see help
    {helpb colrspace##strinput:colrspace} for more examples)

{pstd}
    Example: navy in 5 different ways

{p 8 12 2}
        . {stata colorpalette navy "26 71 111" "85 40 0 144" "hsv 208 .766 .435" "hcl 246.6 38.8 29"}

{pstd}
    Example: hex colors (using colors from
    {browse "http://getbootstrap.com/docs/3.3":Bootstrap v3.3})

        . {stata colorpalette #337ab7 #5cb85c #5bc0de #f0ad4e #d9534f}

{pmore}
    For background information on hex colors see
    {browse "http://en.wikipedia.org/wiki/Web_colors":en.wikipedia.org/wiki/Web_colors}. Some
    websites providing collections of colors are
    {browse "http://www.w3schools.com/colors/":www.w3schools.com/colors},
    {browse "http://htmlcolorcodes.com/":htmlcolorcodes.com}, or
    {browse "http://www.color-hex.com/":www.color-hex.com}).

{pstd}
    Example: specifying opacity and intensity adjustment

        . {stata colorpalette "26 71 111%80" "26 71 111*.7" "26 71 111%80*.7"}

{marker htmlcolors}{...}
{pstd}
    Example: named colors from {helpb colrspace}

        . {stata colorpalette slate paleviolet lightsea sandy}

{pstd}
    Apart from Stata's system colors, the following named colors are supported:

{phang}
    {browse "http://www.w3schools.com/colors/colors_names.asp":140 HTML colors}:
    {cmd:AliceBlue}, {cmd:AntiqueWhite}, {cmd:Aqua}, {cmd:Aquamarine}, {cmd:Azure},
    {cmd:Beige}, {cmd:Bisque}, {cmd:Black}, {cmd:BlanchedAlmond}, {cmd:Blue},
    {cmd:BlueViolet}, {cmd:Brown}, {cmd:BurlyWood}, {cmd:CadetBlue},
    {cmd:Chartreuse}, {cmd:Chocolate}, {cmd:Coral}, {cmd:CornflowerBlue},
    {cmd:Cornsilk}, {cmd:Crimson}, {cmd:Cyan}, {cmd:DarkBlue}, {cmd:DarkCyan},
    {cmd:DarkGoldenRod}, {cmd:DarkGray}, {cmd:DarkGrey}, {cmd:DarkGreen},
    {cmd:DarkKhaki}, {cmd:DarkMagenta}, {cmd:DarkOliveGreen}, {cmd:DarkOrange},
    {cmd:DarkOrchid}, {cmd:DarkRed}, {cmd:DarkSalmon}, {cmd:DarkSeaGreen},
    {cmd:DarkSlateBlue}, {cmd:DarkSlateGray}, {cmd:DarkSlateGrey},
    {cmd:DarkTurquoise}, {cmd:DarkViolet}, {cmd:DeepPink}, {cmd:DeepSkyBlue},
    {cmd:DimGray}, {cmd:DimGrey}, {cmd:DodgerBlue}, {cmd:FireBrick},
    {cmd:FloralWhite}, {cmd:ForestGreen}, {cmd:Fuchsia}, {cmd:Gainsboro},
    {cmd:GhostWhite}, {cmd:Gold}, {cmd:GoldenRod}, {cmd:Gray}, {cmd:Grey},
    {cmd:Green}, {cmd:GreenYellow}, {cmd:HoneyDew}, {cmd:HotPink}, {cmd:IndianRed},
    {cmd:Indigo}, {cmd:Ivory}, {cmd:Khaki}, {cmd:Lavender}, {cmd:LavenderBlush},
    {cmd:LawnGreen}, {cmd:LemonChiffon}, {cmd:LightBlue}, {cmd:LightCoral},
    {cmd:LightCyan}, {cmd:LightGoldenRodYellow}, {cmd:LightGray}, {cmd:LightGrey},
    {cmd:LightGreen}, {cmd:LightPink}, {cmd:LightSalmon}, {cmd:LightSeaGreen},
    {cmd:LightSkyBlue}, {cmd:LightSlateGray}, {cmd:LightSlateGrey},
    {cmd:LightSteelBlue}, {cmd:LightYellow}, {cmd:Lime}, {cmd:LimeGreen},
    {cmd:Linen}, {cmd:Magenta}, {cmd:Maroon}, {cmd:MediumAquaMarine},
    {cmd:MediumBlue}, {cmd:MediumOrchid}, {cmd:MediumPurple}, {cmd:MediumSeaGreen},
    {cmd:MediumSlateBlue}, {cmd:MediumSpringGreen}, {cmd:MediumTurquoise},
    {cmd:MediumVioletRed}, {cmd:MidnightBlue}, {cmd:MintCream}, {cmd:MistyRose},
    {cmd:Moccasin}, {cmd:NavajoWhite}, {cmd:Navy}, {cmd:OldLace}, {cmd:Olive},
    {cmd:OliveDrab}, {cmd:Orange}, {cmd:OrangeRed}, {cmd:Orchid},
    {cmd:PaleGoldenRod}, {cmd:PaleGreen}, {cmd:PaleTurquoise}, {cmd:PaleVioletRed},
    {cmd:PapayaWhip}, {cmd:PeachPuff}, {cmd:Peru}, {cmd:Pink}, {cmd:Plum},
    {cmd:PowderBlue}, {cmd:Purple}, {cmd:RebeccaPurple}, {cmd:Red},
    {cmd:RosyBrown}, {cmd:RoyalBlue}, {cmd:SaddleBrown}, {cmd:Salmon},
    {cmd:SandyBrown}, {cmd:SeaGreen}, {cmd:SeaShell}, {cmd:Sienna}, {cmd:Silver},
    {cmd:SkyBlue}, {cmd:SlateBlue}, {cmd:SlateGray}, {cmd:SlateGrey}, {cmd:Snow},
    {cmd:SpringGreen}, {cmd:SteelBlue}, {cmd:Tan}, {cmd:Teal}, {cmd:Thistle},
    {cmd:Tomato}, {cmd:Turquoise}, {cmd:Violet}, {cmd:Wheat}, {cmd:White},
    {cmd:WhiteSmoke}, {cmd:Yellow}, {cmd:YellowGreen}

{phang}
    {browse "http://www.w3schools.com/w3css/w3css_color_material.asp":30 W3.CSS default colors}:
    {cmd:w3-red}, {cmd:w3-pink}, {cmd:w3-purple}, {cmd:w3-deep-purple},
    {cmd:w3-indigo}, {cmd:w3-blue}, {cmd:w3-light-blue}, {cmd:w3-cyan},
    {cmd:w3-aqua}, {cmd:w3-teal}, {cmd:w3-green}, {cmd:w3-light-green},
    {cmd:w3-lime}, {cmd:w3-sand}, {cmd:w3-khaki}, {cmd:w3-yellow}, {cmd:w3-amber},
    {cmd:w3-orange}, {cmd:w3-deep-orange}, {cmd:w3-blue-grey}, {cmd:w3-brown},
    {cmd:w3-light-grey}, {cmd:w3-grey}, {cmd:w3-dark-grey}, {cmd:w3-black},
    {cmd:w3-white}, {cmd:w3-pale-red}, {cmd:w3-pale-yellow}, {cmd:w3-pale-green},
    {cmd:w3-pale-blue}

{phang}
    Further color collections from W3.CSS (using names as provided by
    W3.CSS, e.g. {cmd:w3-flat-turquoise}):
    {browse "http://www.w3schools.com/w3css/w3css_color_flat.asp":Flat UI Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_metro.asp":Metro UI Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_win8.asp":Windows 8 Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_ios.asp":iOS Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Highway Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Safety Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":European Signal Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2019},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2018},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2017},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Vivid Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Food Colors},
    {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Camouflage Colors},
    {browse "http://www.w3schools.com/colors/colors_fs595.asp":ANA (Army Navy Aero) Colors}, and
    {browse "http://www.w3schools.com/colors/colors_ral.asp":Traffic Colors}.

{pstd}
    The color names can be abbreviated and typed in lowercase letters. If
    abbreviation is ambiguous, the first matching name in the alphabetically
    ordered list will be used. In case of name conflict with a Stata color, the
    color from {helpb colrspace} will take precedence only if the specified name is an exact
    match including case. For example, {cmd:pink} will refer to official
    Stata's pink, whereas {cmd:Pink} will refer to HTML color pink.

{marker st}{...}
{dlgtab:st}

{pstd}
    Palette {cmd:st} ({stata colorpalette st:{it:view}}) contains the 15 colors
    used for p1 to p15 in Stata's {helpb scheme st:stcolor} scheme. {cmd:st} is
    the default palette in Stata 18 or above.

{marker s2}{...}
{dlgtab:s2}

{pstd}
    Palette {cmd:s2} ({stata colorpalette s2:{it:view}}) contains the 15 colors
    used for p1 to p15 in Stata's {helpb scheme s2:s2color} scheme. {cmd:s2} is
    the default palette in Stata 17 or below.

{marker s1}{...}
{dlgtab:s1}

{pstd}
    Palette {cmd:s1} ({stata colorpalette s1:{it:view}}) contains the colors
    used for p1 to p15 in Stata's {helpb scheme s1:s1color} scheme.

{marker s1r}{...}
{dlgtab:s1r}

{pstd}
    Palette {cmd:s1r} ({stata colorpalette s1r:{it:view}}) contains the colors
    used for p1 to p15 in Stata's {helpb scheme s1:s1rcolor} scheme.

{marker economist}{...}
{dlgtab:economist}

{pstd}
    Palette {cmd:economist} ({stata colorpalette economist:{it:view}}) contains the colors
    used for p1 to p15 in Stata's {helpb scheme economist:economist} scheme.

{marker mono}{...}
{dlgtab:mono}

{pstd}
    Palette {cmd:mono} ({stata colorpalette mono:{it:view}}) contains the gray scales
    used for p1 to p15 in Stata's monochrome schemes ({helpb scheme s2:s2mono},
    {helpb scheme s1:s1mono}).

{marker cblind}{...}
{dlgtab:okabe and cblind}

{pstd}
    The {cmd:okabe} palette ({stata colorpalette okabe:{it:view}}) contains
    colorblind-friendly colors suggested by
    {browse "http://jfly.iam.u-tokyo.ac.jp/color/":Okabe and Ito (2002)}. The
    {cmd:cblind} palette ({stata colorpalette cblind:{it:view}}) additionally includes gray as suggested at
    {browse "http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette":www.cookbook-r.com}.
    The same colors are also used (in different order and using {cmd:gs10} for gray)
    in the {cmd:plotplainblind} and {cmd:plottigblind} schemes by
    {browse "http://www.stata-journal.com/article.html?article=gr0070":Bischof (2017b)}. Type
    {stata colorpalette cblind, select(1 2 4 5 9 8 7 3 6)} for a palette equivalent to
    the one used in {cmd:plotplainblind}, and
    {stata colorpalette okabe, select(1 3 4 8 7 6 2 5)} for {cmd:plottigblind}.

{marker plottig}{...}
{dlgtab:plottig}

{pstd}
    Palette {cmd:plottig} ({stata colorpalette plottig:{it:view}}) contains the colors
    used for p1 to p15 in the {cmd:plottig} scheme by
    {browse "http://www.stata-journal.com/article.html?article=gr0070":Bischof (2017b)}. Most of these colors
    are the same as the colors produced by the {helpb colorpalette##hue:hue}
    color generator with default options ({stata colorpalette hue:{it:view}}), although
    in different order.

{marker 538}{...}
{dlgtab:538}

{pstd}
    Palette {cmd:538} ({stata colorpalette 538:{it:view}}) contains the colors
    used for p1 to p6, background, labels, axes etc., and CIs
    in the {cmd:538} scheme by
    {browse "http://ideas.repec.org/c/boc/bocode/s458404.html":Bischof (2017a)}. The palette replicates colors
    used at {browse "http://fivethirtyeight.com":fivethirtyeight.com}.

{marker mrc}{...}
{dlgtab:mrc}

{pstd}
    Palette {cmd:mrc} ({stata colorpalette mrc:{it:view}}) contains the colors
    used for p1 to p7 in the {cmd:mrc} scheme by
    {browse "http://ideas.repec.org/c/boc/bocode/s457703.html":Morris (2013)}. These are colors
    according to guidelines by the UK Medical Research Council.

{marker tfl}{...}
{dlgtab:tfl}

{pstd}
    Palette {cmd:tfl} ({stata colorpalette tfl:{it:view}}) contains the colors
    used for p1 to p8 in the {cmd:tfl} scheme by
    {browse "http://ideas.repec.org/c/boc/bocode/s458103.html":Morris (2015)}. The palette replicates
    Transport for London's corporate colors.

{marker burd}{...}
{dlgtab:burd}

{pstd}
    Palette {cmd:burd} ({stata colorpalette burd:{it:view}}) contains the colors
    used for p1 to p9 and for CIs in the {cmd:burd} scheme by
    {browse "http://ideas.repec.org/c/boc/bocode/s457623.html":Briatte (2013)}. The p1 to p9
    colors are a selection of colors from various
    {browse "http://colorbrewer2.org/":ColorBrewer} schemes.

{marker lean}{...}
{dlgtab:lean}

{pstd}
    Palette {cmd:lean} ({stata colorpalette lean:{it:view}}) contains
    gray scales used for p1area to p15area in schemes {cmd:lean1} and {cmd:lean2}
    by {browse "http://www.stata-journal.com/article.html?article=gr0002":Juul (2003)}.

{marker tableau}{...}
{dlgtab:tableau}

{pstd}
    The {cmd:tableau} ({stata colorpalette tableau, rows(10):{it:view}}) contains 20 categorical
    colors provided by {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)}. The
    hex values of the colors have been taken from code provided by the authors at
    {browse "http://github.com/StanfordHCI/semantic-colors":GitHub}. The same colors are also used
    in {helpb colorpalette##d3:d3}.

{marker hue}{...}
{dlgtab:hue}

{pstd}
    The {cmd:hue} palette implements an algorithm that generates HCL colors with
    evenly spaced hues. The palette has been modeled after function {cmd:hue_pal()} from
    R's {cmd:scales} package by Hadley Wickham (see {browse "http://github.com/hadley/scales"}). This
    is the default color scheme used by R's {cmd:ggplot2} for categorical data
    (see {browse "http://ggplot2.tidyverse.org/reference/scale_hue.html"}). The {cmd:hue} palette
    with default options produces the same colors as the
    {cmd:intense} scheme of the {helpb colorpalette##hcl:hcl} color generator.

{pstd}
    Syntax:

{p 8 15 2}
    {cmd:hue} [{cmd:,} {opt h:ue(start end)} {opt c:hroma(#)} {opt l:uminance(#)}
        {opt dir:ection(#)} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    Options:

{phang}
    {opt hue(start end)} sets the range of hues on the 360 degree color
    wheel. The default is {bind:{cmd:hue(15 375)}}. If the difference between
    {it:start} and {it:end} is a multiple of 360, {it:end} will be reduced by
    360/n, where n is the number of requested colors (so that the space between
    the last and the first color is the same as between the other colors).

{phang}
    {opt chroma(#)} sets the colorfulness (color intensity), with
    {it:#} >= 0. The default is {cmd:chroma(100)}.

{phang}
    {opt luminance(#)} sets the brightness (amount of gray), with {it:#}
    in [0, 100]. The default is {cmd:luminance(65)}.

{phang}
    {opt direction(#)} determines the direction to travel around the color
    wheel. {cmd:direction(1)}, the default, travels clockwise; {cmd:direction(-1)}
    travels counter-clockwise.

{pstd}
    Examples:

{p 8 12 2}
        . {stata "colorpalette, span: hue, n(3) name(3) / hue, n(5) name(5) / hue, n(7) name(7) / hue, n(9) name(9)"}
        {p_end}
        . {stata colorpalette hue, n(5) hue(90 -30) chroma(50) luminance(70)}

{marker hcl}{...}
{dlgtab:HCL/LCh/JMh}

{pstd}
    The {cmd:HCL}, {cmd:LCh}, and {cmd:JMh} palettes are color generators in the
    HCL (Hue-Chroma-Luminance) space (cylindrical representation of CIE L*u*v*), the LCh space
    (cylindrical representation of CIE L*a*b*), and the CIECAM02-based J'M'h space,
    respectively. The {cmd:HCL} implementation is based on R's {cmd:colorspace} package by
    {browse "http://CRAN.R-project.org/package=colorspace":Ihaka et al. (2016)}; also
    see {browse "http://dx.doi.org/10.1016/j.csda.2008.11.033":Zeileis et al. (2009)}
    and {browse "http://hclwizard.org":hclwizard.org}. The {cmd:LCh} and
    {cmd:JMh} generators are implemented analogously.

{pstd}
    Let h1 and h2 be two hues on the 360 degree color wheel, c1 and c2 two
    chroma levels (M' in case of {cmd:JMh}), l1 and l2 two luminance levels (J' in case of
    {cmd:JMh}), p1 and p2 two power parameters, and i an index from 1 to n,
    where n is the number of requested colors. The colors are then generated
    according to the following formulas.

        Qualitative:
            H = h1 + (h2 - h1) * (i - 1) / (n - 1)
            C = c1
            L = l1

        Sequential:
            H = h2 - (h2 - h1) * j
            C = c2 - (c2 - c1) * j^p1
            L = l2 - (l2 - l1) * j^p2
            with j = (n - i) / (n - 1)

        Diverging:
            H = cond(j > 0, h1, h2)
            C = c1 * abs(j)^p1
            L = l2 - (l2 - l1) * abs(j)^p2
            with j = (n - 2*j + 1) / (n - 1)

{pstd}
    Syntax:

{p 8 15 2}
    {c -(}{cmd:HCL}|{cmd:LCh}|{cmd:JMh}{c )-} [{it:scheme}] [{cmd:,} {opt h:ue(h1 [h2])} {opt c:hroma(c1 [c2])} {opt l:uminance(l1 [l2])}
        {opt pow:er(p1 [p2])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} picks the type of color formula (qualitative, sequential,
    or diverging) and sets the default parameter values. {it:scheme} can be one of
    following ({cmd:qualitative} is the default; abbreviations are allowed).

        Qualitative {...}
(view: {...}
{stata "colorpalette, lc(white) n(9): HCL qual / HCL intense / HCL dark / HCL light / HCL pastel":{it:HCL}}, {...}
{stata "colorpalette, lc(white) n(9): LCh qual / LCh intense / LCh dark / LCh light / LCh pastel":{it:LCh}}, {...}
{stata "colorpalette, lc(white) n(9): JMh qual / JMh intense / JMh dark / JMh light / JMh pastel":{it:JMh}})
{col 28} h1              h2   c1   l1
        {stata colorpalette HCL qualitative:{bf:qualitative}}{col 28} 15  h1+360*(n-1)/n   60   70
        {stata colorpalette HCL intense:{bf:intense}}        {col 28} 15  h1+360*(n-1)/n  100   65
        {stata colorpalette HCL dark:{bf:dark}}              {col 28} 15  h1+360*(n-1)/n   80   60
        {stata colorpalette HCL light:{bf:light}}            {col 28} 15  h1+360*(n-1)/n   50   80
        {stata colorpalette HCL pastel:{bf:pastel}}          {col 28} 15  h1+360*(n-1)/n   35   85

        Sequential {...}
(view: {...}
{stata "colorpalette, n(9): HCL blues / HCL greens / HCL grays / HCL oranges / HCL purples / HCL reds / HCL heat / HCL heat2 / HCL terrain / HCL terrain2 / HCL viridis / HCL plasma / HCL redblue":{it:HCL}}, {...}
{stata "colorpalette, n(9): LCh blues / LCh greens / LCh grays / LCh oranges / LCh purples / LCh reds / LCh heat / LCh heat2 / LCh terrain / LCh terrain2 / LCh viridis / LCh plasma / LCh redblue":{it:LCh}}, {...}
{stata "colorpalette, n(9): JMh blues / JMh greens / JMh grays / JMh oranges / JMh purples / JMh reds / JMh heat / JMh heat2 / JMh terrain / JMh terrain2 / JMh viridis / JMh plasma / JMh redblue":{it:JMh}})
{col 28} h1   h2   c1   c2   l1   l2   p1   p2
        {stata colorpalette HCL sequential:{bf:sequential}}{col 28}260   h1   80   10   25   95    1   p1
        {stata colorpalette HCL blues:{bf:blues}}          {col 28}260   h1   80   10   25   95    1   p1
        {stata colorpalette HCL greens:{bf:greens}}        {col 28}145  125   80   10   25   95    1   p1
        {stata colorpalette HCL grays:{bf:grays}}          {col 28}  0   h1    0    0   15   95    1   p1
        {stata colorpalette HCL oranges:{bf:oranges}}      {col 28} 40   h1  100   10   50   95    1   p1
        {stata colorpalette HCL purples:{bf:purples}}      {col 28}280   h1   70   10   20   95    1   p1
        {stata colorpalette HCL reds:{bf:reds}}            {col 28} 10   20   80   10   25   95    1   p1
        {stata colorpalette HCL heat:{bf:heat}}            {col 28}  0   90  100   30   50   90  0.2  1.0
        {stata colorpalette HCL heat2:{bf:heat2}}          {col 28}  0   90   80   30   30   90  0.2  2.0
        {stata colorpalette HCL terrain:{bf:terrain}}      {col 28}130    0   80    0   60   95  0.1  1.0
        {stata colorpalette HCL terrain2:{bf:terrain2}}    {col 28}130   30   65    0   45   90  0.5  1.5
        {stata colorpalette HCL viridis:{bf:viridis}}      {col 28}300   75   35   95   15   90  0.8  1.2
        {stata colorpalette HCL plasma:{bf:plasma}}        {col 28}100   h1   60  100   15   95  2.0  0.9
        {stata colorpalette HCL redblue:{bf:redblue}}      {col 28}  0 -100   80   40   40   75  1.0  1.0

        Diverging {...}
(view: {...}
{stata "colorpalette, n(9): HCL bluered / HCL bluered2 / HCL bluered3 / HCL greenorange / HCL browngreen / HCL pinkgreen / HCL purplegreen":{it:HCL}}, {...}
{stata "colorpalette, n(9): LCh bluered / LCh bluered2 / LCh bluered3 / LCh greenorange / LCh browngreen / LCh pinkgreen / LCh purplegreen":{it:LCh}}, {...}
{stata "colorpalette, n(9): JMh bluered / JMh bluered2 / JMh bluered3 / JMh greenorange / JMh browngreen / JMh pinkgreen / JMh purplegreen":{it:JMh}})
{col 28} h1   h2   c1   l1   l2   p1   p2
        {stata colorpalette HCL diverging:{bf:diverging}}    {col 28}260    0   80   30   95    1   p1
        {stata colorpalette HCL bluered:{bf:bluered}}        {col 28}260    0   80   30   95    1   p1
        {stata colorpalette HCL bluered2:{bf:bluered2}}      {col 28}260    0  100   50   95    1   p1
        {stata colorpalette HCL bluered3:{bf:bluered3}}      {col 28}180  330   60   75   95    1   p1
        {stata colorpalette HCL greenorange:{bf:greenorange}}{col 28}130   45  100   70   95    1   p1
        {stata colorpalette HCL browngreen:{bf:browngreen}}  {col 28} 55  160   60   35   95    1   p1
        {stata colorpalette HCL pinkgreen:{bf:pinkgreen}}    {col 28}340  128   90   35   95    1   p1
        {stata colorpalette HCL purplegreen:{bf:purplegreen}}{col 28}300  128   60   30   95    1   p1

{pstd}
    The shown parameter values are for
    {cmd:HCL}; {cmd:LCh} and {cmd:JMh} use adjusted values such that the end points
    of the generated colors are similar to the ones generated by {cmd:HCL} (see source file
    {help colrspace_library_generators:colrspace_library_generators.sthlp}). Options are:

{phang}
    {opt hue(h1 [h2])} overrides the default values for h1 and h2. h1 and h2 are hues on
    the 360 degree color wheel.

{phang}
    {opt chroma(c1 [c2])} overrides the default values for c1 and c2, with {it:c#} >= 0. c1 and c2
    determine the colorfulness (color intensity).

{phang}
    {opt luminance(l1 [l2])} overrides the default values for l1 and l2, with {it:l#} in [0, 100]. l1 and l2
    determine the brightness.

{phang}
    {opt power(p1 [p2])} overrides the default values for p1 and p2,
    with {it:p#} > 0. p1 and p2 determine the shape of the transition between
    chroma and luminance levels. For linear transitions, set {it:p#}=1; {it:p#}>1 makes
    the transition faster, {it:p#}<1 makes the transition slower.

{marker hsv}{...}
{dlgtab:HSV/HSL}

{pstd}
    The {cmd:HSV} and {cmd:HSL} palettes are color generators in the HSV (Hue-Saturation-Value)
    and HSL (Hue-Saturation-Lightness) spaces. The
    implementation is partially based on R's {cmd:grDevices} package (which is
    part of the R core) and partially on {cmd:colorspace} by
    {browse "http://CRAN.R-project.org/package=colorspace":Ihaka et al. (2016)}. Used formulas
    are analogous to the formulas of the {helpb colorpalette##hcl:HCL} generator.

{pstd}
    Syntax:

{p 8 15 2}
    {c -(}{cmd:HSV}|{cmd:HSL}{c )-} [{it:scheme}] [{cmd:,} {opt h:ue(h1 [h2])} {opt sat:uration(s1 [s2])} {opt val:ue(v1 [v2])}
        {opt pow:er(p1 [p2])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} picks the type of color formula (qualitative, sequential,
    or diverging) and sets the default parameter values. {it:scheme} can be one of
    following ({cmd:qualitative} is the default; abbreviations are allowed).

{pstd}
    For {cmd:HSV}, {it:scheme} can be one of the following.

{p2colset 9 29 30 2}{...}
{p2col:Qualitative ({stata "colorpalette, lc(white) n(9): hsv qual / hsv intense / hsv dark / hsv light / hsv pastel / hsv rainbow":{it:view}}):}
{stata colorpalette HSV qualitative:{bf:qualitative}},
{stata colorpalette HSV intense:{bf:intense}},
{stata colorpalette HSV dark:{bf:dark}},
{stata colorpalette HSV light:{bf:light}},
{stata colorpalette HSV pastel:{bf:pastel}},
{stata colorpalette HSV rainbow:{bf:rainbow}}
{p_end}
{p2col:Sequential ({stata "colorpalette, n(9): hsv blues / hsv greens / hsv grays / hsv oranges / hsv purples / hsv reds / hsv heat / hsv terrain / hsv heat0 / hsv terrain0":{it:view}}):}
{stata colorpalette HSV sequential:{bf:sequential}},
{stata colorpalette HSV blues:{bf:blues}},
{stata colorpalette HSV greens:{bf:greens}},
{stata colorpalette HSV grays:{bf:grays}},
{stata colorpalette HSV oranges:{bf:oranges}},
{stata colorpalette HSV purples:{bf:purples}},
{stata colorpalette HSV reds:{bf:reds}},
{stata colorpalette HSV heat:{bf:heat}},
{stata colorpalette HSV terrain:{bf:terrain}},
{stata colorpalette HSV heat0:{bf:heat0}},
{stata colorpalette HSV terrain0:{bf:terrain0}}
{p_end}
{p2col:Diverging ({stata "colorpalette, n(9): hsv bluered / hsv bluered2 / hsv bluered3 / hsv greenorange / hsv browngreen / hsv pinkgreen / hsv purplegreen":{it:view}}):}
{stata colorpalette HSV diverging:{bf:diverging}},
{stata colorpalette HSV bluered:{bf:bluered}},
{stata colorpalette HSV bluered2:{bf:bluered2}},
{stata colorpalette HSV bluered3:{bf:bluered3}},
{stata colorpalette HSV greenorange:{bf:greenorange}},
{stata colorpalette HSV browngreen:{bf:browngreen}},
{stata colorpalette HSV pinkgreen:{bf:pinkgreen}},
{stata colorpalette HSV purplegreen:{bf:purplegreen}}

{pstd}
    For {cmd:HSL}, {it:scheme} can be: {stata colorpalette HSL qualitative:{bf:qualitative}},
    {stata colorpalette HSL sequential:{bf:sequential}}, or
    {stata colorpalette HSL diverging:{bf:diverging}}

{pstd}
    Options are:

{phang}
    {opt h:ue(h1 [h2])} overrides the default values for h1 and h2. h1 and h2
    are hues on the 360 degree color wheel.

{phang}
    {opt sat:uration(c1 [c2])} overrides the default values for
    c1 and c2, with {it:c#} in [0, 1]. c1 and c2 determine the colorfulness
    (color intensity).

{phang}
    {opt val:ue(l1 [l2])} overrides the default values for l1 and l2, with
    {it:l#} in [0, 1]. l1 and l2 determine the brightness.

{phang}
    {opt power(p1 [p2])} overrides the default values for p1 and p2,
    with {it:p#} > 0. p1 and p2 determine the shape of the transition between
    saturation and value levels. For linear transitions, set {it:p#}=1; {it:p#}>1 makes
    the transition faster, {it:p#}<1 makes the transition slower.

{pstd}
    See source file
    {help colrspace_library_generators:colrspace_library_generators.sthlp} for default
    parameter values of the above schemes.

{marker pals}{...}
{dlgtab:pals}

{pstd}
    The {it:pals} collection provides some categorical color schemes that have been
    obtained from the {bf:pals} package in R; see
    {browse "http://github.com/kwstat/pals":github.com/kwstat/pals}. The
    syntax is

{p 8 15 2}
    {it:scheme} [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette: alphabet / alphabet2 / cols25 / glasbey / kelly / polychrome / watlington":{it:view}}).

{p2colset 9 24 26 2}{...}
{p2col:{stata colorpalette alphabet:{bf:alphabet}}}26 colors{p_end}
{p2col:{stata colorpalette alphabet2:{bf:alphabet2}}}26 colors{p_end}
{p2col:{stata colorpalette cols25:{bf:cols25}}}25 colors{p_end}
{p2col:{stata colorpalette glasbey:{bf:glasbey}}}32 colors{p_end}
{p2col:{stata colorpalette kelly:{bf:kelly}}}22 colors{p_end}
{p2col:{stata colorpalette polychrome:{bf:polychrome}}}36 colors{p_end}
{p2col:{stata colorpalette watlington:{bf:watlington}}}16 colors{p_end}

{marker d3}{...}
{dlgtab:d3}

{pstd}
    The {cmd:d3} collection provides categorical color schemes from
    {browse "http://d3js.org/":D3.js}, using the color values found at
    {browse "http://github.com/d3/d3-scale/blob/master/README.md#category-scales":github.com/d3}. The
    syntax is

{p 8 15 2}
    {cmd:d3} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following
    ({stata "colorpalette: d3 10 / d3 20 / d3 20b / d3 20c":{it:view}}).

{p2colset 9 16 18 2}{...}
{p2col:{stata colorpalette d3 10:{bf:10}}}10 categorical colors; the default; same as first 10 colors in {helpb colorpalette##tableau:tableau}{p_end}
{p2col:{stata colorpalette d3 20:{bf:20}}}20 categorical colors in pairs; same colors as in {helpb colorpalette##tableau:tableau}, but ordered differently{p_end}
{p2col:{stata colorpalette d3 20b:{bf:20b}}}20 categorical colors in groups of four{p_end}
{p2col:{stata colorpalette d3 20c:{bf:20c}}}20 categorical colors in groups of four{p_end}

{pstd}
    These colors appear to be colors that have been used in earlier versions of
    {browse "http://www.tableau.com/":Tableau}. The four palettes are thus also
    available as {stata colorpalette tab10:{bf:tab10}}, {stata colorpalette tab20:{bf:tab20}},
    {stata colorpalette tab20b:{bf:tab20b}}, and {stata colorpalette tab20c:{bf:tab20c}}.

{marker sb}{...}
{dlgtab:sb}

{pstd}
    The {cmd:sb} collection provides categorical color schemes from
    {browse "http://seaborn.pydata.org/":seaborn.pydata.org} (same basic colors as
    {stata colorpalette tab10:{bf:tab10}}, but in different tones). The
    syntax is

{p 8 15 2}
    {cmd:sb} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed).

{p2colset 9 30 31 2}{...}
{p2col:10 colors ({stata "colorpalette: sb deep / sb muted / sb pastel / sb bright / sb dark / sb colorblind":{it:view}})}
    6 colors ({stata "colorpalette: sb deep6 / sb muted6 / sb pastel6 / sb bright6 / sb dark6 / sb colorblind6":{it:view}}){p_end}
{p2col:{hline 18}} {hline 38}{p_end}
{p2col:{stata colorpalette sb deep:{bf:deep}} (default)}   {stata colorpalette sb deep6:{bf:deep6}} (also available as palette {stata colorpalette sb6:{bf:sb6}}){p_end}
{p2col:{stata colorpalette sb muted:{bf:muted}}}           {stata colorpalette sb muted6:{bf:muted6}}{p_end}
{p2col:{stata colorpalette sb pastel:{bf:pastel}}}         {stata colorpalette sb pastel6:{bf:pastel6}}{p_end}
{p2col:{stata colorpalette sb bright:{bf:bright}}}         {stata colorpalette sb bright6:{bf:bright6}}{p_end}
{p2col:{stata colorpalette sb dark:{bf:dark}}}             {stata colorpalette sb dark6:{bf:dark6}}{p_end}
{p2col:{stata colorpalette sb colorblind:{bf:colorblind}}} {stata colorpalette sb colorblind6:{bf:colorblind6}}{p_end}

{marker tab}{...}
{dlgtab:tab}

{pstd}
    The {cmd:tab} collection provides various color schemes from
    {browse "http://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782":Tableau 10}
    (the color values have been obtained from the {browse "http://github.com/jrnold/ggthemes/":{bf:ggthemes}}
    package in R). The syntax is

{p 8 15 2}
    {cmd:tab} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case
    spelling allowed).

{p2colset 9 30 32 2}{...}
{p2col:Qualitative}
({stata "colorpalette: tab 10 / tab 20 / tab Color Blind / tab Seattle Grays / tab Traffic / tab Miller Stone / tab Superfishel Stone / tab Nuriel Stone":{it:view 1-8}})
({stata "colorpalette: tab Jewel Bright / tab Summer / tab Winter / tab Green-Orange-Teal / tab Red-Blue-Brown / tab Purple-Pink-Gray / tab Hue Circle":{it:view 9-15}})
{p_end}
{p2col:{stata colorpalette tab 10:{bf:10}}} 10 colors; the default{p_end}
{p2col:{stata colorpalette tab 20:{bf:20}}} 10 colors{p_end}
{p2col:{stata colorpalette tab Color Blind:{bf:Color Blind}}} 10 colors{p_end}
{p2col:{stata colorpalette tab Seattle Grays:{bf:Seattle Grays}}} 5 colors{p_end}
{p2col:{stata colorpalette tab Traffic:{bf:Traffic}}} 9 colors{p_end}
{p2col:{stata colorpalette tab Miller Stone:{bf:Miller Stone}}} 11 colors{p_end}
{p2col:{stata colorpalette tab Superfishel Stone:{bf:Superfishel Stone}}} 10 colors{p_end}
{p2col:{stata colorpalette tab Nuriel Stone:{bf:Nuriel Stone}}} 9 colors{p_end}
{p2col:{stata colorpalette tab Jewel Bright:{bf:Jewel Bright}}} 9 colors{p_end}
{p2col:{stata colorpalette tab Summer:{bf:Summer}}} 8 colors{p_end}
{p2col:{stata colorpalette tab Winter:{bf:Winter}}} 10 colors{p_end}
{p2col:{stata colorpalette tab Green-Orange-Teal:{bf:Green-Orange-Teal}}} 12 colors{p_end}
{p2col:{stata colorpalette tab Red-Blue-Brown:{bf:Red-Blue-Brown}}} 12 colors{p_end}
{p2col:{stata colorpalette tab Purple-Pink-Gray:{bf:Purple-Pink-Gray}}} 12 colors{p_end}
{p2col:{stata colorpalette tab Hue Circle:{bf:Hue Circle}}} 19 colors{p_end}

{p2col:Sequential}
({stata "colorpalette: tab Blue-Green / tab Blue Light / tab Orange Light / tab Blue / tab Orange / tab Green / tab Red / tab Purple":{it:view 1-8}})
({stata "colorpalette: tab Brown / tab Gray / tab Gray Warm / tab Blue-Teal / tab Orange-Gold / tab Green-Gold / tab Red-Gold":{it:view 9-15}})
{p_end}
{p2col:{stata colorpalette tab Blue-Green:{bf:Blue-Green}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Blue Light:{bf:Blue Light}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Orange Light:{bf:Orange Light}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Blue:{bf:Blue}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Orange:{bf:Orange}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Green:{bf:Green}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Red:{bf:Red}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Purple:{bf:Purple}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Brown:{bf:Brown}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Gray:{bf:Gray}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Gray Warm:{bf:Gray Warm}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Blue-Teal:{bf:Blue-Teal}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Orange-Gold:{bf:Orange-Gold}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Green-Gold:{bf:Green-Gold}}} 20 colors{p_end}
{p2col:{stata colorpalette tab Red-Gold:{bf:Red-Gold}}} 21 colors{p_end}

{p2col:Diverging}
({stata "colorpalette: tab Orange-Blue / tab Red-Green / tab Green-Blue / tab Red-Blue / tab Red-Black / tab Gold-Purple / tab Red-Green-Go / tab Sunset-Sunrise":{it:view 1-8}})
({stata "colorpalette: tab Orange-Blue-White / tab Red-Green-White / tab Green-Blue-White / tab Red-Blue-White / tab Red-Black-White / tab Orange-Blue Light / tab Temperature":{it:view 9-15}})
{p_end}
{p2col:{stata colorpalette tab Orange-Blue:{bf:Orange-Blue}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Green:{bf:Red-Green}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Green-Blue:{bf:Green-Blue}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Blue:{bf:Red-Blue}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Black:{bf:Red-Black}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Gold-Purple:{bf:Gold-Purple}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Green-Gold:{bf:Red-Green-Gold}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Sunset-Sunrise:{bf:Sunset-Sunrise}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Orange-Blue-White:{bf:Orange-Blue-White}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Green-White:{bf:Red-Green-White}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Green-Blue-White:{bf:Green-Blue-White}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Blue-White:{bf:Red-Blue-White}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Red-Black-White:{bf:Red-Black-White}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Orange-Blue Light:{bf:Orange-Blue Light}}} 7 colors{p_end}
{p2col:{stata colorpalette tab Temperature:{bf:Temperature}}} 7 colors{p_end}

{marker tol}{...}
{dlgtab:tol}

{pstd}
    The {cmd:tol} collection provides various color schemes presented by Paul Tol
    at {browse "http://personal.sron.nl/~pault/":personal.sron.nl/~pault}. The syntax is

{p 8 15 2}
    {cmd:tol} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case
    spelling allowed).

{p2colset 9 30 32 2}{...}
{p2col:Qualitative ({stata "colorpalette: tol bright / tol high-contrast / tol vibrant / tol muted / tol medium-contrast / tol light":{it:view}})}{p_end}
{p2col:{stata colorpalette tol bright:{bf:bright}}} 8 colors{p_end}
{p2col:{stata colorpalette tol high-contrast:{bf:high-contrast}}} 4 colors{p_end}
{p2col:{stata colorpalette tol vibrant:{bf:vibrant}}} 8 colors{p_end}
{p2col:{stata colorpalette tol muted:{bf:muted}}} 11 colors; the default{p_end}
{p2col:{stata colorpalette tol medium-contrast:{bf:medium-contrast}}} 7 colors{p_end}
{p2col:{stata colorpalette tol light:{bf:light}}} 10 colors{p_end}

{p2col:Sequential ({stata "colorpalette: tol YlOrBr / tol iridescent":{it:view}})}{p_end}
{p2col:{stata colorpalette tol YlOrBr:{bf:YlOrBr}}} 9 colors{p_end}
{p2col:{stata colorpalette tol iridescent:{bf:iridescent}}} 23 colors{p_end}

{p2col:Rainbow ({stata "colorpalette: tol rainbow / tol PuRd / tol PuBr / tol WhRd / tol WhBr":{it:view}})}{p_end}
{p2col:{stata colorpalette tol rainbow:{bf:rainbow}}} 1-23 colors{p_end}
{p2col:{stata colorpalette tol PuRd:{bf:PuRd}}} 22 colors{p_end}
{p2col:{stata colorpalette tol PuBr:{bf:PuBr}}} 26 colors{p_end}
{p2col:{stata colorpalette tol WhRd:{bf:WhRd}}} 30 colors{p_end}
{p2col:{stata colorpalette tol WhBr:{bf:WhBr}}} 34 colors{p_end}

{p2col:Diverging ({stata "colorpalette: tol sunset / tol BuRd / tol PRGn":{it:view}})}{p_end}
{p2col:{stata colorpalette tol sunset:{bf:sunset}}} 11 colors{p_end}
{p2col:{stata colorpalette tol BuRd:{bf:BuRd}}} 9 colors{p_end}
{p2col:{stata colorpalette tol PRGn:{bf:PRGn}}} 9 colors{p_end}

{pstd}
    The definitions of the schemes have been obtained from source file
    {browse "http://personal.sron.nl/~pault/data/tol_colors.py":tol_colors.py}. These
    definitions may deviate from how the palettes are presented at
    {browse "http://personal.sron.nl/~pault/":personal.sron.nl/~pault} (e.g.,
    with respect to the order of colors in the qualitative schemes).

{marker ptol}{...}
{dlgtab:ptol}

{pstd}
    The {cmd:ptol} collection provides color schemes suggested by
    {browse "http://personal.sron.nl/~pault/colourschemes.pdf":Tol (2012)}. The
    syntax is

{p 8 15 2}
    {cmd:ptol} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette: ptol, n(3) / ptol, n(5) / ptol, n(9) / ptol rain, n(4) / ptol rain, n(6) / ptol rain, n(8) / ptol div, n(3) / ptol div, n(5) / ptol div, n(9)":{it:view}}).

{p2colset 9 24 26 2}{...}
{p2col:{stata colorpalette ptol qualitative:{bf:qualitative}}}1-12 qualitative colors; the default{p_end}
{p2col:{stata colorpalette ptol rainbow:{bf:rainbow}}}4-12 rainbow colors{p_end}
{p2col:{stata colorpalette ptol diverging:{bf:diverging}}}3-11 diverging colors; very similar to reverse {helpb colorpalette##cbrew:RdYlBu}{p_end}

{marker cbrew}{...}
{dlgtab:ColorBrewer}

{pstd}
    ColorBrewer is a set of color schemes developed by Brewer et al. (2003; also
    see Brewer 2016). The colors are licensed under Apache License Version 2.0; see
    the copyright notes at
    {browse "http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html":ColorBrewer_updates.html}. For
    more information on ColorBrewer also see {browse "http://colorbrewer2.org/"}. The
    RGB values for the implementation of the colors in {cmd:colorpalette}
    have been taken from the Excel spreadsheet provided at
    {browse "http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_RGB.html":ColorBrewer_RGB.html}.
    The CMYK values have been taken from file cb.csv provided at
    {browse "http://github.com/axismaps/colorbrewer/":GitHub}. ColorBrewer palettes for Stata
    are also provided by {browse "http://github.com/matthieugomez/stata-colorscheme":Gomez (2015)}
    and by {browse "http://ideas.repec.org/c/boc/bocode/s458050.html":Buchanan (2015)}.

{pstd}The syntax for the ColorBrewer palettes is

{p 8 15 2}
    {it:scheme} [{cmd:cmyk}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case
    spelling allowed)

{p2colset 9 22 24 2}{...}
{p2col:Qualitative ({stata "colorpalette, lc(black): Accent / Dark2 / Paired / Pastel1 / Pastel2 / Set1 / Set2 / Set3":{it:view}})}{p_end}
{p2col:{stata colorpalette Accent:{bf:Accent}}}8 accented colors for qualitative data{p_end}
{p2col:{stata colorpalette Dark2:{bf:Dark2}}}8 dark colors for qualitative data{p_end}
{p2col:{stata colorpalette Paired:{bf:Paired}}}12 paired colors for qualitative data{p_end}
{p2col:{stata colorpalette Pastel1:{bf:Pastel1}}}9 pastel colors for qualitative data{p_end}
{p2col:{stata colorpalette Pastel2:{bf:Pastel2}}}8 pastel colors for qualitative data{p_end}
{p2col:{stata colorpalette Set1:{bf:Set1}}}9 colors for qualitative data{p_end}
{p2col:{stata colorpalette Set2:{bf:Set2}}}8 colors for qualitative data{p_end}
{p2col:{stata colorpalette Set3:{bf:Set3}}}12 colors for qualitative data{p_end}

{p2col:Sequential (single hue) ({stata "colorpalette: Blues / Greens / Greys / Oranges / Purples / Reds":{it:view}})}{p_end}
{p2col:{stata colorpalette Blues:{bf:Blues}}}light blue to blue (3-9 colors){p_end}
{p2col:{stata colorpalette Greens:{bf:Greens}}}light green to green (3-9 colors){p_end}
{p2col:{stata colorpalette Greys:{bf:Greys}}}light gray to gray (3-9 colors){p_end}
{p2col:{stata colorpalette Oranges:{bf:Oranges}}}light orange to orange (3-9 colors){p_end}
{p2col:{stata colorpalette Purples:{bf:Purples}}}light purple to purple (3-9 colors){p_end}
{p2col:{stata colorpalette Reds:{bf:Reds}}}light red to red (3-9 colors){p_end}

{p2col:Sequential (multi-hue) ({stata "colorpalette: BuGn / PuBu / GnBu / OrRd / PuBu / PuBuGn / PuRd / RdPu / YlGn / YlGnBu / YlOrBr / YlOrRd":{it:view}})}{p_end}
{p2col:{stata colorpalette BuGn:{bf:BuGn}}}light blue to green (3-9 colors){p_end}
{p2col:{stata colorpalette BuPu:{bf:BuPu}}}light blue to purple (3-9 colors){p_end}
{p2col:{stata colorpalette GnBu:{bf:GnBu}}}light green to blue (3-9 colors){p_end}
{p2col:{stata colorpalette OrRd:{bf:OrRd}}}light orange to red (3-9 colors){p_end}
{p2col:{stata colorpalette PuBu:{bf:PuBu}}}light purple to blue (3-9 colors){p_end}
{p2col:{stata colorpalette PuBuGn:{bf:PuBuGn}}}light purple over blue to green (3-9 colors){p_end}
{p2col:{stata colorpalette PuRd:{bf:PuRd}}}light purple to red (3-9 colors){p_end}
{p2col:{stata colorpalette RdPu:{bf:RdPu}}}light red to purple (3-9 colors){p_end}
{p2col:{stata colorpalette YlGn:{bf:YlGn}}}light yellow to green (3-9 colors){p_end}
{p2col:{stata colorpalette YlGnBu:{bf:YlGnBu}}}light yellow over green to blue (3-9 colors){p_end}
{p2col:{stata colorpalette YlOrBr:{bf:YlOrBr}}}light yellow over orange to brown (3-9 colors){p_end}
{p2col:{stata colorpalette YlOrRd:{bf:YlOrRd}}}light yellow over orange to red (3-9 colors){p_end}

{p2col:Diverging ({stata "colorpalette: BrBG / PiYG / PRGn / PuOr / RdBu / RdGy / RdYlBu / RdYlGn / Spectral":{it:view}})}{p_end}
{p2col:{stata colorpalette BrBG:{bf:BrBG}}}brown to green, light gray mid (3-11 colors){p_end}
{p2col:{stata colorpalette PiYG:{bf:PiYG}}}pink to green, light gray mid (3-11 colors){p_end}
{p2col:{stata colorpalette PRGn:{bf:PRGn}}}purple to green, light gray mid (3-11 colors){p_end}
{p2col:{stata colorpalette PuOr:{bf:PuOr}}}orange to purple, light gray mid (3-11 colors){p_end}
{p2col:{stata colorpalette RdBu:{bf:RdBu}}}red to blue, light gray mid (3-11 colors){p_end}
{p2col:{stata colorpalette RdGy:{bf:RdGy}}}red to gray, white mid (3-11 colors){p_end}
{p2col:{stata colorpalette RdYlBu:{bf:RdYlBu}}}red to blue, yellow mid (3-11 colors){p_end}
{p2col:{stata colorpalette RdYlGn:{bf:RdYlGn}}}red to green, yellow mid (3-11 colors){p_end}
{p2col:{stata colorpalette Spectral:{bf:Spectral}}}red - orange - yellow - green - blue (3-11 colors){p_end}

{phang}
    and argument {cmd:cmyk} requests the CMYK variant of the scheme. The default is to use
    the RGB variant.

{marker carto}{...}
{dlgtab:carto}

{pstd}
    The {cmd:carto} collection provides various color schemes from
    {browse "http://carto.com/carto-colors/":Carto}. The syntax is

{p 8 15 2}
    {cmd:carto} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case
    spelling allowed).

{pmore}
    Qualitative (2-11 colors, plus one color for missing data) ({stata "colorpalette: carto Antique / carto Bold / carto Pastel / carto Prism / carto Safe / carto Vivid":{it:view}})
    {p_end}
{pmore}
    {stata colorpalette carto Antique:{bf:Antique}},
    {stata colorpalette carto Bold:{bf:Bold}} (the default),
    {stata colorpalette carto Pastel:{bf:Pastel}},
    {stata colorpalette carto Prism:{bf:Prism}},
    {stata colorpalette carto Safe:{bf:Safe}} (colorblind-friendly),
    {stata colorpalette carto Vivid:{bf:Vivid}}

{pmore}
    Sequential (2-7 colors)
    ({stata "colorpalette: carto Burg / carto BurgYl / carto RedOr / carto OrYel / carto Peach / carto PinkYl / carto Mint / carto BluGrn / carto DarkMint / carto Emrld / carto ag_GrnYl":{it:view 1-11}})
    ({stata "colorpalette: carto BluYl / carto Teal / carto TealGrn / carto Purp / carto PurpOr / carto Sunset / carto Magenta / carto SunsetDark / carto ag_Sunset / carto BrwnYl":{it:view 12-21}})
    {p_end}
{pmore}
    {stata colorpalette carto Burg:{bf:Burg}},
    {stata colorpalette carto BurgYl:{bf:BurgYl}},
    {stata colorpalette carto RedOr:{bf:RedOr}},
    {stata colorpalette carto OrYel:{bf:OrYel}},
    {stata colorpalette carto Peach:{bf:Peach}},
    {stata colorpalette carto PinkYl:{bf:PinkYl}},
    {stata colorpalette carto Mint:{bf:Mint}},
    {stata colorpalette carto BluGrn:{bf:BluGrn}},
    {stata colorpalette carto DarkMint:{bf:DarkMint}},
    {stata colorpalette carto Emrld:{bf:Emrld}},
    {stata colorpalette carto ag_GrnYl:{bf:ag_GrnYl}},
    {stata colorpalette carto BluYl:{bf:BluYl}},
    {stata colorpalette carto Teal:{bf:Teal}},
    {stata colorpalette carto TealGrn:{bf:TealGrn}},
    {stata colorpalette carto Purp:{bf:Purp}},
    {stata colorpalette carto PurpOr:{bf:PurpOr}},
    {stata colorpalette carto Sunset:{bf:Sunset}},
    {stata colorpalette carto Magenta:{bf:Magenta}},
    {stata colorpalette carto SunsetDark:{bf:SunsetDark}},
    {stata colorpalette carto ag_Sunset:{bf:ag_Sunset}},
    {stata colorpalette carto BrwnYl:{bf:BrwnYl}}

{pmore}
    Diverging (2-7 colors) ({stata "colorpalette: carto ArmyRose / carto Fall / carto Geyser / carto Temps / carto TealRose / carto Tropic / carto Earth":{it:view}})
    {p_end}
{pmore}
    {stata colorpalette carto ArmyRose:{bf:ArmyRose}},
    {stata colorpalette carto Fall:{bf:Fall}},
    {stata colorpalette carto Geyser:{bf:Geyser}},
    {stata colorpalette carto Temps:{bf:Temps}},
    {stata colorpalette carto TealRose:{bf:TealRose}},
    {stata colorpalette carto Tropic:{bf:Tropic}},
    {stata colorpalette carto Earth:{bf:Earth}}

{marker viridis}{...}
{dlgtab:viridis}

{pstd}
    The {it:viridis} collection provides perceptually uniform colormaps from
    {browse "http://matplotlib.org":matplotlib} (also see
    {browse "http://bids.github.io/colormap/":bids.github.io/colormap}). The
    color values have been taken from file
    {browse "http://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm_listed.py":_cm_listed.py}
    at GitHub.

{pstd}The syntax for the viridis palettes is

{p 8 15 2}
    {it:scheme} [{cmd:,} {opt ra:nge(lb [ub])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette: viridis / plasma / inferno / magma / cividis / twilight / twilight shifted":{it:view}})

{p2colset 9 27 29 2}{...}
{p2col:{stata colorpalette viridis:{bf:viridis}}}blue - green - yellow (sequential){p_end}
{p2col:{stata colorpalette plasma:{bf:plasma}}}blue - red - yellow (sequential){p_end}
{p2col:{stata colorpalette inferno:{bf:inferno}}}black - blue - orange - yellow (sequential){p_end}
{p2col:{stata colorpalette magma:{bf:magma}}}black - blue - red - yellow (sequential){p_end}
{p2col:{stata colorpalette cividis:{bf:cividis}}}blue - olive - yellow (sequential, colorblind-friendly){p_end}
{p2col:{stata colorpalette twilight:{bf:twilight}}}blue - russet (cyclic){p_end}
{p2col:{stata colorpalette twilight shifted:{bf:twilight shifted}}}blue - russet (cyclic){p_end}

{pstd}
    and option {opt range(lb [ub])} selects the range of the colormap to be used, where {it:lb} and {it:ub}
    must be in [0,1]. The default is {cmd:range(0 1)}. If {it:lb} is larger than
    {it:ub}, the colors are returned in reverse order. {cmd:range()} has no
    effect for cyclic colormaps.

{marker seaborn}{...}
{dlgtab:seaborn}

{pstd}
    The {it:seaborn} collection provides perceptually uniform colormaps from
    {browse "http://seaborn.pydata.org/":seaborn.pydata.org}.
    The syntax is

{p 8 15 2}
    {it:scheme} [{cmd:,} {opt ra:nge(lb [ub])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette: rocket / mako / flare / crest / vlag / icefire":{it:view}})

{p2colset 9 22 24 2}{...}
{p2col:{stata colorpalette rocket:{bf:rocket}}}sequential{p_end}
{p2col:{stata colorpalette mako:{bf:mako}}}sequential{p_end}
{p2col:{stata colorpalette flare:{bf:flare}}}sequential{p_end}
{p2col:{stata colorpalette crest:{bf:crest}}}sequential{p_end}
{p2col:{stata colorpalette vlag:{bf:vlag}}}diverging{p_end}
{p2col:{stata colorpalette icefire:{bf:icefire}}}diverging{p_end}

{pstd}
    and option {opt range(lb [ub])} selects the range of the colormap to be used, where {it:lb} and {it:ub}
    must be in [0,1]. The default is {cmd:range(0 1)}. If {it:lb} is larger than
    {it:ub}, the colors are returned in reverse order.

{marker matplotlib}{...}
{dlgtab:matplotlib}

{pstd}
    The {cmd:matplotlib} collection provides several colormaps from
    {browse "http://matplotlib.org":matplotlib} (Hunter 2007). The
    definitions of the colormaps have been taken from file
    {browse "http://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py":_cm.py}
    at GitHub.

{pstd}The syntax for the {cmd:matplotlib} collection is

{p 8 15 2}
    {cmdab:matplotlib} [{it:scheme}] [{cmd:,} {opt ra:nge(lb [ub])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette: matplot jet / matplot autumn / matplot spring / matplot summer / matplot winter / matplot bone / matplot cool / matplot copper / matplot coolwarm / matplot hot / matplot turbo":{it:view}})

{p2colset 9 22 24 2}{...}
{p2col:{stata colorpalette matplotlib jet:{bf:jet}}}blue - green - yellow - red; the default{p_end}
{p2col:{stata colorpalette matplotlib autumn:{bf:autumn}}}red - yellow{p_end}
{p2col:{stata colorpalette matplotlib spring:{bf:spring}}}magenta - yellow{p_end}
{p2col:{stata colorpalette matplotlib summer:{bf:summer}}}green - yellow{p_end}
{p2col:{stata colorpalette matplotlib winter:{bf:winter}}}blue - lime{p_end}
{p2col:{stata colorpalette matplotlib bone:{bf:bone}}}black - bluish gray - white{p_end}
{p2col:{stata colorpalette matplotlib cool:{bf:cool}}}cyan - magenta{p_end}
{p2col:{stata colorpalette matplotlib copper:{bf:copper}}}black - orange/brown{p_end}
{p2col:{stata colorpalette matplotlib coolwarm:{bf:coolwarm}}}blue - red (diverging){p_end}
{p2col:{stata colorpalette matplotlib hot:{bf:hot}}}heat colors{p_end}
{p2col:{stata colorpalette matplotlib turbo:{bf:turbo}}}similar to {cmd:jet}{p_end}

{pstd}
    and option {opt range(lb [ub])} selects the range of the colormap to be used, where {it:lb} and {it:ub}
    must be in [0,1]. The default is {cmd:range(0 1)}. If {it:lb} is larger than
    {it:ub}, the colors are returned in reverse order.

{marker CET}{...}
{dlgtab:CET}

{pstd}
    The {cmd:CET} collection provides perceptually uniform colormaps by
    {browse "http://arxiv.org/abs/1509.03700":Kovesi (2015)}. The syntax is

{p 8 15 2}
    {cmd:CET} {it:scheme} [{cmd:,} {opt ra:nge(lb [ub])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case spelling allowed)
    (see {browse "http://colorcet.com/gallery.html":colorcet.com/gallery.html}
    for an overview)

{p2colset 9 30 31 2}{...}
{p2col:Linear:}
    {stata colorpalette CET L01:{bf:L01}},
    {stata colorpalette CET L02:{bf:L02}},
    {stata colorpalette CET L03:{bf:L03}},
    {stata colorpalette CET L04:{bf:L04}},
    {stata colorpalette CET L05:{bf:L05}},
    {stata colorpalette CET L06:{bf:L06}},
    {stata colorpalette CET L07:{bf:L07}},
    {stata colorpalette CET L08:{bf:L08}},
    {stata colorpalette CET L09:{bf:L09}},
    {stata colorpalette CET L10:{bf:L10}},
    {stata colorpalette CET L11:{bf:L11}},
    {stata colorpalette CET L12:{bf:L12}},
    {stata colorpalette CET L13:{bf:L13}},
    {stata colorpalette CET L14:{bf:L14}},
    {stata colorpalette CET L15:{bf:L15}},
    {stata colorpalette CET L16:{bf:L16}},
    {stata colorpalette CET L17:{bf:L17}},
    {stata colorpalette CET L18:{bf:L18}},
    {stata colorpalette CET L19:{bf:L19}},
    {stata colorpalette CET L20:{bf:L20}} (default)
    {p_end}
{p2col:Rainbow:}
    {stata colorpalette CET R1:{bf:R1}},
    {stata colorpalette CET R2:{bf:R2}},
    {stata colorpalette CET R3:{bf:R3}},
    {stata colorpalette CET R4:{bf:R4}}
    {p_end}
{p2col:Isoluminant:}
    {stata colorpalette CET I1:{bf:I1}},
    {stata colorpalette CET I2:{bf:I2}},
    {stata colorpalette CET I3:{bf:I3}}
    {p_end}
{p2col:Diverging:}
    {stata colorpalette CET D01:{bf:D01}},
    {stata colorpalette CET D01A:{bf:D01A}},
    {stata colorpalette CET D02:{bf:D02}},
    {stata colorpalette CET D03:{bf:D03}},
    {stata colorpalette CET D04:{bf:D04}},
    {stata colorpalette CET D06:{bf:D06}},
    {stata colorpalette CET D07:{bf:D07}},
    {stata colorpalette CET D08:{bf:D08}},
    {stata colorpalette CET D09:{bf:D09}},
    {stata colorpalette CET D10:{bf:D10}},
    {stata colorpalette CET D11:{bf:D11}},
    {stata colorpalette CET D12:{bf:D12}},
    {stata colorpalette CET D13:{bf:D13}}
    {p_end}
{p2col:Circular:}
    {stata colorpalette CET C1:{bf:C1}},
    {stata colorpalette CET C2:{bf:C2}},
    {stata colorpalette CET C3:{bf:C3}},
    {stata colorpalette CET C4:{bf:C4}},
    {stata colorpalette CET C5:{bf:C5}},
    {stata colorpalette CET C6:{bf:C6}},
    {stata colorpalette CET C7:{bf:C7}}
    {p_end}
{p2col:Colorblind-friendly:}
    {stata colorpalette CET CBD1:{bf:CBD1}},
    {stata colorpalette CET CBL1:{bf:CBL1}},
    {stata colorpalette CET CBL2:{bf:CBL2}},
    {stata colorpalette CET CBC1:{bf:CBC1}},
    {stata colorpalette CET CBC2:{bf:CBC2}}
    {p_end}

{pstd}
    and option {opt range(lb [ub])} selects the range of the colormap to be used, where {it:lb} and {it:ub}
    must be in [0,1]. The default is {cmd:range(0 1)}. If {it:lb} is larger than
    {it:ub}, the colors are returned in reverse order. {cmd:range()} has no
    effect for circular colormaps.

{marker scico}{...}
{dlgtab:scico}

{pstd}
    The {cmd:scico} collection provides
    {browse "http://doi.org/10.5281/zenodo.5501399":version 7.0.1} variants of
    the perceptually uniform and colorblind-friendly Scientific color maps by
    {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)}. The syntax
    is

{p 8 15 2}
    {cmd:scico} {it:scheme} [{cmd:,} {opt ra:nge(lb [ub])} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (lower case spelling allowed)
    (see the {browse "http://www.fabiocrameri.ch/colourmaps-userguide/":User Guide}
    for an overview)

{p2colset 9 22 23 2}{...}
{p2col:Sequential:}
    {stata colorpalette scico batlow:{bf:batlow}} (default),
    {stata colorpalette scico batlowW:{bf:batlowW}},
    {stata colorpalette scico batlowK:{bf:batlowK}},
    {stata colorpalette scico devon:{bf:devon}},
    {stata colorpalette scico lajolla:{bf:lajolla}},
    {stata colorpalette scico bamako:{bf:bamako}},
    {stata colorpalette scico davos:{bf:davos}},
    {stata colorpalette scico bilbao:{bf:bilbao}},
    {stata colorpalette scico nuuk:{bf:nuuk}},
    {stata colorpalette scico oslo:{bf:oslo}},
    {stata colorpalette scico grayC:{bf:grayC}},
    {stata colorpalette scico hawaii:{bf:hawaii}},
    {stata colorpalette scico lapaz:{bf:lapaz}},
    {stata colorpalette scico tokyo:{bf:tokyo}},
    {stata colorpalette scico buda:{bf:buda}},
    {stata colorpalette scico acton:{bf:acton}},
    {stata colorpalette scico turku:{bf:turku}},
    {stata colorpalette scico imola:{bf:imola}}
    {p_end}
{p2col:Diverging:}
    {stata colorpalette scico broc:{bf:broc}},
    {stata colorpalette scico cork:{bf:cork}},
    {stata colorpalette scico vik:{bf:vik}},
    {stata colorpalette scico lisbon:{bf:lisbon}},
    {stata colorpalette scico tofino:{bf:tofino}},
    {stata colorpalette scico berlin:{bf:berlin}},
    {stata colorpalette scico roma:{bf:roma}},
    {stata colorpalette scico bam:{bf:bam}},
    {stata colorpalette scico vanimo:{bf:vanimo}}
    {p_end}
{p2col:Cyclic:}
    {stata colorpalette scico romaO:{bf:romaO}},
    {stata colorpalette scico bamO:{bf:bamO}},
    {stata colorpalette scico brocO:{bf:brocO}},
    {stata colorpalette scico corkO:{bf:corkO}},
    {stata colorpalette scico vikO:{bf:vikO}}
    {p_end}

{pstd}
    and option {opt range(lb [ub])} selects the range of the colormap to be
    used, where {it:lb} and {it:ub} must be in [0,1]. The default is
    {cmd:range(0 1)}. If {it:lb} is larger than {it:ub}, the colors are
    returned in reverse order. {cmd:range()} has no effect for cyclic colormaps.

{marker scico8}{...}
{dlgtab:scico8}

{pstd}
    The {cmd:scico8} collection provides
    {browse "http://doi.org/10.5281/zenodo.8409685":version 8.0.1} variants of
    the Scientific color maps by
    {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)}. The syntax
    is as for {helpb colorpalette##scico:sico}, but {cmd:scico8} provides additional
    sequential maps
    {stata colorpalette scico8 glasgow:{bf:glasgow}},
    {stata colorpalette scico8 lipari:{bf:lipari}},
    {stata colorpalette scico8 navia:{bf:navia}}, and
    {stata colorpalette scico8 naviaW:{bf:naviaW}} as well as
    additional diverging map
    {stata colorpalette scico8 managua:{bf:managua}}. Furthermore,
    {cmd:scico8} provides updated variants of
    {stata colorpalette scico8 batlowK:{bf:batlowK}},
    {stata colorpalette scico8 lajolla:{bf:lajolla}},
    {stata colorpalette scico8 bamako:{bf:bamako}},
    {stata colorpalette scico8 bilbao:{bf:bilbao}},
    {stata colorpalette scico8 grayC:{bf:grayC}},
    {stata colorpalette scico8 tokyo:{bf:tokyo}}, and
    {stata colorpalette scico8 acton:{bf:acton}}. For the remaining colormaps,
    {cmd:scico8} is redirected to {helpb colorpalette##scico:sico}.

{marker lin}{...}
{dlgtab:lin}

{pstd}
    The {cmd:lin} collection provides semantic color schemes suggested by
    {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)}. The values
    of the colors have been taken from the source code of the
    {cmd:brewscheme} package by
    {browse "http://ideas.repec.org/c/boc/bocode/s458050.html":Buchanan (2015)} (brewextra.ado,
    version 1.0.0, 21 March 2016).

{pstd}The syntax is

{p 8 15 2}
    {cmd:lin} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed):

{p2colset 9 31 33 2}{...}
{p2col:{stata colorpalette lin carcolor:{bf:carcolor}}}6 car colors (selected by Turkers){p_end}
{p2col:{stata colorpalette lin carcolor algorithm:{bf:carcolor algorithm}}}6 car colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin food:{bf:food}}}7 food colors (selected by Turkers){p_end}
{p2col:{stata colorpalette lin food algorithm:{bf:food algorithm}}}7 food colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin features:{bf:features}}}5 feature colors (selected by Turkers){p_end}
{p2col:{stata colorpalette lin features algorithm:{bf:features algorithm}}}5 feature colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin activities:{bf:activities}}}5 activity colors (selected by Turkers){p_end}
{p2col:{stata colorpalette lin activities algorithm:{bf:activities algorithm}}}5 activity colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin fruits:{bf:fruits}}}7 fruit colors; the default (selected by expert){p_end}
{p2col:{stata colorpalette lin fruits algorithm:{bf:fruits algorithm}}}7 fruit colors; the default (algorithm selected){p_end}
{p2col:{stata colorpalette lin vegetables:{bf:vegetables}}}7 vegetable colors (selected by expert){p_end}
{p2col:{stata colorpalette lin vegetables algorithm:{bf:vegetables algorithm}}}7 vegetable colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin drinks:{bf:drinks}}}7 drinks colors (selected by expert){p_end}
{p2col:{stata colorpalette lin drinks algorithm:{bf:drinks algorithm}}}7 drinks colors (algorithm selected){p_end}
{p2col:{stata colorpalette lin brands:{bf:brands}}}7 brands colors (selected by expert){p_end}
{p2col:{stata colorpalette lin brands algorithm:{bf:brands algorithm}}}7 brands colors (algorithm selected){p_end}

{marker spmap}{...}
{dlgtab:spmap}

{pstd}
    The {cmd:spmap} collection provides color schemes from the {cmd:spmap}
    package by {browse "http://ideas.repec.org/c/boc/bocode/s456812.html":Pisati (2007)}. The
    implementation is based on code from spmap_color.ado (version 1.3.0, 13 March 2017).

{pstd}
    The syntax is

{p 8 15 2}
    {cmd:spmap} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)
    ({stata "colorpalette, n(16) lc(white): spmap bl / spmap green / spmap grey / spmap red / spmap rain / spmap heat / spmap ter / spmap top":{it:view}}):

{p2colset 9 24 26 2}{...}
{p2col:{stata colorpalette spmap blues:{bf:blues}}}light blue to blue (2-99 colors); the default{p_end}
{p2col:{stata colorpalette spmap greens:{bf:greens}}}light green to green (2-99 colors){p_end}
{p2col:{stata colorpalette spmap greys:{bf:greys}}}light gray to black (2-99 colors){p_end}
{p2col:{stata colorpalette spmap reds:{bf:reds}}}light red to red (2-99 colors){p_end}
{p2col:{stata colorpalette spmap rainbow:{bf:rainbow}}}2-99 rainbow colors{p_end}
{p2col:{stata colorpalette spmap heat:{bf:heat}}}2-16 heat colors{p_end}
{p2col:{stata colorpalette spmap terrain:{bf:terrain}}}2-16 terrain colors{p_end}
{p2col:{stata colorpalette spmap topological:{bf:topological}}}2-16 topological colors{p_end}

{marker sfso}{...}
{dlgtab:sfso}

{pstd}
    The {cmd:sfso} collection provides color schemes by the Swiss Federal Statistical
    Office (using hex and CMYK codes found in Bundesamt f{c u:}r Statistik 2017). The
    syntax is

{p 8 15 2}
    {cmd:sfso} [{it:scheme}] [{cmd:cmyk}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed)

{p2colset 9 24 26 2}{...}
{p2col:Sequential ({stata "colorpalette: sfso br / sfso or / sfso red / sfso pi / sfso pu / sfso vi / sfso blu / sfso ltbl / sfso tu / sfso green / sfso ol / sfso black":{it:view}})}{p_end}
{p2col:{stata colorpalette sfso brown:{bf:brown}}}dark brown to light brown (6 colors){p_end}
{p2col:{stata colorpalette sfso orange:{bf:orange}}}dark orange to light orange (6 colors){p_end}
{p2col:{stata colorpalette sfso red:{bf:red}}}dark red to light red (6 colors){p_end}
{p2col:{stata colorpalette sfso pink:{bf:pink}}}dark pink to light pink (6 colors){p_end}
{p2col:{stata colorpalette sfso purple:{bf:purple}}}dark purple to light purple (6 colors){p_end}
{p2col:{stata colorpalette sfso violet:{bf:violet}}}dark violet to light violet (6 colors){p_end}
{p2col:{stata colorpalette sfso blue:{bf:blue}}}dark blue to light blue (7 colors); the default{p_end}
{p2col:{stata colorpalette sfso ltblue:{bf:ltblue}}}lighter version of {cmd:blue} (6 colors){p_end}
{p2col:{stata colorpalette sfso turquoise:{bf:turquoise}}}dark turquoise to light turquoise (6 colors){p_end}
{p2col:{stata colorpalette sfso green:{bf:green}}}dark green to light green (6 colors){p_end}
{p2col:{stata colorpalette sfso olive:{bf:olive}}}dark olive to light olive (6 colors){p_end}
{p2col:{stata colorpalette sfso black:{bf:black}}}dark gray to light gray (6 colors){p_end}

{p2col:Semantic ({stata "colorpalette: sfso parties / sfso languages / sfso votes":{it:view}})}{p_end}
{p2col:{stata colorpalette sfso parties:{bf:parties}}}colors used by the SFSO for Swiss parties (11 colors){p_end}
{p2col:{stata colorpalette sfso languages:{bf:languages}}}colors used by the SFSO for languages (5 colors){p_end}
{p2col:{stata colorpalette sfso votes:{bf:votes}}}colors used by the SFSO for results from votes (10 colors){p_end}

{phang}
    and argument {cmd:cmyk} requests the CMYK variant of the scheme. The default is to use
    the RGB variant.

{marker html}{...}
{dlgtab:HTML}

{pstd}
    The {cmd:HTML} collection provides named HTML colors from
    {browse "http://www.w3schools.com/colors/colors_names.asp":www.w3schools.com}. The
    syntax is

{p 8 15 2}
    {cmdab:HTML} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed).

{p2colset 9 24 26 2}{...}
{p2col:{stata colorpalette HTML pink:{bf:pink}}}6 pink colors{p_end}
{p2col:{stata colorpalette HTML purple:{bf:purple}}}19 purple colors{p_end}
{p2col:{stata colorpalette HTML red:{bf:red}}}14 red and orange colors{p_end}
{p2col:{stata colorpalette HTML orange:{bf:orange}}}14 red and orange colors{p_end}
{p2col:{stata colorpalette HTML yellow:{bf:yellow}}}11 yellow colors{p_end}
{p2col:{stata colorpalette HTML green:{bf:green}}}22 green colors{p_end}
{p2col:{stata colorpalette HTML cyan:{bf:cyan}}}8 cyan colors{p_end}
{p2col:{stata colorpalette HTML blue:{bf:blue}}}16 blue colors{p_end}
{p2col:{stata colorpalette HTML brown:{bf:brown}}}18 brown colors{p_end}
{p2col:{stata colorpalette HTML white:{bf:white}}}17 white colors{p_end}
{p2col:{stata colorpalette HTML gray:{bf:gray}}}10 gray colors{p_end}
{p2col:{stata colorpalette HTML grey:{bf:grey}}}10 grey colors (same color codes as {cmd:gray}){p_end}

{pstd}
    All 148 HTML colors (alphabetically sorted) will be returned if {it:scheme}
    is omitted.

{marker w3}{...}
{dlgtab:w3}

{pstd}
    The {cmd:w3} collection provides colors from
    {browse "http://www.w3schools.com/w3css/w3css_colors.asp":W3.CSS}. The
    syntax is

{p 8 15 2}
    {cmdab:w3} [{it:scheme}] [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations allowed).

{p2colset 9 24 26 2}{...}
{p2col:{stata colorpalette w3 default:{bf:default}}}30 {browse "http://www.w3schools.com/w3css/w3css_color_material.asp":Default Colors}; the default{p_end}
{p2col:{stata colorpalette w3 flat:{bf:flat}}}20 {browse "http://www.w3schools.com/w3css/w3css_color_flat.asp":Flat UI Colors}{p_end}
{p2col:{stata colorpalette w3 metro:{bf:metro}}}17 {browse "http://www.w3schools.com/w3css/w3css_color_metro.asp":Metro UI Colors}{p_end}
{p2col:{stata colorpalette w3 win8:{bf:win8}}}22 {browse "http://www.w3schools.com/w3css/w3css_color_win8.asp":Windows 8 Colors}{p_end}
{p2col:{stata colorpalette w3 ios:{bf:ios}}}12 {browse "http://www.w3schools.com/w3css/w3css_color_ios.asp":iOS Colors}{p_end}
{p2col:{stata colorpalette w3 highway:{bf:highway}}}7 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Highway Colors}{p_end}
{p2col:{stata colorpalette w3 safety:{bf:safety}}}6 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Safety Colors}{p_end}
{p2col:{stata colorpalette w3 signal:{bf:signal}}}10 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":European Signal Colors}{p_end}
{p2col:{stata colorpalette w3 2019:{bf:2019}}}32 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2019}{p_end}
{p2col:{stata colorpalette w3 2018:{bf:2018}}}30 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2018}{p_end}
{p2col:{stata colorpalette w3 2017:{bf:2017}}}20 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2017}{p_end}
{p2col:{stata colorpalette w3 vivid:{bf:vivid}}}21 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Vivid Colors}{p_end}
{p2col:{stata colorpalette w3 food:{bf:food}}}40 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Food Colors}{p_end}
{p2col:{stata colorpalette w3 camo:{bf:camo}}}15 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Camouflage Colors}{p_end}
{p2col:{stata colorpalette w3 ana:{bf:ana}}}44 {browse "http://www.w3schools.com/colors/colors_fs595.asp":Army Navy Aero Colors}{p_end}
{p2col:{stata colorpalette w3 traffic:{bf:traffic}}}9 {browse "http://www.w3schools.com/colors/colors_ral.asp":Traffic Colors}{p_end}
{p2col:{stata colorpalette w3 amber:{bf:amber}}}11 sequential Amber colors{p_end}
{p2col:{stata colorpalette w3 black:{bf:black}}}11 sequential Black colors{p_end}
{p2col:{stata colorpalette w3 blue:{bf:blue}}}11 sequential Blue colors{p_end}
{p2col:{stata colorpalette w3 blue-grey:{bf:blue-grey}}}11 sequential Blue Grey colors{p_end}
{p2col:{stata colorpalette w3 brown:{bf:brown}}}11 sequential Brown colors{p_end}
{p2col:{stata colorpalette w3 cyan:{bf:cyan}}}11 sequential Cyan colors{p_end}
{p2col:{stata colorpalette w3 dark-grey:{bf:dark-grey}}}11 sequential Dark Grey colors{p_end}
{p2col:{stata colorpalette w3 deep-orange:{bf:deep-orange}}}11 sequential Deep Orange colors{p_end}
{p2col:{stata colorpalette w3 deep-purple:{bf:deep-purple}}}11 sequential Deep Purple colors{p_end}
{p2col:{stata colorpalette w3 green:{bf:green}}}11 sequential Green colors{p_end}
{p2col:{stata colorpalette w3 grey:{bf:grey}}}11 sequential Grey colors{p_end}
{p2col:{stata colorpalette w3 indigo:{bf:indigo}}}11 sequential Indigo colors{p_end}
{p2col:{stata colorpalette w3 khaki:{bf:khaki}}}11 sequential Khaki colors{p_end}
{p2col:{stata colorpalette w3 light-blue:{bf:light-blue}}}11 sequential Light Blue colors{p_end}
{p2col:{stata colorpalette w3 light-green:{bf:light-green}}}11 sequential Light Green colors{p_end}
{p2col:{stata colorpalette w3 lime:{bf:lime}}}11 sequential Lime colors{p_end}
{p2col:{stata colorpalette w3 orange:{bf:orange}}}11 sequential Orange colors{p_end}
{p2col:{stata colorpalette w3 pink:{bf:pink}}}11 sequential Pink colors{p_end}
{p2col:{stata colorpalette w3 purple:{bf:purple}}}11 sequential Purple colors{p_end}
{p2col:{stata colorpalette w3 red:{bf:red}}}11 sequential Red colors{p_end}
{p2col:{stata colorpalette w3 teal:{bf:teal}}}11 sequential Teal colors{p_end}
{p2col:{stata colorpalette w3 yellow:{bf:yellow}}}11 sequential Yellow colors{p_end}

{marker wes}{...}
{dlgtab:wesanderson}

{pstd}
    The {it:wesanderson} collection provides Wes Anderson palettes from
    {browse "http://wesandersonpalettes.tumblr.com/":wesandersonpalettes.tumblr.com} (the
    color codes have been obtained from
    {browse "http://github.com/karthik/wesanderson":github.com/karthik/wesanderson}). The syntax is

{p 8 15 2}
    {it:scheme} [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:scheme} is one of the following (abbreviations and lower case spelling allowed).

{p2colset 9 26 27 2}{...}
{p2col:}
    ({stata "colorpalette: BottleRocket1 / BottleRocket2 / Rushmore1 / Royal1 / Royal2 / Zissou1 / Darjeeling1 / Darjeeling2 / Chevalier1 / FantasticFox1":{it:view 1-10}})
    ({stata "colorpalette: Moonrise1 / Moonrise2 / Moonrise3 / Cavalcanti1 / GrandBudapest1 / GrandBudapest2 / IsleofDogs1 / IsleofDogs2 / FrenchDispatch1":{it:view 11-19}})
    {p_end}
{p2col:{stata colorpalette BottleRocket1:{bf:BottleRocket1}}} 7 qualitative colors from Bottle Rocket{p_end}
{p2col:{stata colorpalette BottleRocket2:{bf:BottleRocket2}}} 5 qualitative colors from Bottle Rocket{p_end}
{p2col:{stata colorpalette Rushmore1:{bf:Rushmore1}}} 5 qualitative colors from Rushmore{p_end}
{p2col:{stata colorpalette Royal1:{bf:Royal1}}} 4 qualitative colors from The Royal Tenenbaums{p_end}
{p2col:{stata colorpalette Royal2:{bf:Royal2}}} 5 qualitative colors from The Royal Tenenbaums{p_end}
{p2col:{stata colorpalette Zissou1:{bf:Zissou1}}} 5 diverging colors from The Life Aquatic with Steve Zissou{p_end}
{p2col:{stata colorpalette Darjeeling1:{bf:Darjeeling1}}} 5 qualitative colors from The Darjeeling Limited{p_end}
{p2col:{stata colorpalette Darjeeling2:{bf:Darjeeling2}}} 5 qualitative colors from The Darjeeling Limited{p_end}
{p2col:{stata colorpalette Chevalier1:{bf:Chevalier1}}} 4 qualitative colors from Hotel Chevalier{p_end}
{p2col:{stata colorpalette FantasticFox1:{bf:FantasticFox1}}} 5 qualitative colors from Fantastic Mr. Fox{p_end}
{p2col:{stata colorpalette Moonrise1:{bf:Moonrise1}}} 4 qualitative colors from Moonrise Kingdom{p_end}
{p2col:{stata colorpalette Moonrise2:{bf:Moonrise2}}} 4 qualitative colors from Moonrise Kingdom{p_end}
{p2col:{stata colorpalette Moonrise3:{bf:Moonrise3}}} 5 qualitative colors from Moonrise Kingdom{p_end}
{p2col:{stata colorpalette Cavalcanti1:{bf:Cavalcanti1}}} 5 qualitative colors from Castello Cavalcanti{p_end}
{p2col:{stata colorpalette GrandBudapest1:{bf:GrandBudapest1}}} 4 qualitative colors from The Grand Budapest Hotel{p_end}
{p2col:{stata colorpalette GrandBudapest2:{bf:GrandBudapest2}}} 4 qualitative colors from The Grand Budapest Hotel{p_end}
{p2col:{stata colorpalette IsleofDogs1:{bf:IsleofDogs1}}} 6 qualitative colors from The Isle of Dogs{p_end}
{p2col:{stata colorpalette IsleofDogs2:{bf:IsleofDogs2}}} 5 qualitative colors from The Isle of Dogs{p_end}
{p2col:{stata colorpalette FrenchDispatch1:{bf:FrenchDispatch1}}} 5 qualitative colors from The French Dispatch{p_end}

{marker myname}{...}
{dlgtab:myname}

{pstd}
    A personal named palette can be provided in form of a program called
    {cmd:colorpalette_}{it:myname}, where {it:myname} is the name of the
    palette. Palette {it:myname} will then be available to {cmd:colorpalette}
    like any other palette. Your program should behave as follows.

{phang2}
    (1) It must return the color definitions as a comma-separated list in local
    macro {cmd:P}. All types of {help colorpalette##colorlist:color specifications}
    supported by {cmd:colorpalette}, including opacity and intensity operators,
    are allowed for the individual colors in the list.

{phang2}
    (2) If input is parsed using {helpb syntax}, option {cmd:n()} must
    be allowed. In addition to {cmd:n()}, all options not consumed by {cmd:colorpalette}
    will be passed through to {cmd:colorpalette_}{it:myname}. This makes
    it possible to support custom options in your program.

{phang2}
    (3) Color names can be returned as a comma-separated list in local
    macro {cmd:N}.

{phang2}
    (4) Color descriptions can be returned as a comma-separated list in local
    macro {cmd:I}.

{phang2}
    (5) The palette name can be returned in local macro {cmd:name} ({it:myname}
    is used as the palette name if no name is returned).

{phang2}
    (6) The palette class can be returned in local macro {cmd:class}.

{phang2}
    (7) A palette description can be returned in local macro {cmd:note}.

{phang2}
    (8) Information on the source of the palette can be returned in local macro {cmd:source}.

{pstd}
    For example, the following program provides a palette called
    {cmd:bootstrap3} containing semantic colors used for buttons in
    {browse "http://getbootstrap.com/docs/3.3/":Bootstrap 3.3}:

        {com}program colorpalette_bootstrap3
            syntax [, n(str) ] // n() not used
            c_local P #ffffff,#337ab7,#5cb85c,#5bc0de,#f0ad4e,#d9534f,/*
                    */#e6e6e6,#286090,#449d44,#31b0d5,#ec971f,#c9302c
            c_local N default,primary,success,info,warning,danger,/*
                */default.focus,primary.focus,success.focus,info.focus,/*
                */warning.focus,danger.focus
            c_local class qualitative
            c_local note Button colors from Bootstrap 3.3
            c_local source https://getbootstrap.com/docs/3.3/
        end{txt}

{pstd}
    After defining the program, you can, for example, type

        {com}. colorpalette bootstrap3, rows(6){txt}

{pstd}
    To make the new palette permanently available, store the program in
    file {cmd:colorpalette_}{it:myname}{cmd:.ado} in the working directory or
    somewhere along Stata’s ado path (see {helpb sysdir}).

{marker mata}{...}
{dlgtab:Mata ColrSpace object}

{pstd}
    {cmd:colorpalette} can retrieve colors from a Mata
    {helpb colrspace:ColrSpace} object. The syntax is

        {cmdab:m:ata(}{it:name}{cmd:)} [{cmd:,} {help colorpalette##opts:{it:palette_options}} ]

{pstd}
    where {it:name} is the name the object. Example:

        . {stata `"mata: C1 = C2 = ColrSpace()"'}
        . {stata `"mata: C1.palette("s2", 5)"'}
        . {stata `"mata: C1.name("original colors")"'}
        . {stata `"mata: C2.set(C1.get("Lab") :* (1.2, 1, 1), "Lab")"'}
        . {stata `"mata: C2.name("luminance * 1.2")"'}
        . {stata "colorpalette: mata(C1) / mata(C2)"}
        . {stata "colorpalette mata(C2), cblind(0.5) title(50% deuteranomaly)"}


{marker examples}{...}
{title:Examples}

{dlgtab:Retrieving colors from r()}

{pstd}
    Apart from viewing color palettes, {cmd:colorpalette} is useful for
    retrieving colors that can then be passed through to a graph command. The
    color codes are stored in {helpb colorpalette##returns:r()}. Here is
    an example of a contour plot using the {cmd:terrain} scheme from
    the {helpb colorpalette##hcl:hcl} color generator:

        . {stata sysuse sandstone, clear}
        . {stata colorpalette jmh terrain, n(10) nograph}
{p 8 12 2}
        . {stata twoway contour depth northing easting, levels(`r(n)') ccolors(`r(p)')}

{marker exglobals}{...}
{dlgtab:Making colors available as global macros}

{pstd}
    To make colors available as {helpb global} macros, use the
    {helpb colorpalette##macrooptions:globals()} option. For example, to
    make all {help colorpalette##htmlcolors:HTML colors} available, type:

        {com}. colorpalette HTML, globals{txt}

{pstd}
    You can then use the created globals in subsequent graph commands:

        {com}. sysuse auto, clear
        . scatter price weight, mc($Fuchsia) graphr(color($HoneyDew))
        . scatter price weight, mc($Tomato) graphr(color($SeaShell)){txt}

{pstd}
    The globals will disappear once you restart Stata.

{marker exlocals}{...}
{dlgtab:Making colors available as local macros}

{pstd}
    Depending on context (e.g., within a do-file or program), it may be
    convenient to make colors available as
    {helpb local} macros; see the {helpb colorpalette##macrooptions:locals()}
    option. Example:

        {com}. colorpalette Accent, locals
        . sysuse auto, clear
        . scatter trunk turn mpg weight, mc(`1' `2' `3'){txt}

{pstd}
    The locals will disappear once the do-file or program concludes.

{marker exstylefiles}{...}
{dlgtab:Making colors permanently available using style files}

{pstd}
    To make new colors available permanently, use the
    {helpb colorpalette##stylefiles:stylefiles()} option. This will cause
    RGB color definitions to be stored in style files on disk (one file for each
    color), from where Stata will read the color definitions (if the graph system
    has already been loaded, that is, if you already produced a graph in the current
    session, you will need to clear the graph memory before the stored colors
    become available; use {helpb discard} or {helpb clear all} to flush the
    working memory). The color names can then be used just like Stata's
    official color names. Example:

        {com}. colorpalette Fuchsia Tomato SteelBlue SeaShell, stylefiles
        . discard
        . sysuse auto, clear{txt}
{p 8 12 2}
        {com}. scatter trunk turn mpg weight, mc(Fuchsia Tomato SteelBlue) graphr(color(SeaShell)){txt}

{pstd}
    {cmd:colorpalette} will store the style files in folder 'style' in the current
    working directory. That is, the color definitions will be found by Stata
    as long as you do not change the working directory.

{pstd}
    To make the colors permanently available irrespective
    of the working directory, type {cmd:sylefiles(, personal)}. In this case the
    style files will be stored in folder 'style' within the {cmd:PERSONAL} ado-file
    directory; see help {helpb sysdir}.

{marker excheck}{...}
{dlgtab:Analyzing colors using colorcheck}

        . {stata colorpalette s2, nograph}
        . {stata colorcheck}
        . {stata colorcheck, sort(mono)}
        . {stata colorcheck, sort(deuter)}
        . {stata colorcheck, metric(E2000) nograph}
        . {stata colorcheck, mono(0.9) cvd(0.75) vertical}


{marker returns}{...}
{title:Saved results}

{pstd}
    {cmd:colorpalette} stores the following in {cmd:r()} (Syntax 1 only):

{synoptset 16 tabbed}{...}
{p2col 5 16 20 2: Scalars}{p_end}
{synopt:{cmd:r(n)}}number of returned colors{p_end}

{synoptset 16 tabbed}{...}
{p2col 5 16 20 2: Macros}{p_end}
{synopt:{cmd:r(ptype)}}{cmd:color}{p_end}
{synopt:{cmd:r(pname)}}name of palette or {cmd:custom}{p_end}
{synopt:{cmd:r(pclass)}}palette class (if provided){p_end}
{synopt:{cmd:r(pnote)}}palette description (if provided){p_end}
{synopt:{cmd:r(psource)}}palette source (if provided){p_end}
{synopt:{cmd:r(p)}}space-separated list of colors{p_end}
{synopt:{cmd:r(p#)}}#th color{p_end}
{synopt:{cmd:r(p#name)}}name of #th color (if provided){p_end}
{synopt:{cmd:r(p#info)}}info of #th color (if provided){p_end}
{synopt:{cmd:r(c)}}space-separated list of colors stored by option {helpb colorpalette##return:return()}{p_end}
{synopt:{cmd:r(cspace)}}color space applied by option {helpb colorpalette##return:return()}{p_end}

{pstd}
    {cmd:colorcheck} adds the following to {cmd:r()}, in addition to the
    results from {cmd:colorpalette}:

{synoptset 16 tabbed}{...}
{p2col 5 16 20 2: Scalars}{p_end}
{synopt:{cmd:r(mono)}}proportion of gray{p_end}
{synopt:{cmd:r(cvd)}}CVD severity{p_end}

{synoptset 16 tabbed}{...}
{p2col 5 16 20 2: Macros}{p_end}
{synopt:{cmd:r(metric)}}color difference metric{p_end}
{synopt:{cmd:r(mono_method)}}grayscale conversion method{p_end}
{synopt:{cmd:r(p_norm)}}list of normal sight colors{p_end}
{synopt:{cmd:r(p_mono)}}list of grayscale converted colors{p_end}
{synopt:{cmd:r(p_deut)}}list of deteranomaly transformed colors{p_end}
{synopt:{cmd:r(p_prot)}}list of protanomaly transformed colors{p_end}
{synopt:{cmd:r(p_trit)}}list of tritanomaly transformed colors{p_end}

{synoptset 16 tabbed}{...}
{p2col 5 16 20 2: Matrix}{p_end}
{synopt:{cmd:r(delta)}}color difference statistics{p_end}


{marker references}{...}
{title:References}

{phang}
    Bischof, D. 2017a. G538SCHEMES: module to provide graphics schemes for
    http://fivethirtyeight.com. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458404.html"}.
    {p_end}
{phang}
    Bischof, D. 2017b. {browse "http://www.stata-journal.com/article.html?article=gr0070":New graphic schemes for Stata: plotplain and plottig}.
    The Stata Journal 17(3): 748–759.
    {p_end}
{phang}
    Briatte, F. 2013. SCHEME-BURD: Stata module to provide a
    ColorBrewer-inspired graphics scheme with qualitative and blue-to-red
    diverging colors. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s457623.html"}.
    {p_end}
{phang}
    Brewer, C. A., G. W. Hatchard, M. A. Harrower. 2003. {browse "http://doi.org/10.1559/152304003100010929":ColorBrewer in Print: A Catalog of Color Schemes for Maps}.
    Cartography and Geographic Information Science 30(1): 5–32.
    {p_end}
{phang}
    Brewer, C. A. 2016. Designing Better Maps. A Guide for GIS Users. 2nd ed. Redlands, CA: Esri Press.
    {p_end}
{phang}
    Buchanan, B. 2015. BREWSCHEME: Stata module for generating customized
    graph scheme files. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458050.html"}.
    {p_end}
{phang}
    Bundesamt f{c u:}r Statistik 2017. Layoutrichtlinien. Gestaltungs und
    Redaktionsrichtlinien für Publikationen, Tabellen und grafische
    Assets. Version 1.1.1. Neuchâtel.
    {p_end}
{phang}
    Crameri, F. (2018). Scientific colour maps. Zenodo. {browse "http://doi.org/10.5281/zenodo.1243862":DOI: 10.5281/zenodo.1243862}.
    {p_end}
{phang}
    Gomez, M. 2015. Stata command to generate color schemes. Available from
    {browse "http://github.com/matthieugomez/stata-colorscheme"}.
    {p_end}
{phang}
    Hunter, J. D. 2007. {browse "http://dx.doi.org/10.1109/MCSE.2007.55":Matplotlib: A 2D graphics environment}. Computing
    in Science & Engineering 9(3): 90-95.
    {p_end}
{phang}
    Ihaka, R., P. Murrell, K. Hornik, J. C. Fisher, R. Stauffer, A. Zeileis.
    2016. colorspace: Color Space Manipulation. R package version 1.3-2.
    Available from {browse "http://CRAN.R-project.org/package=colorspace"}.
    {p_end}
{phang}
    Juul, S. 2003. {browse "http://www.stata-journal.com/article.html?article=gr0002":Lean mainstream schemes for Stata 8 graphics}. The Stata
    Journal 3(3): 295-301.
    {p_end}
{phang}
    Kovesi, P. (2015). Good Colour Maps: How to Design Them. {browse "http://arxiv.org/abs/1509.03700":arXiv:1509.03700} [cs.GR].
    {p_end}
{phang}
    Lin, S., J. Fortuna, C. Kulkarni, M. Stone,
    J. Heer. 2013. {browse "http://dx.doi.org/10.1111/cgf.12127":Selecting Semantically-Resonant Colors for Data Visualization}. Computer
    Graphics Forum 32(3pt4): 401-410.
    {p_end}
{phang}
    Morris, T. 2013. SCHEME-MRC: Stata module to provide graphics scheme for UK
    Medical Research Council. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s457703.html"}.
    {p_end}
{phang}
    Morris, T. 2015. SCHEME-TFL: Stata module to provide graph scheme, based on
    Transport for London's corporate colour pallette. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458103.html"}.
    {p_end}
{phang}
    Okabe, M., K. Ito. 2002. Color Universal Design (CUD). How to make figures and presentations that
    are friendly to Colorblind people. Available from
    {browse "http://jfly.iam.u-tokyo.ac.jp/color/"}.
    {p_end}
{phang}
    Pisati, M. 2007. SPMAP: Stata module to visualize spatial data. Available
    from {browse "http://ideas.repec.org/c/boc/bocode/s456812.html"}.
    {p_end}
{phang}
    Tol, P. 2012. Colour Schemes. SRON Technical Note, Doc. no. SRON/EPS/TN/09-002. Available
    from {browse "http://personal.sron.nl/~pault/colourschemes.pdf"}.
    {p_end}
{phang}
    Zeileis, A., K. Hornik, P. Murrell. 2009.
    {browse "http://dx.doi.org/10.1016/j.csda.2008.11.033":Escaping RGBland: Selecting Colors for Statistical Graphics}.
    Computational Statistics & Data Analysis 53: 3259-3270.
    {p_end}


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software in one of the following ways:

{phang}
    Jann, B. 2022. Color palettes for Stata graphics: an update.  University of Bern
    Social Sciences Working Papers No. 43. Available from {browse "http://ideas.repec.org/p/bss/wpaper/43.html"}.
    {p_end}
{phang}
    Jann, B. 2018. {browse "http://www.stata-journal.com/article.html?article=gr0075":Color palettes for Stata graphics}. The Stata Journal
    18(4): 765-785.
    {p_end}
{phang}
    Jann, B. 2017. palettes: Stata module providing color palettes, symbol
    palettes, and line pattern palettes. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458444.html"}.
    {p_end}


{marker alsosee}{...}
{title:Also see}

{psee}
    Online:  help for {helpb colrspace}, {helpb colorpalette9}, {helpb symbolpalette},
    {helpb linepalette}, {helpb grstyle set}, {helpb graph}, {it:{help colorstyle}}

