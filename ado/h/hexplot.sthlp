{smcl}
{* 20jul2021}{...}
{hi:help hexplot}{right:{browse "http://github.com/benjann/heatplot/"}}
{hline}

{title:Title}

{pstd}{hi:hexplot} {hline 2} Command to create hexagon plots


{title:Syntax}

{pstd}
    Syntax 1: Hex plot from variables

{p 8 15 2}
    {cmd:hexplot} [{it:z}] {it:y} {it:x} {ifin} {weight}
    [{cmd:,}
    {help hexplot##opts:{it:options}}
    ]

{pmore}
    where {it:z} is a numeric variable (assumed constant if omitted), {it:y}
    is a numeric variable or a string variable, and {it:x} is a numeric variable
    or a string variable. Categorical {it:y} and {it:x} variables can be specified
    as {cmd:i.}{it:varname}.

{pstd}
    Syntax 2: Hex plot from Mata matrix

{p 8 15 2}
    {cmd:hexplot} {opt m:ata(name)}
    [{cmd:,}
    {help hexplot##opts:{it:options}}
    ]

{pmore}
    where {it:name} is a numeric {help mata:Mata matrix} (contents = {it:z}, row index = {it:y},
    column index = {it:x}).

{pstd}
    Syntax 3: Hex plot from Stata matrix

{p 8 15 2}
    {cmd:hexplot} {it:matname} [{cmd:,}
    {help hexplot##opts:{it:options}}
    ]

{pmore}
    where {it:matname} is a {help matrix:Stata matrix}
    (contents = {it:z}, row names = {it:y}, column names = {it:x}).


{marker opts}{...}
{synoptset 22}{...}
{synopthdr:options}
{synoptline}
{synopt :{opt hor:izontal}}arrange hexagons horizontally
    {p_end}
{synopt :{opt left}}start with a left shift
    {p_end}
{synopt :{opt odd}}use odd number of columns
    {p_end}
{synopt :{helpb heatplot##heatopts:{it:heatplot_options}}}Syntax 1, Syntax 2, or
    Syntax 3 options of {helpb heatplot}
    {p_end}
{synoptline}

{pstd}
    {cmd:fweight}s, {cmd:aweight}s, {cmd:iweight}s, and {cmd:pweight}s are allowed with Syntax 1; see help {help weight}.


{title:Description}

{pstd}
    {cmd:hexplot} creates hexagon plots. It is implemented as a wrapper for
    {helpb heatplot}. {cmd:hexplot} is equivalent to {cmd:heatplot} with option
    {cmd:hexagon}.


{title:Options}

{phang}
    {opt horizontal} arranges the hexagons horizontally. The default is to arrange
    the hexagons vertically.

{phang}
    {opt left} starts with a left-shifted hexagon row. The default is to start 
    with a right-shifted row. If {cmd:horizontal} is specified, {cmd:left}
    starts with an down-shifted row instead of an up-shifted row.

{phang}
    {opt odd} uses an odd number of hexagon columns. The default is to use
    an even number of columns. That is, by default the bins on the x-axis are
    constructed in a way such that each bin contains a double column of hexagons,
    yielding an even overall number of columns. Specify {cmd:odd} to construct the
    bins in a way such that the last bin only contains a single column. If 
    {cmd:horizontal} is specified, {cmd:odd} affects the number of
    hexagon rows rather than columns.

{phang}
    {it:heatplot_options} are {helpb heatplot} options allowed in Syntax 1, 2, or
    3, respectively. Not allowed are options {cmd:scatter()}, {cmd:hexagon()}, 
    {cmd:bcuts()}, {cmd:ybcuts()}, and {cmd:xbcuts()}.

{title:Examples}

    . {stata drawnorm y x, n(10000) corr(1 .5 1) cstorage(lower) clear}
    . {stata hexplot y x}
    . {stata hexplot y x, horizontal}
    . {stata hexplot y x, size recenter}
{p 4 8 2}
    . {stata hexplot y x, statistic(count) cuts(@min(5)@max) colors(dimgray black) keylabels(, range(1))}

    . {stata sysuse auto, clear}
{p 4 8 2}
    . {stata hexplot price weight mpg, colors(plasma, intensity(.6)) p(lc(black) lalign(center)) legend(off) values(format(%9.0f)) aspectratio(1)}


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2019). heatplot: Stata module to create heat plots and hexagon plots. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458595.html"}.


{title:Also see}

{psee}
    Online:  help for {helpb heatplot}, {helpb colorpalette},
    {helpb twoway contour}
