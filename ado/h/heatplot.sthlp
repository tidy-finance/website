{smcl}
{* 23jul2021}{...}
{hi:help heatplot}{right:{browse "http://github.com/benjann/heatplot/"}}
{hline}

{title:Title}

{pstd}{hi:heatplot} {hline 2} Command to create heat plots


{title:Syntax}

{pstd}
    Syntax 1: Heat plot from variables

{p 8 15 2}
    {cmd:heatplot} [{it:z}] {it:y} {it:x} {ifin} {weight}
    [{cmd:,}
    {help heatplot##zopt:{it:z_options}}
    {help heatplot##yxopt:{it:yx_options}}
    {help heatplot##gropt:{it:graph_options}}
    {help heatplot##genopt:{it:generate_options}}
    ]

{pmore}
    where {it:z} is a numeric variable (assumed constant if omitted), {it:y}
    is a numeric variable or a string variable, and {it:x} is a numeric variable
    or a string variable. Categorical {it:y} and {it:x} variables can be specified
    as {cmd:i.}{it:varname}.

{pstd}
    Syntax 2: Heat plot from Mata matrix

{p 8 15 2}
    {cmd:heatplot} {opt mata(name)}
    [{cmd:,}
    {help heatplot##zopt:{it:z_options}}
    {help heatplot##yxopt:{it:yx_options}}
    {help heatplot##mopt:{it:matrix_options}}
    {help heatplot##gropt:{it:graph_options}}
    {help heatplot##genopt:{it:generate_options}}
    ]

{pmore}
    where {it:name} is a numeric {help mata:Mata matrix} (contents = {it:z}, row index = {it:y},
    column index = {it:x}).

{pstd}
    Syntax 3: Heat plot from Stata matrix

{p 8 15 2}
    {cmd:heatplot} {it:matname} [{cmd:,}
    {help heatplot##zopt:{it:z_options}}
    {help heatplot##mopt:{it:matrix_options}}
    {help heatplot##gropt:{it:graph_options}}
    {help heatplot##genopt:{it:generate_options}}
    ]

{pmore}
    where {it:matname} is a {help matrix:Stata matrix}
    (contents = {it:z}, row names = {it:y}, column names = {it:x}).


{synoptset 22}{...}
{marker zopt}{synopthdr:z_options}
{synoptline}
{synopt :{helpb heatplot##levels:{ul:lev}els({it:#})}}number of color bins
    {p_end}
{synopt :{helpb heatplot##cuts:{ul:cut}s({it:numlist})}}custom cut points for color bins
    {p_end}
{synopt :{helpb heatplot##colors:{ul:c}olors({it:palette})}}color map to be used for the color bins
    {p_end}
{synopt :{helpb heatplot##backfill:{ul:backf}ill{sf:[}({it:options}){sf:]}}}fill background using first or last color
    {p_end}
{synopt :{helpb heatplot##statistic:{ul:s}tatistic({it:stat})}}(syntax 1 and 2 only) type of aggregation
    {p_end}
{synopt :{helpb heatplot##fast:fast}}(syntax 1 and 2 only) use fast aggregation; requires {helpb gtools}
    {p_end}
{synopt :{helpb heatplot##normalize:{ul:norm}alize}}normalize z values by the size of the covered area
        {p_end}
{synopt :{helpb heatplot##transform:{ul:trans}form({it:@exp})}}transform z values before creating color bins
        {p_end}
{synopt :{helpb heatplot##size:size}}scale size of color fields by absolute values of z
    {p_end}
{synopt :{helpb heatplot##size:size({it:spec})}}scale size of color fields by alternative values
    {p_end}
{synopt :{helpb heatplot##sizeprop:{ul:sizep}rop}}(syntax 1 only) scale size of color fields by relative frequencies
    {p_end}
{synopt :{helpb heatplot##recenter:{ul:rec}enter}}(syntax 1 only) recenter color fields at data center within field
    {p_end}
{synopt :{helpb heatplot##srange:{ul:sr}ange({it:lo} {sf:[}{it:up}{sf:]})}}set range of relative sizes of color fields
    {p_end}
{synopt :{helpb heatplot##missing:{ul:m}issing{sf:[}({it:options}){sf:]}}}display missing values
    {p_end}
{synopt :{helpb heatplot##values:{ul:val}ues{sf:[}({it:options}){sf:]}}}display numeric values as marker labels
    {p_end}
{synopt :{helpb heatplot##hexagon:{ul:hex}agon{sf:[}({it:options}){sf:]}}}display color fields as hexagons
    instead of rectangles; also see {helpb hexplot}
    {p_end}
{synopt :{helpb heatplot##scatter:scatter{sf:[}({it:palette}){sf:]}}}display marker symbols instead of color fields
    {p_end}
{synopt :{helpb heatplot##keylabels:{ul:keyl}abels({it:spec})}}determine how color fields are labelled in the legend
    {p_end}
{synopt :{helpb heatplot##ramp:ramp{sf:[}({it:options}){sf:]}}}display a color ramp instead of the legend
    {p_end}
{synopt :{helpb heatplot##p:p{sf:[}{it:#}{sf:]}({it:area_options})}}detailed rendering of color fields
    {p_end}
{synoptline}

{synoptset 22}{...}
{marker yxopt}{synopthdr:yx_options}
{synoptline}
{synopt :{helpb heatplot##bins:{sf:[}x{sf:|}y{sf:]}bins({it:spec})}}how
    continuous {it:y} and {it:x} are binned
    {p_end}
{synopt :{helpb heatplot##bins:{sf:[}{ul:x}{sf:|}{ul:y}{sf:]}{ul:bw}idth({it:spec})}}how
    continuous {it:y} and {it:x} are binned; alternative to {cmd:bins()}
    {p_end}
{synopt :{helpb heatplot##bcuts:{sf:[}{ul:x}{sf:|}{ul:y}{sf:]}{ul:bc}uts({it:numlist})}}how
    continuous {it:y} and {it:x} are binned; alternative to {cmd:bins()}
    {p_end}
{synopt :{helpb heatplot##discrete:{sf:[}{ul:x}{sf:|}{ul:y}{sf:]}{ul:discr}ete{sf:[}({it:#}){sf:]}}}treat variables
    as discrete and omit binning
    {p_end}
{synopt :{helpb heatplot##clip:{sf:[}{ul:l}{sf:|}{ul:r}|{ul:b}{sf:|}{ul:t}{sf:]}clip}}clip color fields at outer bounds
    {p_end}
{synopt :{helpb heatplot##fillin:{ul:fill}in({it:#} {sf:[}{it:#}{sf:]})}}(syntax 1 only) fill in empty combinations of (binned)
    {it:y} and {it:x} by setting {it:z} to {it:#}
    {p_end}
{synoptline}

{synoptset 22}{...}
{marker mopt}{synopthdr:matrix_options}
{synoptline}
{synopt :{helpb heatplot##drop:drop({it:numlist})}}drop elements equal to one of the values in {it:numlist}
    {p_end}
{synopt :{helpb heatplot##lower:lower}}only display lower triangle
    {p_end}
{synopt :{helpb heatplot##upper:upper}}only display upper triangle
    {p_end}
{synopt :{helpb heatplot##nodiagonal:{ul:nodiag}onal}}omit diagonal
    {p_end}
{synopt :{helpb heatplot##equations:{ul:eq}uations{sf:[}({it:line_opts}){sf:]}}}(syntax 3 only) label and outline equations
    {p_end}
{synoptline}

{synoptset 22}{...}
{marker gropt}{synopthdr:graph_options}
{synoptline}
{synopt :{helpb heatplot##nograph:{ul:nogr}aph}}do not create a graph
    {p_end}
{synopt :{helpb heatplot##label:{sf:[}{ul:no}{sf:]}{ul:l}abel}}do/do not use variable/value labels
    {p_end}
{synopt :{helpb heatplot##addplot:addplot({it:plot})}}add other plots to the graph
    {p_end}
{synopt :{helpb heatplot##addplotnopreserve:{ul:addplotnopr}eserve}}technical option relevant for {cmd:addplot()}
    {p_end}
{synopt :{helpb heatplot##by:by({it:varlist}{sf:[}, {it:byopts}{sf:]})}}(syntax 1 only) repeat plot by subgroups
    {p_end}
{synopt :{helpb heatplot##twopts:{it:twoway_options}}}general twoway options
    {p_end}
{synoptline}

{synoptset 22}{...}
{marker genopt}{synopthdr:generate_options}
{synoptline}
{synopt :{helpb heatplot##generate:{ul:gen}erate{sf:[}({it:namelist}){sf:]}}}store
    the plotted data as variables
    {p_end}
{synopt :{helpb heatplot##replace:{ul:r}eplace}}allow overwriting existing variables
    {p_end}
{synopt :{helpb heatplot##nopreserve:{ul:nopres}erve}}replace the original data by
    the plotted data
    {p_end}
{synoptline}

{pstd}
    {cmd:fweight}s, {cmd:aweight}s, {cmd:iweight}s, and {cmd:pweight}s are allowed with Syntax 1; see help {help weight}.


{title:Description}

{pstd}
    {cmd:heatplot} creates heat plots from variables or matrices. One
    example of a heat plot is a two-dimensional histogram in which the
    frequencies of combinations of binned {it:y} and {it:x} are displayed as
    rectangular (or hexagonal) fields using a color gradient. Another example
    is a plot of a trivariate distribution where the color gradient is used to
    visualize the (average) value of {it:z} within bins of {it:y} and
    {it:x}. Yet another example is a plot that displays the contents of a matrix,
    say, a correlation matrix or a spacial weights matrix, using a color
    gradient. For a selection of different applications, see the
    {help heatplot##examples:Examples} below.


{title:Dependencies}

{pstd}
    {cmd:heatplot} requires {cmd:palettes} (Jann 2018) and, in Stata 14.2 or newer,
    {cmd:colrspace} (Jann 2019a). To install these packages, type

        {com}. ssc install palettes, replace
        . ssc install colrspace, replace{txt}

{pstd}
    The {cmd:fast} option requires {cmd:gtools} (Caceres Bravo 2018). To install
    {cmd:gtools}, type

        {com}. ssc install gtools, replace
        . gtools, upgrade{txt}


{title:Options}

{dlgtab:z_options}

{marker levels}{...}
{phang}
    {opt levels(#)} sets the number of color bins into which {it:z} (or the observed frequencies, if
    {it:z} is omitted) is categorized, using a regular grid of intervals from the observed
    minimum of (aggregated) {it:z} to the observed maximum. The intervals are right-open, except
    for the last interval interval, which is right-closed. Only one of
    {cmd:levels()} and {cmd:cuts()} is allowed.

{marker cuts}{...}
{phang}
    {opt cuts(numlist)} specifies the thresholds used to categorize {it:z},
    where {it:numlist} is an ascending number list; see help
    {it:{help numlist}}. The intervals defined by the number list are right-open, except
    for the last interval, which is right-closed. If the smallest number is
    larger than the observed minimum of (aggregated) {it:z}, an extra interval
    will be added at the bottom. Likewise, if the largest number is smaller than the
    maximum, an extra interval will be added at the top. Within the number list,
    you can use {cmd:@min} and {cmd:@max} to denote the
    observed minimum and maximum, respectively. For example, you could type

            {cmd:@min(20)@max}

{pmore}
    to create intervals from the minimum to the maximum in steps of 20. Furthermore,
    expressions specified as {cmd:{c -(}}{it:{help exp}}{cmd:{c )-}} will be evaluated
    before expanding the number list. For example, you could type

            {cmd:{c -(}@min-.5{c )-}(1){@max+.5}}

{pmore}
    to create intervals from the observed minimum minus 0.5 to the observed maximum
    plus 0.5 in steps of 1.

{marker colors}{...}
{phang}
    {cmd:colors(}[{it:{help colorpalette##palette:palette}}] [{cmd:,}
    {it:{help colorpalette##opts:palette_options}}]{cmd:)}
    selects the color palette to be used for the bins of {it:z}. This also
    sets the number of bins, unless {cmd:levels()} or {cmd:cuts()} is specified. {it:palette} is any
    palette allowed by {helpb colorpalette} (which can also be a simple list of colors, see
    help {it:{help colorpalette##colorlist:colorlist}}) and {it:palette_options} are
    corresponding options. The default is {cmd:colors(viridis)}. For example, to use a
    red to blue HCL color scheme, you could type {cmd:colors(hcl bluered, reverse)}. In
    Stata versions older than 14.2, {cmd:heatplot} uses command
    {helpb colorpalette9} instead of {helpb colorpalette}
    and sets the default to {cmd:colors(hcl, viridis)}.

{marker backfill}{...}
{phang}
    {cmd:backfill}[{cmd:(}{it:options}{cmd:)}] fills the background (the plotregion)
    using the first color of the colors provided by {cmd:colors()}. This makes
    sense, for example, in a bivariate histogram. When applying {cmd:backfill},
    you may want to turn grid lines off; in most situations this can be achieved by
    typing {cmd:ylabel(, nogrid)} and/or {cmd:xlabel(, nogrid)}. {it:options} are:

{phang2}
    {cmdab:l:ast} uses the last color instead of the first color.

{phang2}
    {cmdab:i:nner} only colors the inner plotregion. The default is to color both,
    the inner and the outer plotregion.

{marker statistic}{...}
{phang}
    {opt statistic(stat)} sets the type of aggregation of z values within
    yx-bins. {opt statistic()} is only allowed in syntax 1 and 2 (in syntax 3,
    each cell of the specified matrix constitutes a separate color field; hence, there is
    no aggregation). {it:stat} can be any statistic supported by {helpb collapse}. In
    addition, {it:stat} can be {cmd:proportion} to compute proportions (rather
    than percentages) or {cmd:asis} to skip aggregation. The main purpose of
    {cmd:asis} is to save computer time in cases where no aggregation is
    needed; typically, you only want to specify {cmd:asis} if you are certain
    that all combinations of (binned) {it:y} and {it:x} are unique.

{pmore}
    In syntax 1, if variable {it:z} is provided, the default is
    {cmd:statistic(mean)}; if {it:z} is omitted, the default is
    {cmd:statistic(percent)}. In syntax 2, the default is {cmd:statistic(sum)},
    or, if {cmd:discrete} has been specified, {cmd:statistic(asis)}.

{marker fast}{...}
{phang}
    {opt fast} performs some of the computations using fast commands provided
    by {helpb gtools} (e.g. {helpb gcollapse} instead of official {helpb collapse}
    for aggregation). Use this option to speed up computations in very large
    datasets. The {helpb gtools} package (Caceres Bravo 2018) has to be
    installed on the system; see {browse "http://github.com/mcaceresb/stata-gtools"}
    for more information. Option {opt fast} is only allowed in syntax 1 and 2.

{marker normalize}{...}
{phang}
    {opt normalize} causes the (aggregated) z values to be normalized by the size
    of the area covered by a color field before assigning colors. For example, specifying
    {cmd:normalize} together with {cmd:statistic(proportion)} will visualize densities 
    instead of proportions. {cmd:normalize} may be useful if you clip the color
    fields using option {helpb heatplot##clip:clip} or if you apply option
    {helpb heatplot##bcuts:bcuts()} in a way such that color fields have different
    sizes. The area covered by a color field will be computed before rescaling
    the fields according to {helpb heatplot##size:size()} or
    {helpb heatplot##sizeprop:sizeprop}. If option
    {helpb heatplot##scatter:scatter} is specified, the sizes of the areas
    will be computed as if {cmd:scatter} was not specified. If
    option {helpb heatplot##hexagon:hexagon} is specified, the computations will
    be based on the (possibly clipped) shapes of the hexagons. {cmd:normalize}
    has no effect in syntax 3.

{marker transform}{...}
{phang}
    {opt transform(@exp)} causes the (aggregated and, possibly, normalized) z
    values to be transformed before assigning colors. {it:@exp} is
    an expression (see {it:{help exp}}) in which {cmd:@} acts as a placeholder
    for the values to be transformed. For example, to take the natural logarithm,
    type {cmd:transform(ln(@))}.

{marker size}{...}
{phang}
    {opt size}[{cmd:(}{it:spec}{cmd:)}] scales the sizes of the color fields. If
    {cmd:size} is specified without argument, the color fields will be scaled in
    proportion to the absolute value of (aggregated and, possibly,
    normalized) z. Alternatively, provide a custom source for the scaling as
    follows.

{pmore}
    In syntax 1, specify
    {cmd:size(}{it:exp}[{cmd:,} {cmdab:s:tatistic(}{it:stat}{cmd:)}]{cmd:)}
    to obtain the size information from {it:{help exp}} (typically, {it:exp}
    is a simple variable name). Observations for which {it:exp} is missing will
    {it:not} be excluded from the estimation sample; if {it:exp} is missing for
    all observations within a specific color field, the size of the field will
    be set to the minimum as set by {helpb heatplot##srange:srange()}.

{pmore}
    In syntax 2, specify
    {cmd:size(}{it:name}[{cmd:,} {cmdab:s:tatistic(}{it:stat}{cmd:)}]{cmd:)}
    to obtain the size information from Mata matrix {it:name}. The matrix must
    be numeric and have the same dimension as the main matrix.

{pmore}
    In syntax 3, specify {opt size(matname)} to obtain the size information
    from Stata matrix {it:matname}. The matrix must have the same dimension
    as the main matrix.

{pmore}
    In syntax 1 and 2, suboption {opt statistic(stat)} sets the type
    of aggregation, where {it:stat} can be any statistic supported by
    {helpb collapse} (the default is {cmd:mean}). Suboption {cmd:statistic()}
    is only relevant if the main {helpb heatplot##statistic:statistic()} option
    has not been set to {cmd:asis}. In any case, absolute values of the
    (possibly aggregated) information will be used for the scaling.

{marker sizeprop}{...}
{phang}
    {opt sizeprop} scales the size of the color fields in proportion to the
    relative frequency of the underlying data. Use {cmd:sizeprop} as an
    alternative to {cmd:size()}. {opt sizeprop} is only allowed in syntax 1.

{marker recenter}{...}
{phang}
    {opt recenter} moves the color fields such that their center is at
    the center of the included data (mean of y and x within the bin). This can
    be useful if {cmd:size()} or {cmd:sizeprop} has been specified. {cmd:recenter} is only allowed
    in syntax 1.

{marker srange}{...}
{phang}
    {cmd:srange(}{it:lo} [{it:up}]{cmd:)} sets the range of relative sizes of
    the color fields. {cmd:srange()} is only relevant if {cmd:size()} or
    {cmd:sizeprop} has been specified. Let {it:v}, {it:v}>=0, be the variable to which the
    field sizes should be proportional (e.g. relative frequencies). The field
    sizes are then computed as {it:lo} + {it:v}/max({it:v}) *
    ({it:up} - {it:lo}). The default is {it:lo}=0 and {it:up}=1, that is, the smallest
    possible field has size 0 (invisible) and the largest field has size 1
    (full size). Specify, for example, {cmd:srange(0.5)} to set the size of the
    smallest possible field to 0.5 (half of full size).

{marker missing}{...}
{phang}
    {opt missing}[{opt (options)}] displays color fields for
    combinations of (binned) y and x for which (aggregated)
    z is equal to missing. {it:options} are:

{phang2}
    {opt l:abel(label)} sets the text to be used for missing values in the legend. The
    default is {cmd:label("missing")}.

{phang2}
    {it:area_options} are options to affect the look of the missing
    value color fields; see help {it:{help area_options}}. The default is to
    display the fields in black.

{marker values}{...}
{phang}
    {opt values}[{opt (options)}] displays the z values (or other information)
    as marker labels in the middle of the color fields. {it:options} are:

{phang2}
    {opt l:abel(spec)} determines the source of the values of the labels. The
    default is to display the (aggregated and, possibly, normalized) z
    values. Alternatively, provide a custom source as follows.

{pmore2}
    In syntax 1, specify
    {cmd:label(}{it:exp}[{cmd:,} {cmdab:s:tatistic(}{it:stat}{cmd:)}]{cmd:)}
    to display the (aggregated) value of {it:{help exp}} (typically, {it:exp}
    is a simple variable name). Observations for which {it:exp} is missing will
    {it:not} be excluded from the estimation sample. The default for
    {it:stat} is {cmd:mean} if {it:exp} is numeric and {cmd:first} if {it:exp} is string.

{pmore2}
    In syntax 2, specify
    {cmd:label(}{it:name}[{cmd:,} {cmdab:s:tatistic(}{it:stat}{cmd:)}]{cmd:)}
    to obtain the values from Mata matrix {it:name}. The matrix must
    be numeric and have the same dimension as the main matrix. The default for
    {it:stat} is {cmd:mean}.

{pmore2}
    In syntax 3, specify {opt label(matname)} to obtain the values
    from Stata matrix {it:matname}. The matrix must have the same dimension
    as the main matrix.

{pmore2}
    In syntax 1 and 2, suboption {opt statistic(stat)} sets the type of
    aggregation, where {it:stat} can be any statistic supported by
    {helpb collapse}.  The suboption is only relevant if the main
    {helpb heatplot##statistic:statistic()} option has not been set to {cmd:asis}.

{phang2}
    {opt trans:form(@exp)} causes the values to be transformed before
    displaying. {it:@exp} is an expression (see {it:{help exp}}) in which
    {cmd:@} acts as a placeholder for the values to be transformed. The result
    of {it:@exp} may be numeric or string; for example, you could type
    {cmd:transform(cond(@>0, "+", cond(@<0, "-", "")))} to display "+" or "-"
    (or nothing) depending on whether the value is (strictly) positive or
    (strictly) negative.

{phang2}
    {opth sty:le(markerlabelstyle)} sets the overall style of the labels.

{phang2}
    {opth p:osition(clockposstyle)} specifies where the label is to be located
    relative to the middle of the color field. The default is {cmd:position(0)}
    (centered).

{phang2}
    {opth g:ap(size)} specifies how much space should be put between the
    label the middle of the color field. This is only relevant if {cmd:position()}
    is not 0.

{phang2}
    {opth ang:le(anglestyle)} specifies the angle of the text.

{phang2}
    {opth s:ize(textsizestyle)} specifies the size of the text.

{phang2}
    {opth c:olor(colorstyle)} specifies the color of the text. Default is
    {cmd:color(black)} (unless {cmd:style()} is specified in which case the
    color is determined by the selected style).

{phang2}
    {opth f:ormat(%fmt)} sets the display format for the values. This option is
    only useful if the labels are numeric.

{pmore}
    For more details on options {cmd:style()} through {cmd:format()} also see
    the corresponding options with {cmd:"mlab"} prefix in {it:{help marker_label_options}}.

{marker hexagon}{...}
{phang}
    {opt hexagon}[{opt (options)}] causes the color fields to be rendered as
    hexagons instead of rectangles; also see {helpb hexplot}. {cmd:hexagon} and
    {cmd:scatter} are not both allowed. {it:options} are:

{phang2}
    {opt hor:izontal} arranges the hexagons horizontally. The default is to arrange
    the hexagons vertically.

{phang2}
    {opt left} starts with a left-shifted hexagon row. The default is to start 
    with a right-shifted row. If {cmd:horizontal} is specified, {cmd:left}
    starts with an down-shifted row instead of an up-shifted row.

{phang2}
    {opt odd} uses an odd number of hexagon columns. The default is to use
    an even number of columns. That is, by default the bins on the x-axis are
    constructed in a way such that each bin contains a double column of hexagons,
    yielding an even overall number of columns. Specify {cmd:odd} to construct the
    bins in a way such that the last bin only contains a single column. If 
    {cmd:horizontal} is specified, {cmd:odd} affects the number of
    rows rather than columns.

{marker scatter}{...}
{phang}
    {cmd:scatter}[{cmd:(}{it:{help symbolpalette##palette:palette}} [{cmd:,}
    {it:{help symbolpalette##opts:palette_options}}]{cmd:)}] causes the heat
    plot to be rendered as a scatter plot, with markers placed at the centers
    of the bins. Only one of {cmd:scatter} and {cmd:hexagon} is allowed. {it:palette}
    is any palette allowed by {helpb symbolpalette} (which can also be a simple list of
    symbol styles, see help {it:{help symbolpalette##symbollist:symbollist}}) and
    {it:palette_options} are corresponding options. The default is
    {cmd:scatter(circle)}. If less symbol styles are specified than there are
    bins of {it:z}, the symbols will be recycled.

{marker keylabels}{...}
{phang}
    {opt keylabels(spec)} selects the legend keys to be labeled and affects the
    formatting of the labels. {it:spec}
    is

            [{it:rule}] [{cmd:,} {it:suboptions} ]

{pmore}
    where {it:rule} may be

{p2colset 13 23 25 2}{...}
{p2col:{it:{help numlist}}}label the specified keys, where 1 refers to the first key (lowest value), 2 to the second, etc.
    {p_end}
{p2col:{cmd:minmax}}label the lower boundary of the first key and the upper boundary of the last key
    {p_end}
{p2col:{cmd:none}}omit the labels
    {p_end}
{p2col:{cmd:all}}label all keys; this is the default unless there are more than 24 keys
    {p_end}

{pmore}
    and suboptions are:

{phang2}
    {opth f:ormat(%fmt)} sets the display format. The default is {cmd:%7.0g}.

{phang2}
    {opt trans:form(@exp)} causes the values to be transformed before being
    displayed. {it:@exp} is an expression in which {cmd:@} acts as
    a placeholder for the values to be transformed. Typically, {it:@exp} will be
    the inverse of the main {helpb heatplot##transform:transform()}
    option. Example: {cmd:transform(ln(@))} would go along with
    {cmd:keylab(,transform(exp(@))}.

{phang2}
    {opt inter:val} uses interval notation for the key labels, e.g. [0,10),
    [10,20), etc. Only one of {cmd:interval} and {cmd:range()}
    is allowed. The default is to display interval midpoints. {cmd:interval}
    has no effect if {it:rule} is {cmd:minmax}.

{phang2}
    {opt ran:ge(#)} uses range notation for the key labels. Argument {it:#} specifies by how much the upper
    bound should be reduced. For example, {cmd:range(1)} would display interval [10,20) as "10-19", whereas
    {cmd:range(0.1)} would display the interval as "10-19.9". You may want to set an appropriate
    display format when specifying {cmd:range()}; see {cmd:format()} above. {cmd:interval} and {cmd:range()}
    are not both allowed. {cmd:range()} has no effect if {it:rule} is {cmd:minmax}.

{phang2}
    {opt area} displays legend keys as areas (rectangles) even if {cmd:scatter} has been specified.

{phang2}
    {it:textbox_options} are general options to affect the rendering of the
    labels, such as {cmd:size()}; see {it:{help textbox_options}}.

{phang2}
    {it:legend_options} are further options affecting the rendering of the
    legend; see {it:contents} and {it:location} in help {it:{help legend_options}}.

{marker ramp}{...}
{phang}
    {cmd:ramp}[{cmd:(}{it:options}{cmd:)}] renders the legend as a color ramp
    instead of using {helpb graph}'s legend
    option. Internally, if {cmd:ramp} is specified, two graphs are created, one
    for the main plot and one for the color ramp; these plots are then combined into a single graph
    using {helpb graph combine}. {it:options} are:

{phang2}
    {opt l:eft}, {opt r:ight}, {opt t:op}, or {opt b:ottom} specify the location
    of the ramp on the final graph. The location also affects the orientation of
    the ramp. In case of {cmd:top} or {cmd:bottom}, the ramp
    will be oriented horizontally; in case of {cmd:left} or {cmd:right}, the ramp
    will be oriented vertically. The default is {cmd:bottom}.

{phang2}
    {opt lab:els(rule_or_values)} specifies how the axis of the ramp
    should be labeled and ticked, where {it:rule_or_values} is as described in
    {it:{help axis_label_options}}. You may specify {cmd:@min}
    and {cmd:@max} to refer to the lower bound of the first interval and the
    upper bound of the last interval. For example, you could type
    {cmd:labels(@min .5 @max)} to place a label at the minimum, at 0.5, and at the maximum. Various
    suboptions are available to control the rendering of the labels and ticks; see
    {it:{help axis_label_options}}.

{phang2}
    {opth f:ormat(%fmt)} sets the display format for the labels. The default
    is {cmd:%7.0g}.

{phang2}
    {opt l:ength(#)} sets the length of the ramp as a percentage of the
    available space (the graph's width or height, depending on the orientation
    of the ramp). In horizontal orientation the default is {cmd:length(80)}; in
    vertical orientation the default is {cmd:length(60)}.

{phang2}
    {opt s:pace(#)} specifies the space to be consumed by the plot containing
    the ramp, as a percentage of the overall size of the graph. In horizontal
    orientation the default is {cmd:space(12)}; in
    vertical orientation the default is {cmd:space(20)}.

{phang2}
    {opt trans:form(@exp)} causes the ramp to be displayed on a transformed
    scale. {it:@exp} is an expression in which {cmd:@} acts as
    a placeholder for the values to be transformed. Typically, {it:@exp} will be
    the inverse of the main {helpb heatplot##transform:transform()}
    option. For example, {cmd:transform(ln(@))} would go along with
    {cmd:ramp(transform(exp(@))}.

{phang2}
    {opt c:ombine(combine_options)} are options to be passed through to
    {helpb graph combine}, such as {it:{help region_options}}. Note that the
    following options will be collected from the main options
    and passed through to {helpb graph combine} automatically: {cmd:title()},
    {cmd:subtitle()}, {cmd:note()}, {cmd:caption()}, {cmd:ysize()},
    {cmd:xsize()}, {cmd:nodraw}, {cmd:scheme()}, {cmd:name()}, and
    {cmd:saving()}.

{phang2}
    {it:{help twoway_options}} are general options to be applied to the plot
    containing the color ramp.

{marker p}{...}
{phang}
    {opt p}[{it:#}]{opt (area_options)} provides options to affect the rendering of
    the color fields; see help {it:{help area_options}}. For example, if you want
    the fields to have black outlines, you can type {cmd:p(lcolor(black) lalign(center))}. Unnumbered
    option {cmd:p()} affects all color fields. In addition, to address only the fields
    corresponding a specific level of z, you can type {cmd:p}{it:#}{cmd:()}
    where {it:#} is the number of the level. For example, if you want the color fields
    of the 3rd level to have red outlines, you could type
    {cmd:p3(lcolor(red))}.

{dlgtab:yx_options}

{marker bins}{...}
{phang}
    {opt bins(spec)} or {opt bwidth(spec)}, {opt ybins(spec)} or {opt ybwidth(spec)},
    and {opt xbins(spec)} or {opt xbwidth(spec)} specify how
    {it:y} and {it:x} are binned. These options are only allowed with
    continuous variables. {cmd:bins()}/{cmd:bwidth()} affects both,
    y and x. {cmd:ybins()}/{cmd:ybwidth()} and
    {opt xbins()}/{cmd:xbwidth()} only affect y or x, respectively, taking precedence over
    {cmd:bins()}/{cmd:bwidth()}. {it:spec} is

            [{it:n} {it:lb} {it:ub}] [{cmd:,} {it:suboptions} ]

{pmore}
    for {cmd:bins()} and

            [{it:width} {it:lb} {it:ub}] [{cmd:,} {it:suboptions} ]

{pmore}
    for {cmd:bwidth()} where

{p2colset 13 23 25 2}{...}
{p2col:{it:n}}number of bins
    {p_end}
{p2col:{it:width}}bin width
    {p_end}
{p2col:{it:lb}}midpoint (or lower bound) of first bin
    {p_end}
{p2col:{it:ub}}midpoint (or upper bound) of last bin
    {p_end}

{pmore}
    If neither {it:n} nor {it:width} is specified, the
    default is to set {it:n} to trunc(min(sqrt({it:N}), 10*ln({it:N})/ln(10))^(9/10))
    (or a fraction thereof if {it:lb} and {it:ub} define a range that is
    smaller than the observed data range), where {it:N} is the number of
    observations. If {it:lb} and {it:ub} are
    omitted, they are set to the observed minimum and maximum of the data,
    respectively. Default values can also be requested
    by typing "{cmd:.}" instead of providing a number. For example, you could type
    {cmd:bins(. 0 10)} to create bins from 0 to 10 using the default number
    of bins. Data outside the bin range
    defined by {it:lb} and {it:ub} will not be displayed, but will be taken into
    account when computing relative frequencies. {it:suboptions} are:

{phang2}
    {cmd:tight} makes the bins tight. By default {it:lb} and {it:ub} are
    interpreted as midpoints of the the first and last bins. Specify
    {cmd:tight} to treat {it:lb} and {it:ub} as outer bounds of the the first
    and last bins (in general, all bins are defined using right-open intervals;
    however, if {cmd:tight} is specified, observations equal to {it:ub} will
    be included in the last bin). If {cmd:tight} is specified together with {cmd:hexagon},
    the first and last bins are made as tight as possible given the shape and
    arrangement of the hexagons; all data falling into the hexagons will be
    taken into account even if lower than {it:lb} or larger than {it:ub}
    (unless option {cmd:clip} is specified).

{phang2}
    {cmd:ltight} makes the first bin tight, but leaves the last bin unchanged.

{phang2}
    {cmd:rtight} makes the last bin tight, but leaves the first bin unchanged.

{pmore}
    Note on setting the number of bins or the bin width for {cmd:hexagon}
    plots: In a grid of true hexagons, the vertical distance between hexagons
    midpoints is smaller than the horizontal distance by a factor of
    sqrt(3)/2 (or vice versa if {cmd:hexagon(horizontal)} is specified). If applying graph's
    {cmd:aspectratio(1)} option to produce a square plot, you may thus want to
    set the number of y-bins to about 2/sqrt(3) times the number x-bins. Likewise, if
    y and x have the same scale, you may want to set the width of y-bins to
    sqrt(3)/2 times the width of x-bins.

{marker bcuts}{...}
{phang}
    {opth bcuts(numlist)}, {opth ybcuts(numlist)} and {opth xbcuts(numlist)}
    specify how {it:y} and {it:x} are binned. Use these options as an alternative
    to [{cmd:y}|{cmd:x}]{cmd:bins()} or [{cmd:y}|{cmd:x}]{cmd:bwidth()}. {cmd:bcuts()}
    affects both, y and x; {cmd:ybcuts()} and {opt xbcuts()} only affect y or x,
    respectively, taking precedence over {cmd:bcuts()}. {it:numlist} is an (ascending)
    list of (at least two) cutpoints defining the bins. The bins are defined
    as right-open intervals from one cutpoint to the next (except for the last bin
    which is right-closed). Data smaller than the first cutpoint or larger then
    the last cutpoint will not be displayed, but will be taken into
    account when computing relative frequencies. Option {helpb heatplot##clip:clip}
    will have no effect for variables binned by {cmd:bcuts()}. Option
    {helpb heatplot##hexagon:hexagon} is not allowed with {cmd:bcuts()}.

{marker discrete}{...}
{phang}
    {opt discrete}[{opt (#)}], {opt ydiscrete}[{opt (#)}], and {opt xdiscrete}[{opt (#)}]
    specify that the variables are discrete and should not be binned. {cmd:discrete}
    affects both, y and x. {cmd:ydiscrete} and {opt xdiscrete} only affect y or x,
    respectively. Typically, treating variables as discrete only makes sense if
    their values are regularly spaced, as {cmd:heatplot} will print color fields
    centered at each observed value (furthermore, although allowed, specifying 
    {helpb heatplot##hexagon:hexagon} together with {cmd:discrete} does not
    lead to useful results in most situations). Optional argument {it:#}
    specifies the step width affecting the size of the color fields. The default
    step width is 1. Categorical variables specified as {cmd:i.}{it:varname} are always treated as
    discrete, but you can still use [{cmd:y}|{cmd:x}]{opt discrete(#)} to affect the size of the color fields
    in this case.

{marker clip}{...}
{phang}
    {cmd:clip}, {cmd:rclip}, {cmd:lclip}, {cmd:tclip}, and {cmd:bclip}
    cause the color fields to be clipped at the lower and upper bounds of the
    data range (or the range selected by {it:lb} and {it:ub} in {cmd:bins()} or {cmd:bwidth()}; data
    outside the clipped range will be omitted in this case). {cmd:clip}
    causes clipping on all four sides, {cmd:rclip} clips on the right,
    {cmd:lclip} clips on the left, {cmd:tclip} clips at the top, and {cmd:bclip}
    clips at the bottom (any combination is allowed). Option {helpb heatplot##normalize:normalize}
    will only take into account the remaining area of a field after clipping.

{marker fillin}{...}
{phang}
    {opt fillin(value [size])} fills in empty combinations of (binned) {it:y} and {it:x}
    by setting {it:z} to {it:value}. {it:value} can be {cmd:.} to set {it:z} to missing,
    which will be displayed if the {cmd:missing} option has been specified. Optional {it:size}
    is a number between 0 and 1 that sets the relative size of the color fields created by
    {cmd:fillin()}; this is only relevant if {cmd:size()} or {cmd:sizeprop}
    has been specified. {it:size} defaults to 1. {opt fillin()} is only allowed in
    syntax 1. See {helpb heatplot##backfill:backfill} for a more efficient (but less
    flexible) approach to color empty combinations of {it:y} and {it:x}.

{dlgtab:matrix_options}

{marker drop}{...}
{phang}
    {opt drop(numlist)} drops cells that have a value equal to one of the
    specified numbers. For example, type {cmd:drop(0)} to omit cells that contain 0.

{marker lower}{...}
{phang}
    {opt lower} causes only the lower triangle of the matrix to be displayed. Only one of {cmd:lower} and
    {cmd:upper} is allowed.

{marker upper}{...}
{phang}
    {opt upper} causes only the upper triangle of the matrix to be displayed. Only one of {cmd:upper} and
    {cmd:lower} is allowed.

{marker nodiagonal}{...}
{phang}
    {opt nodiagonal} omits the diagonal of the matrix.

{marker equations}{...}
{phang}
    {opt equations}[{cmd:(}{it:{help line_options}}{cmd:)}] uses the equation
    names of the matrix as axis labels, places ticks between equations, and
    draws outlines around diagonal equation areas. This
    can be useful, for example, if the matrix contains pairwise distances and the
    equations identify clusters. Use {it:{help line_options}} to affect the
    rendering of the outline. {opt equations()} is only allowed in
    syntax 3.

{dlgtab:graph_options}

{marker nograph}{...}
{phang}
    {opt nograph} omits creating a graph.

{marker label}{...}
{phang}
    [{cmd:no}]{opt label} specifies whether variable and value labels should be used or not. In syntax 1
    (plot from variables) the default is to use the variable labels of {it:y} and {it:x}
    as axis titles and, for categorical variables, the value labels as tick
    labels, if variable and value labels exist. Specify {cmd:nolabel} to
    use variable names and values instead. In syntax 3 (plot from Stata matrix) the default is to use
    row and column names as tick labels. Specify {cmd:label} to instruct {cmd:heatplot} to
    look for corresponding variables in the dataset and use their labels. In syntax 2, {cmd:label}
    has no effect.

{marker addplot}{...}
{phang}
    {opt addplot(plot)} provides a way to add other plots to the generated graph; see help {it:{help addplot_option}}.

{marker addplotnopreserve}{...}
{phang}
    {opt addplotnopreserve} drops the original data when drawing the graph even if
    {cmd:addplot()} has been specified. By default, {cmd:heatplot} temporarily deletes the
    original data to speed up drawing the graph. However, if {cmd:addplot()} is
    specified, the original data is preserved because {cmd:addplot()} might
    make use of it. If you are certain that {cmd:addplot()} does not make use of the original data
    (for example, because it only contains a {helpb twoway_scatteri:scatteri} plot), you can
    specify {cmd:addplotnopreserve} to save memory and computing time in large datasets.

{marker by}{...}
{phang}
    {opt by(varlist [, byopts])} specifies that the plot should be repeated for each set of values of {varlist}; see help
    {it:{help by_option}} (but note that suboption {cmd:total} is not supported). {cmd:by()} is only allowed in syntax 1. Computation
    of relative frequencies will be across all by-groups.

{marker twopts}{...}
{phang}
    {it:twoway_options} are any other options documented in help {it:{help twoway_options}}.

{dlgtab:generate_options}

{marker generate}{...}
{phang}
    {opt generate}[{opt (namelist)}] stores the plotted data as new
    variables. Depending on context, {cmd:generate()} might need to increase the
    number of rows in the dataset to store the variables (by default, five rows 
    are required per color field, the coordinates of the four corners plus
    missing as delimiter; if option {helpb heatplot##scatter:scatter} is specified, 
    only one row per field is required; if option {helpb heatplot##hexagon:hexagon}
    is specified, 7 or 9 rows are required depending on whether 
    {helpb heatplot##clip:clip} has been specified). The default
    variable names are:

            {cmd:_Z}       (aggregated) values of z
            {cmd:_Zid}     categorized z
            {cmd:_Y}       y midpoints
            {cmd:_Yshape}  y shape coordinates
            {cmd:_X}       x midpoints
            {cmd:_Xshape}  x shape coordinates
            {cmd:_Size}    field size (if relevant)
            {cmd:_Mlab}    marker label (if relevant)

{pmore}
    Alternatively, specify {it:{help namelist}} containing a custom list of
    variable names. If the list contains fewer elements than the number of
    variables to be generated, the above names are used for the remaining variables.

{pmore}
    If you only want to generate the variables without drawing a graph,
    apply the {helpb heatplot##nograph:nograph} option.

{marker replace}{...}
{phang}
    {opt replace} allows overwriting existing variables.

{marker nopreserve}{...}
{phang}
    {opt nopreserve} instructs {cmd:generate()} to replace the original data by
    the new variables. The default is to keep the original data.


{marker examples}{...}
{title:Examples}

{pstd}
    Also see {browse "http://ideas.repec.org/p/boc/usug19/24.html":Jann (2019b)}.

{dlgtab:Histograms}

{pstd}
    Bivariate histogram of weight and height:

        . {stata webuse nhanes2, clear}
        . {stata heatplot weight height, ylabel(25(25)175)}

{pstd}
    Using first color for background:

{p 8 12 2}
        . {stata heatplot weight height, ylabel(25(25)175, nogrid) backfill colors(magma, reverse)}

{pstd}
    Using hexagons instead of rectangles:

{p 8 12 2}
        . {stata heatplot weight height, ylabel(25(25)175, nogrid) backfill colors(magma, reverse) hexagon}

{pstd}
    Make size of hexagons proportional to the relative frequency and shift
    their midpoints to the empirical centers of the included data:

{p 8 12 2}
        . {stata heatplot weight height, ylabel(25(25)175, nogrid) backfill colors(magma, reverse) hexagon sizeprop recenter}

{pstd}
    Report counts instead of percentages and change labels in legend:

{p 8 12 2}
        . {stata heatplot weight height, ylabel(25(25)175, nogrid) backfill colors(magma, reverse) hexagon sizeprop recenter statistic(count) cuts(1(5)96 100) keylabels(, range(1))}

{dlgtab:Display color ramp instead of legend}

{pstd}
    By default, a legend produced by {helpb graph}'s legend option is
    displayed. Alternatively, use the {helpb heatplot##ramp:ramp} option to render the legend as
    a color ramp in a separate coordinate system (internally,
    {helpb graph combine} will be employed to combine the main plot and the
    ramp in a single graph):

        . {stata webuse nhanes2, clear}
        . {stata heatplot weight height, ramp}

{pstd}
    Place ramp on right, adjust the space used for the
    ramp, specify custom labels:

{p 8 12 2}
        . {stata heatplot weight height, ramp(right space(12) label(0(.1).9))}

{pstd}
    Use text labels and remove title:

{p 8 12 2}
        . {stata heatplot weight height, ramp(right label(@min "low" @max "high") subtitle(""))}

{pstd}
    Assign colors based on a transformed scale and retransform the ramp:

{p 8 12 2}
        . {stata heatplot weight height, stat(count) transform(ln(@)) ramp(transform(exp(@)))}

{dlgtab:Trivariate distributions}

{pstd}
    The following graph displays the gender distribution (proportion female) by weight and height:

{p 8 12 2}
        . {stata webuse nhanes2, clear}
{p_end}
{p 8 12 2}
        . {stata heatplot female weight height, hexagon ylabel(25(25)175) cuts(0(.05)1)}

{pstd}
    The same graph additionally taking into account relative frequencies:

{p 8 12 2}
        . {stata heatplot female weight height, hexagon ylabel(25(25)175) cuts(0(.05)1) sizeprop recenter}

{pstd}
    Distribution of the body mass index by gender and its relation to high blood pressure:

{p 8 12 2}
        . {stata heatplot highbp bmi i.female, xdiscrete(0.9) yline(18.5 25) cuts(0(.05).75) sizeprop recenter colors(inferno) plotregion(color(gs11)) ylabel(, nogrid)}

{pstd}
    Sea surface temperature by longitude, latitude, and date:

{p 8 12 2}
        . {stata sysuse surface, clear}
{p_end}
{p 8 12 2}
        . {stata heatplot temperature longitude latitude, bwidth(.5) statistic(asis) by(date, legend(off)) ylabel(30(1)38) aspectratio(1)}

{pmore}
     In this data, {cmd:longitude} and {cmd:latitude} are on a regular grid with a step width of half a degree. This is why
     we set the bin width to 0.5 using option {cmd:bwidth(.5)}. Furthermore, for each
     combination of {cmd:date}, {cmd:longitude}, and {cmd:latitude} there is only a single {cmd:temperature} measurement. This
     is why we can add option {cmd:statistic(asis)}. The option is not strictly needed, it just skips unnecessary
     computations.

{pstd}
    Same plot using hexagons:

{p 8 12 2}
        . {stata heatplot temperature longitude latitude, hexagon bwidth(.5) clip statistic(asis) by(date, legend(off)) ylabel(30(1)38) aspectratio(1)}

{pmore}
    Option {cmd:clip} has been specified to clip the hexagons at the outer bounds of the data.

{dlgtab:Correlation matrix}

{pstd}
    Correlation matrix including correlation coefficients as marker labels:

{p 8 12 2}
        . {stata sysuse auto, clear}
{p_end}
{p 8 12 2}
        . {stata correlate price mpg trunk weight length turn foreign}
{p_end}
{p 8 12 2}
        . {stata matrix C = r(C)}
{p_end}
{p 8 12 2}
        . {stata heatplot C, values(format(%9.3f)) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1)}

{pstd}
    Display only lower triangle and omit the diagonal:

{p 8 12 2}
        . {stata heatplot C, values(format(%9.3f)) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1) lower nodiagonal}

{pstd}
    An issue with correlation graph above is that the color gradient is not
    centered at zero. If using a diverging gradient, we would probably want the
    center of the gradient to denote a correlation of zero, and then
    use symmetric intervals on both sides. Use the {helpb heatplot##cuts:cuts()}
    option control how the intervals are constructed. Examples:

{p 8 12 2}
        . {stata heatplot C, color(hcl diverging, intensity(.6)) aspectratio(1) cuts(-1.05(.1)1.05)}
{p_end}
{p 8 12 2}
        . {stata heatplot C, color(hcl diverging, intensity(.6)) aspectratio(1) cuts(-1(`=2/15')1) keylabels(, interval)}
{p_end}

{pstd}
    As seen above, option {cmd:values()} can be used to display the values of the correlations
    with the color fields. It is also possible to print alternative information collected from
    a second matrix using suboption {cmd:label()}. In the following example, p-values are printed:

{p 8 12 2}
        . {stata pwcorr price mpg trunk weight length turn foreign, sig}
{p_end}
{p 8 12 2}
        . {stata matrix C = r(C)}
{p_end}
{p 8 12 2}
        . {stata matrix sig = r(sig)}
{p_end}
{p 8 12 2}
        . {stata heatplot C, values(label(sig) format(%9.3f)) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1)}

{pstd}
    Furthermore, suboption {cmd:transform()} can be used to edit the labels. Here is an
    example that marks non-significant correlations:

{p 8 12 2}
        . {stata heatplot C, values(label(sig) transform(cond(@>.05, "n.s.", ""))) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1)}


{dlgtab:Dissimilarity matrix with clusters}

{pstd}
    Illustration of the use of the {helpb heatplot##equations:equations()} option:

        . {stata sysuse lifeexp, clear}
        . {stata keep if gnppc<.}
        . {stata cluster wards popgrowth lexp gnppc}
        . {stata cluster generate N = groups(`=_N'), ties(fewer)}
        . {stata cluster generate G = groups(5)}
        . {stata sort G N}
        . {stata matrix dissim D = popgrowth lexp gnppc}
        . {stata `"mata: st_matrixcolstripe("D", strofreal(st_data(., "G N")))"'}
        . {stata `"mata: st_matrixrowstripe("D", strofreal(st_data(., "G N")))"'}
{p 8 12 2}
        . {stata heatplot D, equations(lcolor(red)) plotregion(margin(zero)) legend(off) aspectratio(1) xscale(alt)}

{dlgtab:Spacial weights matrix}

{pstd}
    Setup:

        . {stata "copy http://www.stata-press.com/data/r15/homicide1990.dta ."}
        . {stata "copy http://www.stata-press.com/data/r15/homicide1990_shp.dta ."}
        . {stata use homicide1990}
        . {stata spmatrix create contiguity W}      {it:(this may take a while)}
        . {stata spmatrix matafromsp W id = W}

{pstd}
    Heat plot with default settings, ignoring cells (i.e. weights) that are
    equal to zero:

        . {stata heatplot mata(W), drop(0) aspectratio(1)}

{pstd}
    Hexagon plot with fine-grained resolution:

        . {stata heatplot mata(W), drop(0) aspectratio(1) hexagon bins(100)}

{pstd}
    Plotting each cell individually using the {cmd:discrete} option:

{p 8 12 2}
        . {stata heatplot mata(W), drop(0) aspectratio(1) discrete color(black) p(lalign(center))}

{pmore}
    Since in this matrix all (non-zero) weights have the same value, we only need a single
    color, requested by {cmd:color(black)}. Furthermore, {cmd:p(lalign(center))} has been specified
    to prevent the individual color fields from becoming (almost) invisible.

{pstd}
    A very similar plot can also be produced using the {cmd:scatter} option:

{p 8 12 2}
        . {stata heatplot mata(W), drop(0) aspectratio(1) discrete color(black) scatter p(ms(p))}


{title:Returned results}

{p2colset 5 20 20 2}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{p2col : {cmd:r(N)}}number of observations
    {p_end}
{p2col : {cmd:r(levels)}}number of z levels (colors)
    {p_end}
{p2col : {cmd:r(y_k)}}number of y bins
    {p_end}
{p2col : {cmd:r(y_wd)}}y bin width
    {p_end}
{p2col : {cmd:r(y_lb)}}midpoint (or lower bound) of first y bin
    {p_end}
{p2col : {cmd:r(y_ub)}}midpoint (or upper bound) of last y bin
    {p_end}
{p2col : {cmd:r(x_k)}}number of x bins
    {p_end}
{p2col : {cmd:r(x_wd)}}x bin width
    {p_end}
{p2col : {cmd:r(x_lb)}}midpoint (or lower bound) of first x bin
    {p_end}
{p2col : {cmd:r(x_ub)}}midpoint (or upper bound) of last x bin
    {p_end}

{p2col 5 20 24 2: Macros}{p_end}
{p2col : {cmd:r(ztitle)}}legend title
    {p_end}
{p2col : {cmd:r(ytitle)}}y-axis title
    {p_end}
{p2col : {cmd:r(xtitle)}}x-axis title
    {p_end}
{p2col : {cmd:r(colors)}}list of color codes
    {p_end}
{p2col : {cmd:r(keylabels)}}legend keys
    {p_end}
{p2col : {cmd:r(eqcoords)}}coordinates of equation outlines
    {p_end}

{p2col 5 20 24 2: Matrices}{p_end}
{p2col : {cmd:r(cuts)}}cut points used to categorize z
    {p_end}


{title:References}

{phang}
    Caceres Bravo, M. (2018). GTOOLS: Stata module to provide a fast
    implementation of common group commands. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458514.html"} (also see
    {browse "http://github.com/mcaceresb/stata-gtools"}).
    {p_end}
{phang}
    Jann, B. (2018). {browse "https://www.stata-journal.com/article.html?article=gr0075":Color palettes for Stata graphics}. The Stata Journal
    18(4): 765-785.
    {p_end}
{phang}
    Jann, B. (2019a). ColrSpace: Mata class for color management. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458597.html"}.
    {p_end}
{phang}
    Jann, B. (2019b). Heat (and hexagon) plots in Stata. Presentation at London
    Stata Conference 2019. Available from {browse "http://ideas.repec.org/p/boc/usug19/24.html"}.
    {p_end}

{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2019). heatplot: Stata module to create heat plots and hexagon plots. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458598.html"}.


{title:Also see}

{psee}
    Online:  help for {helpb hexplot}, {helpb colorpalette},
    {helpb twoway contour}

