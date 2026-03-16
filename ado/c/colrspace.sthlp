{smcl}
{* 11dec2025}{...}
{cmd:help colrspace}{...}
{right:{browse "http://github.com/benjann/colrspace/"}}
({browse "http://ideas.repec.org/p/bss/wpaper/42.html":PDF manual}){...}
{right:{browse "http://repec.sowi.unibe.ch/stata/palettes/"}}
{hline}

{title:Title}

{pstd}
    {bf:ColrSpace -- Mata class for color management}


{title:Description}

{pstd}
    {cmd:ColrSpace} is a class-based color management system implemented in
    Mata. It supports a wide variety of color spaces and translations among
    them, provides color generators and a large collection of named palettes,
    and features functionality such as color interpolation, grayscale conversion,
    or color vision deficiency simulation.

{pstd}
    {cmd:ColrSpace} requires Stata 14.2 or newer.

{pstd}
    The examples below make use of the {helpb colorpalette} command, which is
    provided as part of the {cmd:palettes} package. Type

        {com}. ssc install palettes, replace{txt}

{pstd}
    to install the package.


{title:Contents}

    {help colrspace##cspace:Color spaces}
    {help colrspace##index:Alphabetical index of functions}
    {help colrspace##init:Initialize a ColrSpace object}
    {help colrspace##meta:Display contents and set meta data}
    Define and transform colors:
        {help colrspace##string:String input/output (Stata interface)}
        {help colrspace##palette:Color palettes and color generators}
        {help colrspace##opint:Set/retrieve opacity and intensity}
        {help colrspace##recycle:Recycle, select, and order colors}
        {help colrspace##ipolate:Interpolate and mix}
        {help colrspace##intensify:Intensify, saturate, luminate}
        {help colrspace##gray:Grayscale conversion}
        {help colrspace##cvd:Color vision deficiency simulation}
        {help colrspace##delta:Color differences and contrast ratios}
        {help colrspace##io:Import/export colors in various spaces}
        {help colrspace##util:Color converter and other utilities}
    Settings:
        {help colrspace##settings:Overview of color space settings}
        {help colrspace##rgbspace:RGB working space}
        {help colrspace##xyzwhite:XYZ reference white}
        {help colrspace##viewcond:CIECAM02 viewing conditions}
        {help colrspace##ucscoefs:Default coefficients for J'M'h and J'a'b'}
        {help colrspace##chadapt:Chromatic adaption method}
    {help colrspace##src:Source code and certification script}
    {help colrspace##ref:References}
    {help colrspace##author:Author}
    {help colrspace##alsosee:Also see}


{marker cspace}{...}
{title:Color spaces}

{pstd}
    The following diagram shows an overview of the different color spaces
    and coding schemes supported by {cmd:ColrSpace}:

                  {help colrspace##HEX:HEX}   {c TLC}{c -} {help colrspace##HSV:HSV}
                   |    {c |}
         ({help colrspace##RGBA:RGBA}) {c -} {help colrspace##RGB:RGB}   {c LT}{c -} {help colrspace##HSL:HSL}                  {help colrspace##xyY1:xyY1}
                   |    {c |}                        |
        ({help colrspace##RGBA1:RGBA1}) {c -} {helpb colrspace##RGB1:RGB1} {c -}{c BT}{c -} {help colrspace##CMYK1:CMYK1} {hline 1} {help colrspace##CMYK:CMYK}         {help colrspace##xyY:xyY}
                   |                             |
                  {help colrspace##lRGB:lRGB} {c -} ({help colrspace##chadapt:chromatic adaption}) {c -} {help colrspace##XYZ:XYZ} {c -} {help colrspace##XYZ1:XYZ1}
                                                 |
                        {c TLC}{hline 7}{c TT}{hline 7}{c TT}{hline 8}{c BRC}
                       {help colrspace##Lab:Lab}     {help colrspace##Luv:Luv}    {help colrspace##CAM02:CAM02 [{it:mask}]}
                        |       |       |
                       {help colrspace##LCh:LCh}     {help colrspace##HCL:HCL}     {help colrspace##JMh:JMh [{it:coefs}]}
                                        |
                                       {help colrspace##Jab:Jab [{it:coefs}]}

{pstd}
    The shown acronyms are the names by which the color spaces are referred to
    in {cmd:ColrSpace}. Internally, {cmd:ColrSpace} stores colors using
    their RGB1 values and additionally maintains an opacity value
    (alpha) in [0,1] and an intensity adjustment multiplier in [0,255] for each
    color.

{marker HEX}{...}
{phang}
    HEX is a hex RGB value (hex triplet; see
    {browse "http://en.wikipedia.org/wiki/Web_colors":Wikipedia 2019c}). Examples
    are {cmd:"#ffffff"} for white or {cmd:"#1a476f"} for Stata's navy. {cmd:ColrSpace} will
    always return HEX colors using their lowercase 6-digit codes. As input, however, uppercase spelling
    and 3-digit abbreviations are allowed. For example, white can be specified as
    are {cmd:"#ffffff"}, {cmd:"#FFFFFF"}, {cmd:"#fff"}, or {cmd:"#FFF"}.

{marker RGB}{...}
{phang}
    RGB is an RGB triplet (red, green, blue) in 0-255 scaling
    (see {browse "http://en.wikipedia.org/wiki/RGB_color_model":Wikipedia 2018f}). When
    returning RGB values, {cmd:ColrSpace} will round the values to integers and clip
    them at 0 and 255.

{marker RGB1}{...}
{phang}
    RGB1 is an RGB triplet in 0-1 scaling. {cmd:ColrSpace} does not clip or
    round the values and may thus return values larger than 1 or smaller than
    0. Using unclipped values ensures consistency of translations among
    different color spaces. To retrieve a matrix of clipped values, you can type
    {it:C} = {it:S}{cmd:.clip(}{it:S}{cmd:.get("RGB1"), 0, 1)}.

{pmore}
    RGB1 is the format in which {cmd:ColrSpace} stores colors internally. By
    default, {cmd:ColrSpace} assumes that the colors are in the standard RGB
    working space ({cmd:"sRGB"}), but this can be changed; see
    {help colrspace##rgbspace:Setting the RGB working space}. Note
    that changing the RGB working space after colors have been added to a
    {cmd:ColrSpace} object will not change the stored values. To transform
    colors from one RGB working space to another RGB working space, you could
    export the colors to XYZ typing {it:XYZ} = {it:S}{cmd:.get("XYZ")}, change
    the RGB working space using function {it:S}{cmd:.rgbspace()}, and
    then reimport the colors typing {it:S}{cmd:.set(}{it:XYZ}{cmd:, "XYZ")}.

{marker lRGB}{...}
{phang}
    lRGB stands for linear RGB in 0-1 scaling, that is, {help colrspace##RGB1:RGB1}
    from which {help colrspace##gamma:gamma correction} has been removed.

{marker HSV}{...}
{phang}
    HSV is a color triplet in the HSV (hue, saturation, value) color space. Hue is
    in degrees of the color wheel (0-360), saturation and value are numbers
    in [0,1]. {cmd:ColrSpace} uses the procedure described in
    {browse "http://en.wikipedia.org/wiki/HSL_and_HSV":Wikipedia (2018d)}
    to translate between HSV and RGB.

{marker HSL}{...}
{phang}
    HSL is a color triplet in the HSL (hue, saturation, lightness) color space. Hue is
    in degrees of the color wheel (0-360), saturation and lightness are numbers
    in [0,1]. {cmd:ColrSpace} uses the procedure described in
    {browse "http://en.wikipedia.org/wiki/HSL_and_HSV":Wikipedia (2018d)}
    to translate between HSL and RGB.

{marker CMYK}{...}
{phang}
    CMYK is a CMYK quadruplet (cyan, magenta, yellow, black) in 0-255 scaling. When returning
    CMYK values, {cmd:ColrSpace} will round the values to integers and clip
    them at 0 and 255. There is no unique standard method to translate between
    CMYK and RGB, as translation is device-specific. {cmd:ColrSpace} uses the
    same translation as is implemented in official Stata (for
    CMYK to RGB see program {cmd:setcmyk} in file
    {stata viewsource color.class:color.class}; for RGB to CMYK see
    program {cmd:rgb2cmyk} in file
    {stata viewsource palette.ado:palette.ado}).

{marker CMYK1}{...}
{phang}
    CMYK1 is a CMYK quadruplet (cyan, magenta, yellow, black) in 0-1 scaling. {cmd:ColrSpace} does
    not clip or round the values and may thus return values larger than 1 or smaller than 0. To
    retrieve a matrix of clipped values, you can type
    {it:C} = {it:S}{cmd:.clip(}{it:S}{cmd:.get("CMYK1"), 0, 1)}. See
    {help colrspace##CMYK:CMYK} for additional explanations.

{marker XYZ}{...}
{phang}
    XYZ is a CIE 1931 XYZ tristimulus value in Y_white = 100 scaling. See
    {browse "http://en.wikipedia.org/wiki/CIE_1931_color_space":Wikipedia (2018a)} for
    background information. XYZ values are defined with respect to a reference
    white; see {help colrspace##xyzwhite:Setting the XYZ reference white}. The
    default illuminant used by {cmd:ColrSpace} to define the reference white is {cmd:"D65"} (noon
    daylight for a CIE 1931 2° standard observer). To transform
    RGB to CIE XYZ, {cmd:ColrSpace} first removes
    {help colrspace##gamma:gamma correction} to obtain
    linear RGB (lRGB) and then transforms lRGB to XYZ using an appropriate
    transformation matrix (see, e.g.,
    {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf":Pascale 2003} for
    detailed explanations of both steps),
    possibly applying {help colrspace##chadapt:chromatic adaption} to take account
    of a change in the reference white between the RGB working space and the
    XYZ color space.

{marker XYZ1}{...}
{phang}
    XYZ1 is a CIE XYZ tristimulus value in Y_white = 1 scaling. See
    {help colrspace##XYZ:XYZ} for additional explanations.

{marker xyY}{...}
{phang}
    xyY is a CIE xyY triplet, where x (cyan to red for y around .2) and y (magenta to
    green for x around .2) are the chromaticity coordinates in [0,1], with x + y <= 1, and Y is
    the luminance in Y_white = 100 scaling (Y in CIE xyY is the same as Y in
    CIE XYZ). {cmd:ColrSpace} uses the procedure described in
    {browse "http://en.wikipedia.org/wiki/CIE_1931_color_space":Wikipedia (2018a)}
    to translate between XYZ and xyY.

{marker xyY1}{...}
{phang}
    xyY1 is a CIE xyY triplet, with Y in Y_white = 1 scaling. See
    {help colrspace##xyY:xyY} for additional explanations.

{marker Lab}{...}
{phang}
    Lab is a color triplet in the CIE L*a*b* color space. L* in [0,100] is the
    lightness of the color, a* is the green (-) to red (+) component, b* is the
    blue (-) to yellow (+) component. The range of a* and b* is somewhere
    around +/- 100 for typical colors. {cmd:ColrSpace} uses the procedure described in
    {browse "http://en.wikipedia.org/wiki/CIELAB_color_space":Wikipedia (2018b)}
    to translate between XYZ and CIE L*a*b*.

{marker LCh}{...}
{phang}
    LCh is a color triplet in the CIE LCh color space
    (cylindrical representation of CIE L*a*b*). L (lightness) in [0,100] is the same as L* in
    CIE L*a*b*, C (chroma) is the relative colorfulness (with typical values in a range
    of 0-100, although higher values are possible), h (hue) is the angle on the color wheel
    in degrees (0-360). See
    {browse "http://en.wikipedia.org/wiki/CIELAB_color_space":Wikipedia (2018b)}.

{marker Luv}{...}
{phang}
    Luv is a color triplet in the CIE L*u*v* color space. L* in [0,100] is the
    lightness of the color, u* is the green (-) to red (+) component, v* is the
    blue (-) to yellow (+) component. The range of u* and v* is somewhere
    around +/- 100 for typical colors. {cmd:ColrSpace} uses the procedure described in
    {browse "http://en.wikipedia.org/wiki/CIELUV":Wikipedia (2018c)}
    to translate between XYZ and CIE L*u*v*. L* in CIE L*u*v* is the same
    as L* in CIE L*a*b*.

{marker HCL}{...}
{phang}
    HCL is a color triplet in the HCL color space (cylindrical
    representation of CIE L*u*v*). H (hue) is the angle on the color wheel
    in degrees (0-360), C (chroma) is the relative colorfulness (with
    typical values in a range of 0-100, although higher values are possible),
    L (lightness) in [0,100] is the same as L* in CIE L*u*v*. See
    {browse "http://en.wikipedia.org/wiki/CIELUV":Wikipedia (2018c)}.

{marker CAM02}{...}
{phang}
    CAM02 is a color value in the CIECAM02 color space. See
    {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013)}
    for details. In {cmd:ColrSpace}, CIECAM02 is specified as

            {cmd:"CAM02 }[{it:mask}]{cmd:"}

{pmore}
    where optional {it:mask} selects the CIECAM02 attributes. The supported
    attributes are {cmd:Q} (brightness), {cmd:J} (lightness), {cmd:M} (colourfulness),
    {cmd:C} (chroma), {cmd:s} (saturation), {cmd:h} (hue angle), and {cmd:H}
    (hue composition). For example, you could type

            C = {it:S}{cmd:.get("CAM02 QJMCshH")}

{pmore}
    to obtain a {it:n} x 7 matrix containing all available attributes for each
    color. When importing colors, e.g. using {it:S}{cmd:.colors()} or
    {it:S}{cmd:.set()}, {it:mask} must contain at least one of {cmd:Q} and
    {cmd:J}, at least one of {cmd:M}, {cmd:C}, and {cmd:s}, and at least one of
    {cmd:h} and {cmd:H}. If {it:mask} is omitted, {cmd:ColrSpace} assumes
    {cmd:"CAM02 JCh"}.

{marker JMh}{...}
{phang}
    JMh is a color triplet in the CIECAM02-based perceptually uniform
    J'M'h color space. See
    {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013, chapter 2.6.1)}
    and {browse "http://doi.org/10.1002/col.20227":Luo et al. (2006)}
    for details. In {cmd:ColrSpace}, J'M'h is specified as

            {cmd:"JMh }[{it:coefs}]{cmd:"}

{pmore}
    where optional {it:coefs} selects the transformation coefficients. {it:coefs} can be

                {cmd:UCS}
            or  {cmd:LCD}
            or  {cmd:SCD}
            or  {it:K_L} {it:c_1} {it:c_2}

{pmore}
    (lowercase spelling and abbreviations allowed). {bind:{cmd:"JMh UCS"}} is equivalent
    to {bind:{cmd:"JMh 1 .007 .0228"}}, {bind:{cmd:"JMh LCD"}} is equivalent to
    {bind:{cmd:"JMh .77 .007 .0053"}}, {bind:{cmd:"JMh SCD"}}
    is equivalent to {bind:{cmd:"JMh 1.24 .007 .0363"}}. If {it:coefs} is omitted,
    the default coefficients as set by {help colrspace##ucscoefs:{it:S}{bf:.ucscoefs()}}
    will be used.

{marker Jab}{...}
{phang}
    Jab is a color triplet in the CIECAM02-based perceptually uniform
    J'a'b' color space. See
    {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013, chapter 2.6.1)}
    and {browse "http://doi.org/10.1002/col.20227":Luo et al. (2006)}
    for details. In {cmd:ColrSpace}, J'a'b' is specified as

            {cmd:"Jab }[{it:coefs}]{cmd:"}

{pmore}
    where optional {it:coefs} is as described in {help colrspace##JMh:JMh}.

{marker RGBA}{...}
{phang}
    RGBA is an opacity-extended RGB value (red, green, blue, alpha), where
    red, green, and blue are in 0-255 scaling and alpha is a number in [0,1] (0 =
    fully transparent, 1 = fully opaque). RGBA is not directly supported by
    {it:S}{cmd:.convert()}, but is allowed as input or output
    format in functions such as {it:S}{cmd:.colors()}, {it:S}{cmd:.set()}, or
    {it:S}{cmd:.get()}. Alternatively, in {it:S}{cmd:.colors()}, you can use
    non-extended RGB and specify opacity using Stata's {it:{help colorstyle}}
    syntax; for example {cmd:"RGBA 26 71 111 0.7"} is equivalent to
    {cmd:"RGB 26 71 111%70"} or {cmd:"26 71 111%70"} (see the section on
    {help colrspace##string:String input/output} below). A further alternative
    is to manage opacity using {it:S}{cmd:.opacity()} or {it:S}{cmd:.alpha()} (see
    {help colrspace##opint:Set/retrieve opacity and intensity}).

{marker RGBA1}{...}
{phang}
    RGBA1 is an opacity-extended RGB value (red, green, blue, alpha), where
    red, green, and blue are in 0-1 scaling and alpha is a number in [0,1] (0 =
    fully transparent, 1 = fully opaque). See {help colrspace##RGBA:RGBA} for
    additional explanations.


{marker index}{...}
{title:Alphabetical index of functions}

{p2colset 5 25 27 2}{...}
{p2col:{helpb colrspace##init:ColrSpace()}}initialize a {cmd:ColrSpace} object{p_end}
{p2col:{helpb colrspace##io:{it:S}.add()}}add colors in particular space{p_end}
{p2col:{helpb colrspace##opint:{it:S}.alpha()}}set/retrieve opacity{p_end}
{p2col:{helpb colrspace##chadapt:{it:S}.chadapt()}}set chromatic adaption method{p_end}
{p2col:{helpb colrspace##clear:{it:S}.clear()}}remove all colors and meta data{p_end}
{p2col:{helpb colrspace##clearindex:{it:S}.clearindex()}}clear external look-up tables{p_end}
{p2col:{helpb colrspace##settings:{it:S}.clearsettings()}}clear color space settings{p_end}
{p2col:{helpb colrspace##clip:{it:S}.clip()}}helper function for clipping{p_end}
{p2col:{helpb colrspace##contrast:{it:S}.contrast()}}compute contrast ratios{p_end}
{p2col:{helpb colrspace##delta:{it:S}.delta()}}compute color differences{p_end}
{p2col:{helpb colrspace##describe:{it:S}.describe()}}displays contents of {it:S}{p_end}
{p2col:{helpb colrspace##select:{it:S}.drop()}}drop colors{p_end}
{p2col:{helpb colrspace##colipolate:{it:S}.colipolate()}}helper function for interpolation{p_end}
{p2col:{helpb colrspace##colipolate:{it:S}.colipolate_c()}}helper function for circular interpolation{p_end}
{p2col:{helpb colrspace##string:{it:S}.colors()}}string input/output (scalar){p_end}
{p2col:{helpb colrspace##string:{it:S}.Colors()}}string input/output (vector){p_end}
{p2col:{helpb colrspace##colrecycle:{it:S}.colrecycle()}}helper function for recycling{p_end}
{p2col:{helpb colrspace##convert:{it:S}.convert()}}convert colors between spaces{p_end}
{p2col:{helpb colrspace##cvalid:{it:S}.cvalid()}}check whether color is valid{p_end}
{p2col:{helpb colrspace##cvd:{it:S}.cvd()}}color vision deficiency simulation{p_end}
{p2col:{helpb colrspace##cvd:{it:S}.cvd_M()}}helper function to retrieve CVD matrix{p_end}
{p2col:{helpb colrspace##get:{it:S}.get()}}retrieve colors in particular space{p_end}
{p2col:{helpb colrspace##gray:{it:S}.gray()}}gray scale conversion{p_end}
{p2col:{helpb colrspace##info:{it:S}.info()}}color description input/output (scalar){p_end}
{p2col:{helpb colrspace##info:{it:S}.Info()}}color description input/output (vector){p_end}
{p2col:{helpb colrspace##intensify:{it:S}.intensify()}}adjust color intensity{p_end}
{p2col:{helpb colrspace##intensify:{it:S}.Intensify()}}apply existing intensity multipliers{p_end}
{p2col:{helpb colrspace##intensity:{it:S}.intensity()}}set/retrieve intensity multipliers{p_end}
{p2col:{helpb colrspace##ipolate:{it:S}.ipolate()}}interpolate colors{p_end}
{p2col:{helpb colrspace##isipolate:{it:S}.isipolate()}}whether interpolation has been applied{p_end}
{p2col:{helpb colrspace##lsmap:{it:S}.lsmap()}}helper function to create linear segmented colormaps{p_end}
{p2col:{helpb colrspace##luminate:{it:S}.luminate()}}adjust luminance of colors{p_end}
{p2col:{helpb colrspace##mix:{it:S}.mix()}}mix colors{p_end}
{p2col:{helpb colrspace##ncolors:{it:S}.N()}}retrieve number of colors{p_end}
{p2col:{helpb colrspace##name:{it:S}.name()}}set/retrieve name of color collection{p_end}
{p2col:{helpb colrspace##namedcolors:{it:S}.namedcolors()}}return index of available named colors{p_end}
{p2col:{helpb colrspace##names:{it:S}.names()}}color names input/output (scalar){p_end}
{p2col:{helpb colrspace##names:{it:S}.Names()}}color names input/output (vector){p_end}
{p2col:{helpb colrspace##note:{it:S}.note()}}set/retrieve description of color collection{p_end}
{p2col:{helpb colrspace##opint:{it:S}.opacity()}}set/retrieve opacity{p_end}
{p2col:{helpb colrspace##select:{it:S}.order()}}order colors{p_end}
{p2col:{helpb colrspace##palette:{it:S}.palette()}}retrieve colors from named palette{p_end}
{p2col:{helpb colrspace##palettes:{it:S}.palettes()}}return index of available palettes{p_end}
{p2col:{helpb colrspace##pclass:{it:S}.pclass()}}set/retrieve class of color collection{p_end}
{p2col:{helpb colrspace##pexists:{it:S}.pexists()}}check whether named palette exists{p_end}
{p2col:{helpb colrspace##recycle:{it:S}.recycle()}}recycle colors{p_end}
{p2col:{helpb colrspace##io:{it:S}.reset()}}reset colors in particular space{p_end}
{p2col:{helpb colrspace##select:{it:S}.reverse()}}reverse order of colors{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgbspace()}}set RGB working space{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgb_gamma()}}set/retrieve gamma correction{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgb_invM()}}set/retrieve XYZ-to-lRGB matrix{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgb_M()}}set/retrieve lRGB-to-XYZ matrix{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgb_white()}}set/retrieve RGB reference white{p_end}
{p2col:{helpb colrspace##rgbspace:{it:S}.rgb_xy()}}set/retrieve RGB primaries{p_end}
{p2col:{helpb colrspace##saturate:{it:S}.saturate()}}adjust saturation (chroma) of colors{p_end}
{p2col:{helpb colrspace##select:{it:S}.select()}}select colors{p_end}
{p2col:{helpb colrspace##io:{it:S}.set()}}set colors in particular space{p_end}
{p2col:{helpb colrspace##settings:{it:S}.settings()}}display color space settings{p_end}
{p2col:{helpb colrspace##select:{it:S}.shift()}}shift positions of colors{p_end}
{p2col:{helpb colrspace##source:{it:S}.source()}}set/retrieve source of color collection{p_end}
{p2col:{helpb colrspace##chadapt:{it:S}.tmatrix()}}retrieve transformation matrices{p_end}
{p2col:{helpb colrspace##ucscoefs:{it:S}.ucscoefs()}}set default J'M'h/J'a'b' coefficients{p_end}
{p2col:{helpb colrspace##viewcond:{it:S}.viewcond()}}set/retrieve CIECAM02 viewing conditions{p_end}
{p2col:{helpb colrspace##xyzwhite:{it:S}.xyzwhite()}}set/retrieve XYZ reference white{p_end}
{p2col:{helpb colrspace##chadapt:{it:S}.XYZ_to_XYZ()}}apply chromatic adaption{p_end}

{pstd}
    Several of the above functions also come in variants such as {it:S}{cmd:.add_}{it:name}{cmd:()},
    {it:S}{cmd:.}{it:name}{cmd:_added()}, or {it:S}{cmd:.add_}{it:name}{cmd:_added()},
    where {it:name} is the function name.


{marker init}{...}
{title:Initialize a ColrSpace object}

{dlgtab:Initialize}

{pstd}
    To initialize a new {cmd:ColrSpace} object, type

        {cmd:class ColrSpace scalar} {it:S}

{pstd}
    or

        {it:S} = {cmd:ColrSpace()}

{pstd}
    where {it:S} is the name of the object. After initialization, the object
    will be empty, that is, contain no colors. However, the object will be
    initialized with the following default color space settings:

        {help colrspace##rgbspace:{it:S}{bf:.rgbspace("sRGB")}}
        {help colrspace##xyzwhite:{it:S}{bf:.xyzwhite("D65")}}
        {help colrspace##viewcond:{it:S}{bf:.viewcond(20, 64/(5*pi()), "average")}}
        {help colrspace##ucscoefs:{it:S}{bf:.ucscoefs("UCS")}}
        {help colrspace##chadapt:{it:S}{bf:.chadapt("Bfd")}}

{pstd}
    Use {helpb colrspace##settings:{it:S}.settings()} to display the
    current color space settings of object {it:S}. To restore the above defaults,
    you can type {helpb colrspace##settings:{it:S}.clearsettings()}.

{marker clear}{...}
{dlgtab:Reinitialize}

{pstd}
    To reinitialize an existing {cmd:ColrSpace} object, type

        {it:S}{cmd:.clear()}

{pstd}
    This will remove all colors from {it:S}.

{pstd}
    Color space settings are not affected by {it:S}{cmd:.clear()}. Use
    {help colrspace##settings:{it:S}{bf:.clearsettings()}} if you want to reset the
    color space settings.

{pstd}
    The external look-up tables are not affected by {it:S}{cmd:.clear()}. Use
    {help colrspace##clearindex:{it:S}{bf:.clearindex()}} if you want to clear the
    look-up tables.

{marker clearindex}{...}
{dlgtab:Clear external look-up tables}

{pstd}
    Some of the functions below make use of look-up tables for palettes
    and named colors. {cmd:ColrSpace} stores these tables as external global
    objects for reasons of efficiency (the names of the external globals are
    {bf:ColrSpace_paletteindex} and {cmd:ColrSpace_namedcolorindex}). To
    ulink these objects from {it:S} and remove them from memory, type

        {it:S}{cmd:.clearindex()}

{pstd}
    The tables will be rebuilt automatically if a function is called that
    makes use of them.

{pstd}
    Note that the memory consumed by the look-up tables will only be freed if
    there are no other {cmd:ColrSpace} objects in memory that are linked to
    them. That is, {it:S}{cmd:.clearindex()} removes the links to the look-up
    tables in {it:S} and also drops the external globals, but it does not clear
    the links that may exist in other {cmd:ColrSpace} objects. If
    the tables are rebuilt, these other {cmd:ColrSpace} objects will remain linked
    to the old copy of the tables.


{marker meta}{...}
{title:Display contents and set meta data}

{marker describe}{...}
{dlgtab:Overview of contents}

{pstd}
    To display an overview of the contents of {it:S}, type

        {it:S}{cmd:.describe}{cmd:(}[{it:short}]{cmd:)}

{pstd}
    where {it:short}!=0 suppresses listing the individual colors. Examples:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.palette("HTML pink")"'}
        . {stata "mata: S.describe()"}
        . {stata "mata: S.describe(1)"}

{marker ncolors}{...}
{dlgtab:Number of colors}

{pstd}
    To retrieve the number of colors defined in {it:S}, type

        {it:n} = {it:S}{cmd:.N}[{cmd:_added}]{cmd:()}

{pstd}
    {it:S}{cmd:.N()} returns the total number of colors; {it:S}{cmd:.N_added()}
    returns the number of colors added last.

{marker name}{...}
{dlgtab:Collection name}

{pstd}
    To assign a name or title to the collection of colors in {it:S}, type

        {it:S}{cmd:.name(}{it:name}{cmd:)}

{pstd}
    where {it:name} is a string scalar. To retrieve the name, type

        {it:name} = {it:S}{cmd:.name()}

{marker pclass}{...}
{dlgtab:Class}

{pstd}
    To assign a class to the collection of colors in {it:S}, type

        {it:S}{cmd:.pclass(}{it:class}{cmd:)}

{pstd}
    where {it:class} is a string scalar such as {cmd:"qualitative"} (or
    {cmd:"categorical"}), {cmd:"sequential"}, {cmd:"diverging"}, or
    {cmd:"circular"} (or {cmd:"cyclic"}). To retrieve the class, type

        {it:class} = {it:S}{cmd:.pclass()}

{marker note}{...}
{dlgtab:Description}

{pstd}
    To assign a description to the collection of colors in {it:S}, type

        {it:S}{cmd:.note(}{it:note}{cmd:)}

{pstd}
    where {it:note} is a string scalar. To retrieve the description, type

        {it:note} = {it:S}{cmd:.note()}

{marker source}{...}
{dlgtab:Source}

{pstd}
    To assign information on the source of the colors in {it:S}, type

        {it:S}{cmd:.source(}{it:source}{cmd:)}

{pstd}
    where {it:source} is a string scalar. To retrieve the source, type

        {it:source} = {it:S}{cmd:.source()}

{marker isipolate}{...}
{dlgtab:Interpolation status}

{pstd}
    {cmd:ColrSpace} maintains a 0/1 flag of whether colors have been interpolated
    by {help colrspace##ipolate:{it:S}{bf:.ipolate()}}. To retrieve the status
    of the flag, type

        {it:flag} = {it:S}{cmd:.isipolate()}


{marker string}{...}
{title:String input/output (Stata interface)}

{dlgtab:Color input}

{pstd}
    To import colors from a string scalar {it:colors} containing a list of color
    specifications, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:colors(}{it:colors}[{cmd:,} {it:delimiter}]{cmd:)}

{pstd}
    or

        {it:rc} = {it:S}{cmd:.}{cmd:_}[{cmd:add_}]{cmd:colors(}{it:colors}[{cmd:,} {it:delimiter}]{cmd:)}

{pstd}
    where string scalar {it:delimiter} sets the character(s) delimiting the
    specifications; the default is to assume a space-separated list,
    i.e. {it:delimiter} = {cmd:" "}. To avoid breaking a specification that
    contains a delimiting character, enclose the specification in double
    quotes. {it:S}{cmd:.colors()} will replace preexisting colors in {it:S} by
    the new colors. Alternatively, use {it:S}{cmd:.add_colors()} to append the
    new colors to the existing colors. {it:S}{cmd:._colors()} and
    {it:S}{cmd:._add_colors()} perform the same action as {it:S}{cmd:.colors()}
    and {it:S}{cmd:.add_colors()}, but they return {it:rc} instead of aborting,
    if {it:colors} contains invalid color specifications. {it:rc} will be set
    to the index of the first offending color specification, or to 0 if all
    specifications are valid. Also see function
    {help colrspace##cvalid:{it:S}{bf:.cvalid()}} for a way to check whether a
    color specification is valid.

{pstd}
    To import colors from a string vector {it:Colors} (each
    element containing a single color specification), type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:Colors(}{it:Colors}{cmd:)}

{pstd}
    or

        {it:rc} = {it:S}{cmd:.}{cmd:_}[{cmd:add_}]{cmd:Colors(}{it:Colors}{cmd:)}

{pstd}
    The syntax for a single color specification is

        {it:color}[{cmd:%}{it:#}][*{it:#}]

{pstd}
    where {cmd:%}{it:#} sets the opacity (in percent; 0 = fully transparent,
    100 = fully opaque), {cmd:*}{it:#} sets the intensity adjustment multiplier
    (values between 0 and 1 make the color lighter; values larger than one
    make the color darker), and {it:color} is one of the following:

{marker strinput}{...}
{p2colset 9 28 30 2}{...}
{p2col:{it:name}}a color name; this includes official Stata's color names as
    listed in {help colorstyle##colorstyle:{it:colorstyle}}, possible user additions
    provided through style files, as well as a large collection of {help colrspace##colorlist:named colors}
    provided by {cmd:ColrSpace}{p_end}
{p2col:{cmd:#}{it:rrggbb}}6-digit hex RGB value; white = {cmd:#FFFFFF} or {cmd:#ffffff}, navy = {cmd:#1A476F} or {cmd:#1a476f} {p_end}
{p2col:{cmd:#}{it:rgb}}3-digit abbreviated hex RGB value; white = {cmd:#FFF} or {cmd:#fff}{p_end}
{p2col:{it:# # #}}RGB value in 0-255 scaling; navy = {cmd:"26 71 111"}{p_end}
{p2col:{it:# # # #}}CMYK value in 0-255 or 0-1 scaling; navy = {cmd:"85 40 0 144"} or {cmd:".333 .157 0 .565"}{p_end}
{p2col:{cmd:RGB }{it:# # #}}RGB value in 0-255 scaling; navy = {cmd:"RGB 26 71 111"}{p_end}
{p2col:{cmd:RGB1 }{it:# # #}}RGB value in 0-1 scaling; navy = {cmd:"RGB1 .102 .278 .435"} {p_end}
{p2col:{cmd:lRGB }{it:# # #}}linear RGB value in 0-1 scaling; navy = {cmd:"lRGB .0103 .063 .159"}{p_end}
{p2col:{cmd:CMYK }{it:# # # #}}CMYK value in 0-255 scaling; navy = {cmd:"CMYK 85 40 0 144"}{p_end}
{p2col:{cmd:CMYK1 }{it:# # # #}}CMYK value in 0-1 scaling; navy = {cmd:"CMYK1 .333 .157 0 .565"}{p_end}
{p2col:{cmd:HSV }{it: # # #}}HSV value; navy = {cmd:"HSV 208 .766 .435"}{p_end}
{p2col:{cmd:HSL }{it:# # #}}HSL value; navy = {cmd:"HSL 208 .620 .269"}{p_end}
{p2col:{cmd:XYZ }{it:# # #}}CIE XYZ value in 0-100 scaling; navy = {cmd:"XYZ 5.55 5.87 15.9"}{p_end}
{p2col:{cmd:XYZ1 }{it:# # #}}CIE XYZ value in 0-1 scaling; navy = {cmd:"XYZ1 .0555 .0587 .159"}{p_end}
{p2col:{cmd:xyY }{it:# # #}}CIE xyY value with Y in 0-100 scaling; navy = {cmd:"xyY .203 .215 5.87"}{p_end}
{p2col:{cmd:xyY1 }{it:# # #}}CIE xyY value with Y in 0-1 scaling; navy = {cmd:"xyY1 .203 .215 .0587"}{p_end}
{p2col:{cmd:Lab }{it:# # #}}CIE L*a*b* value; navy = {cmd:"Lab 29 -.4 -27.5"}{p_end}
{p2col:{cmd:LCh }{it:# # #}}LCh value (polar CIE L*a*b*); navy = {cmd:"LCh 29 27.5 269.2"}{p_end}
{p2col:{cmd:Luv }{it:# # #}}CIE L*u*v* value; navy = {cmd:"Luv 29 -15.4 -35.6"}{p_end}
{p2col:{cmd:HCL }{it:# # #}}HCL value (polar CIE L*u*v*); navy = {cmd:"HCL 246.6 38.8 29"}{p_end}
{p2col:{cmd:CAM02 }[{help colrspace##CAM02:{it:mask}}] {it:...}}CIECAM02 value according to {help colrspace##CAM02:{it:mask}}; navy = {cmd:"CAM02 JCh 20.2 37 245"} or {cmd:"CAM02 QsH 55.7 69.5 303.5"}{p_end}
{p2col:{cmd:JMh }[{help colrspace##JMh:{it:coefs}}] {it:# # #}}CIECAM02 J'M'h value; navy = {cmd:"JMh 30.1 21 245"}{p_end}
{p2col:{cmd:Jab }[{help colrspace##JMh:{it:coefs}}] {it:# # #}}CIECAM02 J'a'b' value; navy = {cmd:"Jab 30.1 -8.9 -19"} or {cmd:"Jab LCD 39 -10.6 -23"}{p_end}
{p2col:{cmd:RGBA }{it:# # # #}}RGB 0-255 value where the last number specifies the opacity in [0,1]{p_end}
{p2col:{cmd:RGBA1 }{it:# # # #}}RGB 0-1 value where the last number specifies the opacity in [0,1] {p_end}

{pmore}
    The colorspace identifiers (but not {it:mask}) can be typed in lowercase
    letters. The provided examples are for standard viewing conditions.

{marker colorlist}{...}
{pmore}
    The named colors provided by {cmd:ColrSpace} in addition to Stata's named colors are
    as follows (see {help colrspace_library_namedcolors:colrspace_library_namedcolors.sthlp}
    for the source file containing all color definitions):

{phang2}
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

{phang2}
    {browse "http://www.w3schools.com/w3css/w3css_color_material.asp":30 W3.CSS default colors}:
    {cmd:w3-red}, {cmd:w3-pink}, {cmd:w3-purple}, {cmd:w3-deep-purple},
    {cmd:w3-indigo}, {cmd:w3-blue}, {cmd:w3-light-blue}, {cmd:w3-cyan},
    {cmd:w3-aqua}, {cmd:w3-teal}, {cmd:w3-green}, {cmd:w3-light-green},
    {cmd:w3-lime}, {cmd:w3-sand}, {cmd:w3-khaki}, {cmd:w3-yellow}, {cmd:w3-amber},
    {cmd:w3-orange}, {cmd:w3-deep-orange}, {cmd:w3-blue-grey}, {cmd:w3-brown},
    {cmd:w3-light-grey}, {cmd:w3-grey}, {cmd:w3-dark-grey}, {cmd:w3-black},
    {cmd:w3-white}, {cmd:w3-pale-red}, {cmd:w3-pale-yellow}, {cmd:w3-pale-green},
    {cmd:w3-pale-blue}

{phang2}
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

{pmore}
    Color names can be abbreviated and typed in lowercase letters. If
    spelling is ambiguous, a choice will be made based on alphabetical order
    and capitalization (for example, type {cmd:pink} to select official Stata's
    pink, type {cmd:Pink} to select HTML color pink).

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("LightCyan MediumAqua BurlyWood")"'}
        . {stata "colorpalette mata(S)"}
        . {stata `"mata: S.add_colors("SeaShell Crimson")"'}
        . {stata "colorpalette mata(S)"}
        . {stata `"mata: S.colors("#337ab7, lab 50 -23 32, xyz 80 30 40, hcl 200 50 30", ",")"'}
        . {stata "colorpalette mata(S)"}
        . {stata `"mata: S.colors("navy*.5 orange%80 maroon*.7%60")"'}
        . {stata "colorpalette mata(S)"}

{dlgtab:Color output}

{pstd}
    To export colors into a string scalar containing a space-separated list of
    color specifications compatible with Stata graphics, type

        {it:colors} = {it:S}{cmd:.colors}[{cmd:_added}]{cmd:(}[{it:rgbforce}]{cmd:)}

{pstd}
    where {it:rgbforce}!=0 enforces exporting all colors using their in RGB
    values. Colors that have been defined in terms of their Stata color names
    are exported as is by default. Specify {it:rgbforce}!=0
    to export these colors as RGB values. {it:S}{cmd:.colors()} exports all
    colors; use {it:S}{cmd:.colors_added()} to export only the colors that have
    been added last.

{pstd}
    To export colors into a string column vector (each
    row containing a single color specification), type

        {it:Colors} = {it:S}{cmd:.Colors}[{cmd:_added}]{cmd:(}[{it:rgbforce}]{cmd:)}

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("navy*.5 orange%80 maroon*.7%60")"'}
        . {stata "mata: S.colors()"}
        . {stata "mata: S.colors(1)"}
        . {stata `"mata: S.add_colors("SeaShell Crimson")"'}
        . {stata "mata: S.colors_added()"}

{marker info}{...}
{dlgtab:Names input}

{pstd}
    To import information from a string scalar {it:names} containing a list of color
    names, type

        {it:S}{cmd:.names}[{cmd:_added}]{cmd:(}{it:names}[{cmd:,} {it:delimiter}]{cmd:)}

{pstd}
    where string scalar {it:delimiter} sets the character(s) delimiting the names;
    the default is to assume a space-separated list, i.e. {it:delimiter} = {cmd:" "}. To
    avoid breaking a name that contains a delimiting character, enclose
    the name in double quotes. {it:S}{cmd:.names()} affects all colors
    defined in {it:S}; use {it:S}{cmd:.names_added()} to affect only the
    colors that have been added last.

{pstd}
    To import names from a string vector {it:Names} (each
    element containing a single name), type

        {it:S}{cmd:.Names}[{cmd:_added}]{cmd:(}{it:Names}{cmd:)}

{pstd}
    Note that redefining the colors, e.g. by applying {help colrspace##string:{it:S}{bf:.colors()}}
    or {help colrspace##io:{it:S}{bf:.set()}}, will delete existing color names.

{pstd}
    Example (colors from {browse "http://getbootstrap.com/docs/3.3/"}):

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("#337ab7 #5cb85c #5bc0de #f0ad4e #d9534f")"'}
        . {stata `"mata: S.names("primary success info warning danger")"'}
        . {stata `"mata: S.name("colors from Bootstrap 3.3")"'}
        . {stata "colorpalette mata(S)"}

{pstd}
    Functions {help colrspace##string:{it:S}{bf:.colors()}} and
    {help colrspace##palette:{it:S}{bf:.palette()}} fill in names automatically
    for colors that have a name.

{dlgtab:Names output}

{pstd}
    To export color names into a string scalar containing a
    space-separated list of the descriptions, type

        {it:names} = {it:S}{cmd:.names}[{cmd:_added}]{cmd:()}

{pstd}
    {it:S}{cmd:.names()} exports names from all
    colors; use {it:S}{cmd:.names_added()} to export names only from the
    colors that have been added last.

{pstd}
    Alternatively, to export the names into a string column vector (each
    row containing a single name) type

        {it:Names} = {it:S}{cmd:.Names}[{cmd:_added}]{cmd:()}

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("SeaShell Crimson")"'}
        . {stata "mata: S.colors()"}
        . {stata "mata: S.names()"}

{marker info}{...}
{dlgtab:Description input}

{pstd}
    To import information from a string scalar {it:info} containing a list of color
    descriptions (e.g. color names or other text describing a color), type

        {it:S}{cmd:.info}[{cmd:_added}]{cmd:(}{it:info}[{cmd:,} {it:delimiter}]{cmd:)}

{pstd}
    where string scalar {it:delimiter} sets the character(s) delimiting the descriptions;
    the default is to assume a space-separated list, i.e. {it:delimiter} = {cmd:" "}. To
    avoid breaking a description that contains a delimiting character, enclose
    the description in double quotes. {it:S}{cmd:.info()} affects all colors
    defined in {it:S}; use {it:S}{cmd:.info_added()} to affect only the
    colors that have been added last.

{pstd}
    To import descriptions from a string vector {it:Info} (each
    element containing a single color description), type

        {it:S}{cmd:.Info}[{cmd:_added}]{cmd:(}{it:Info}{cmd:)}

{pstd}
    Note that redefining the colors, e.g. by applying {help colrspace##string:{it:S}{bf:.colors()}}
    or {help colrspace##io:{it:S}{bf:.set()}}, will delete existing color descriptions.

{pstd}
    Example (colors from {browse "http://getbootstrap.com/docs/3.3/"}):

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("#337ab7 #5cb85c #5bc0de #f0ad4e #d9534f")"'}
        . {stata `"mata: S.info("primary success info warning danger")"'}
        . {stata `"mata: S.name("colors from Bootstrap 3.3")"'}
        . {stata "colorpalette mata(S)"}

{pstd}
    Functions {help colrspace##string:{it:S}{bf:.colors()}} and {help
    colrspace##palette:{it:S}{bf:.palette()}} fill in descriptions
    automatically for colors that have been translated to RGB (the original
    color codes will be used as descriptions). Furthermore, modification
    functions such as {help colrspace##palette:{it:S}{bf:.ipolate()}} set the
    descriptions to the color codes in the color space in which the
    modification has been performed.

{dlgtab:Description output}

{pstd}
    To export color descriptions into a string scalar containing a
    space-separated list of the descriptions, type

        {it:info} = {it:S}{cmd:.info}[{cmd:_added}]{cmd:()}

{pstd}
    {it:S}{cmd:.info()} exports descriptions from all colors; use
    {it:S}{cmd:.info_added()} to export descriptions only from the colors that
    have been added last.

{pstd}
    Alternatively, to export the color descriptions into a string column vector (each
    row containing a single description) type

        {it:Info} = {it:S}{cmd:.Info}[{cmd:_added}]{cmd:()}

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("SeaShell Crimson")"'}
        . {stata "mata: S.colors()"}
        . {stata "mata: S.info()"}


{marker palette}{...}
{title:Color palettes}

{pstd}
    {cmd:ColrSpace} features a large collection of named color palettes and color
    generators. The syntax to import colors from such a palette or color generator
    is

        {it:S}{cmd:.}[{cmd:add_}]{cmd:palette(}[{cmd:"}{it:name}{cmd:"}{cmd:,} {it:n}{cmd:,} {it:opt1}{cmd:,} {it:opt2}{cmd:,} {it:opt3}{cmd:,} {it:opt4}]{cmd:)}

{pstd}
    where {it:name} selects the palette and {it:n} sets
    the desired number of colors. Arguments {it:opt1} to {it:opt4} depend
    the type of palette as explained below. {it:S}{cmd:.palette()} will replace
    preexisting colors in {it:S} by the new colors; {it:S}{cmd:.add_palette()}
    will append the new colors to the existing colors. The functions abort with
    error if {it:name} does not match an existing palette. See function
    {help colrspace##pexists:{it:S}{bf:.pexists()}} for a way to check whether a
    palette exists or not. See function
    {help colrspace##palettes:{it:S}{bf:.palettes()}} if you want to obtain a list
    available palettes.

{pstd}
    There are three types of palettes, discussed under the following headings:

        {help colrspace##stdpalettes:Standard palettes}
        {help colrspace##colormaps:Colormaps}
        {help colrspace##generators:Color generators}

{marker stdpalettes}{...}
{dlgtab:Standard palettes}

{pstd}
    The syntax for standard palettes is

        {it:S}{cmd:.}[{cmd:add_}]{cmd:palette(}[{cmd:"}{it:name}{cmd:"}{cmd:,} {it:n}{cmd:,} {it:noexpand}]{cmd:)}

{pstd}
    where

{phang}
    {it:name} selects the palette; see {help colrspace##palettelist:below} for
    available names. Default is {cmd:st} in Stata 18 or above and
    {cmd:s2} in Stata 17 or below; the default can also be selected by typing {cmd:""}.

{phang}
    {it:n} is the number of colors to be retrieved from the palette. Many
    palettes, such as, e.g., the sequential and
    diverging ColorBrewer palettes, are adaptive to {it:n} in the sense that
    they return different colors depending on {it:n}. Other palettes, such
    as {cmd:s2}, contain a fixed set of colors. In any case, if {it:n} is
    different from the (maximum or minimum) number of colors defined by a
    palette, the colors are either recycled (qualitative palettes, i.e. if
    {help colrspace##pclass:{it:S}{bf:.pclass()}} is {cmd:"qualitative"}
    or {cmd:"categorical"}) or interpolated (all other palettes) such that the
    number of retrieved colors is equal to {it:n}. Interpolation will be
    performed in the {cmd:"Jab"} space; if you want to interpolate in another
    space, specify {it:noexpand}!=0 and then apply
    {help colrspace##ipolate:{it:S}{bf:.ipolate()}}.

{phang}
    {it:noexpand}!=0 prevents recycling or interpolating colors if {it:n}, the
    number of requested colors, is larger than the maximum number of colors
    defined by a palette. That is, if {it:noexpand}!=0 is specified, the
    resulting number of colors in {it:S} may be smaller than the requested number
    of colors. If {it:n} is smaller than the minimum number of colors
    defined by a palette, {it:noexpand}!=0 causes the first {it:n} colors
    to be selected (for qualitative palettes this corresponds to the default
    behavior; for other palettes the default would be to interpolate).

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.palette("lin fruits")"'}
        . {stata `"mata: S.add_palette("lin veg")"'}
        . {stata `"mata: S.name("fruits and vegetables")"'}
        . {stata "colorpalette mata(S)"}

{marker palettelist}{...}
{pstd}
    Currently available standard palettes are as follows (click on
    a palette name to view the palette; this requires the {helpb palettes} package to be
    installed):

{p2colset 9 25 27 2}{...}
{p2col:{stata colorpalette st:{bf:st}}}15 qualitative colors as in Stata's {helpb scheme st:stcolor} scheme (default in Stata 18 or above){p_end}
{p2col:{stata colorpalette s2:{bf:s2}}}15 qualitative colors as in Stata's {helpb scheme s2:s2color} scheme (default in Stata 17 or below){p_end}
{p2col:{stata colorpalette s1:{bf:s1}}}15 qualitative colors as in Stata's {helpb scheme s1:s1color} scheme{p_end}
{p2col:{stata colorpalette s1r:{bf:s1r}}}15 qualitative colors as in Stata's {helpb scheme s1:s1rcolor} scheme{p_end}
{p2col:{stata colorpalette economist:{bf:economist}}}15 qualitative colors as in Stata's {helpb scheme economist:economist} scheme{p_end}
{p2col:{stata colorpalette mono:{bf:mono}}}15 gray scales (qualitative) as in Stata's monochrome schemes{p_end}
{p2col:{stata colorpalette okabe:{bf:okabe}}}8 CVD-friendly qualitative colors by {browse "http://jfly.iam.u-tokyo.ac.jp/color/":Okabe and Ito (2002)}{p_end}
{p2col:{stata colorpalette cblind:{bf:cblind}}}like {cmd:okabe}, but including gray on second position{p_end}
{p2col:{stata colorpalette plottig:{bf:plottig}}}15 qualitative colors as in {cmd:plottig} by {browse "http://www.stata-journal.com/article.html?article=gr0070":Bischof (2017b)}{p_end}
{p2col:{stata colorpalette 538:{bf:538}}}13 qualitative colors as in {cmd:538} by {browse "http://ideas.repec.org/c/boc/bocode/s458404.html":Bischof (2017a)}{p_end}
{p2col:{stata colorpalette mrc:{bf:mrc}}}8 qualitative colors as in {cmd:tfl} by {browse "http://ideas.repec.org/c/boc/bocode/s458103.html":Morris (2015)}{p_end}
{p2col:{stata colorpalette tfl:{bf:tfl}}}7 qualitative colors as in {cmd:mrc} by {browse "http://ideas.repec.org/c/boc/bocode/s457703.html":Morris (2013)}{p_end}
{p2col:{stata colorpalette burd:{bf:burd}}}13 qualitative colors as in {cmd:burd} by {browse "http://ideas.repec.org/c/boc/bocode/s457623.html":Briatte (2013)}{p_end}
{p2col:{stata colorpalette lean:{bf:lean}}}15 gray scales (qualitative) as in {cmd:lean} by {browse "http://www.stata-journal.com/article.html?article=gr0002":Juul (2003)}{p_end}
{p2col:{stata colorpalette tableau:{bf:tableau}}}20 qualitative colors from {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)}{p_end}
{p2col:{it:pals}}qualitative palettes from the {cmd:pals} package in R
    ({browse "http://github.com/kwstat/pals":github.com/kwstat/pals}),
    where {it:pals} is
    {stata colorpalette alphabet:{bf:alphabet}} (26),
    {stata colorpalette alphabet2:{bf:alphabet2}} (26),
    {stata colorpalette cols25:{bf:cols25}} (25),
    {stata colorpalette glasbey:{bf:glasbey}} (32),
    {stata colorpalette kelly:{bf:kelly}} (22; default),
    {stata colorpalette polychrome:{bf:polychrome}} (36), or
    {stata colorpalette watlington:{bf:watlington}} (16)
    {p_end}
{p2col:{cmd:d3} [{it:scheme}]}qualitative palettes from {browse "http://d3js.org/":D3.js}, where {it:scheme} is
    {stata colorpalette d3 10:{bf:10}} (default),
    {stata colorpalette d3 20, rows(8):{bf:20}},
    {stata colorpalette d3 20b, rows(8):{bf:20b}}, or
    {stata colorpalette d3 20c, rows(8):{bf:20c}}
    (aliases: {stata colorpalette tab10:{bf:tab10}},
    {stata colorpalette tab20:{bf:tab20}},
    {stata colorpalette tab20b:{bf:tab20b}}, and
    {stata colorpalette tab20c:{bf:tab20c}})
    {p_end}
{p2col:{cmd:sb} [{it:scheme}]}qualitative palettes from
    {browse "http://seaborn.pydata.org/":seaborn.pydata.org}, where {it:scheme} is as follows
    {p_end}
{p2col:}10-color variants: {stata colorpalette sb deep:{bf:deep}} (default),
    {stata colorpalette sb muted:{bf:muted}},
    {stata colorpalette sb pastel:{bf:pastel}},
    {stata colorpalette sb bright:{bf:bright}},
    {stata colorpalette sb dark:{bf:dark}},
    {stata colorpalette sb colorblind:{bf:colorblind}}
    {p_end}
{p2col:}6-color variants: {stata colorpalette sb deep6:{bf:deep6}} (alias: {stata colorpalette sb6:{bf:sb6}}),
    {stata colorpalette sb muted6:{bf:muted6}},
    {stata colorpalette sb pastel6:{bf:pastel6}},
    {stata colorpalette sb bright6:{bf:bright6}},
    {stata colorpalette sb dark6:{bf:dark6}},
    {stata colorpalette sb colorblind6:{bf:colorblind6}}
    {p_end}
{p2col:{cmd:tab} [{it:scheme}]}color schemes from
    {browse "http://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782":Tableau 10}
    ({browse "http://github.com/jrnold/ggthemes/blob/main/data-raw/theme-data/tableau.yml":source}),
    where {it:scheme} is as follows
    {p_end}
{p2col:}qualitative: {stata colorpalette tab 10:{bf:10}} (default),
    {stata colorpalette tab 20:{bf:20}},
    {stata colorpalette tab Color Blind:{bf:Color Blind}} (10),
    {stata colorpalette tab Seattle Grays:{bf:Seattle Grays}} (5),
    {stata colorpalette tab Traffic:{bf:Traffic}} (9),
    {stata colorpalette tab Miller Stone:{bf:Miller Stone}} (11),
    {stata colorpalette tab Superfishel Stone:{bf:Superfishel Stone}} (10),
    {stata colorpalette tab Nuriel Stone:{bf:Nuriel Stone}} (9),
    {stata colorpalette tab Jewel Bright:{bf:Jewel Bright}} (9),
    {stata colorpalette tab Summer:{bf:Summer}} (8),
    {stata colorpalette tab Winter:{bf:Winter}} (10),
    {stata colorpalette tab Green-Orange-Teal:{bf:Green-Orange-Teal}} (12),
    {stata colorpalette tab Red-Blue-Brown:{bf:Red-Blue-Brown}} (12),
    {stata colorpalette tab Purple-Pink-Gray:{bf:Purple-Pink-Gray}} (12),
    {stata colorpalette tab Hue Circle:{bf:Hue Circle}} (19)
    {p_end}
{p2col:}sequential: {stata colorpalette tab Blue-Green:{bf:Blue-Green}} (7),
    {stata colorpalette tab Blue Light:{bf:Blue Light}} (7),
    {stata colorpalette tab Orange Light:{bf:Orange Light}} (7),
    {stata colorpalette tab Blue:{bf:Blue}} (20),
    {stata colorpalette tab Orange:{bf:Orange}} (20),
    {stata colorpalette tab Green:{bf:Green}} (20),
    {stata colorpalette tab Red:{bf:Red}} (20),
    {stata colorpalette tab Purple:{bf:Purple}} (20),
    {stata colorpalette tab Brown:{bf:Brown}} (20),
    {stata colorpalette tab Gray:{bf:Gray}} (20),
    {stata colorpalette tab Gray Warm:{bf:Gray Warm}} (20),
    {stata colorpalette tab Blue-Teal:{bf:Blue-Teal}} (20),
    {stata colorpalette tab Orange-Gold:{bf:Orange-Gold}} (20),
    {stata colorpalette tab Green-Gold:{bf:Green-Gold}} (20),
    {stata colorpalette tab Red-Gold:{bf:Red-Gold}} (21)
    {p_end}
{p2col:}diverging (7): {stata colorpalette tab Orange-Blue:{bf:Orange-Blue}},
    {stata colorpalette tab Red-Green:{bf:Red-Green}},
    {stata colorpalette tab Green-Blue:{bf:Green-Blue}},
    {stata colorpalette tab Red-Blue:{bf:Red-Blue}},
    {stata colorpalette tab Red-Black:{bf:Red-Black}},
    {stata colorpalette tab Gold-Purple:{bf:Gold-Purple}},
    {stata colorpalette tab Red-Green-Gold:{bf:Red-Green-Gold}},
    {stata colorpalette tab Sunset-Sunrise:{bf:Sunset-Sunrise}},
    {stata colorpalette tab Orange-Blue-White:{bf:Orange-Blue-White}},
    {stata colorpalette tab Red-Green-White:{bf:Red-Green-White}},
    {stata colorpalette tab Green-Blue-White:{bf:Green-Blue-White}},
    {stata colorpalette tab Red-Blue-White:{bf:Red-Blue-White}},
    {stata colorpalette tab Red-Black-White:{bf:Red-Black-White}},
    {stata colorpalette tab Orange-Blue Light:{bf:Orange-Blue Light}},
    {stata colorpalette tab Temperature:{bf:Temperature}}
    {p_end}
{p2col:{cmd:tol} [{it:scheme}]}color schemes by Paul Tol
    ({browse "http://personal.sron.nl/~pault/":personal.sron.nl/~pault};
    using definitions from {browse "http://personal.sron.nl/~pault/data/tol_colors.py":tol_colors.py},
    which may deviate from {browse "http://personal.sron.nl/~pault/":personal.sron.nl/~pault}),
    where {it:scheme} is as follows
    {p_end}
{p2col:}qualitative: {stata colorpalette tol bright:{bf:bright}} (8),
    {stata colorpalette tol high-contrast:{bf:high-contrast}} (4),
    {stata colorpalette tol vibrant:{bf:vibrant}} (8),
    {stata colorpalette tol muted:{bf:muted}} (11; default),
    {stata colorpalette tol medium-contrast:{bf:medium-contrast}} (7),
    {stata colorpalette tol light:{bf:light}} (10)
    {p_end}
{p2col:}sequential: {stata colorpalette tol YlOrBr:{bf:YlOrBr}} (9),
    {stata colorpalette tol iridescent:{bf:iridescent}} (23)
    {p_end}
{p2col:}rainbow: {stata colorpalette tol rainbow:{bf:rainbow}} (1-23),
    {stata colorpalette tol PuRd:{bf:PuRd}} (22),
    {stata colorpalette tol PuBr:{bf:PuBr}} (26),
    {stata colorpalette tol WhRd:{bf:WhRd}} (30),
    {stata colorpalette tol WhBr:{bf:WhBr}} (34)
    {p_end}
{p2col:}diverging: {stata colorpalette tol sunset:{bf:sunset}} (11),
    {stata colorpalette tol BuRd:{bf:BuRd}} (9),
    {stata colorpalette tol PRGn:{bf:PRGn}} (9)
    {p_end}
{p2col:{it:colorbrewer}}color schemes from {browse "http://colorbrewer2.org/":ColorBrewer}
    (Brewer et al. 2013, Brewer 2016){p_end}
{p2col:}qualitative: {stata colorpalette Accent:{bf:Accent}} (8),
    {stata colorpalette Dark2:{bf:Dark2}} (8),
    {stata colorpalette Paired:{bf:Paired}} (12),
    {stata colorpalette Pastel1:{bf:Pastel1}} (9),
    {stata colorpalette Pastel2:{bf:Pastel2}} (8),
    {stata colorpalette Set1:{bf:Set1}} (9),
    {stata colorpalette Set2:{bf:Set2}} (8),
    {stata colorpalette Set3:{bf:Set3}} (12)
    {p_end}
{p2col:}sequential (3-9): {stata colorpalette Blues:{bf:Blues}},
    {stata colorpalette BuGn:{bf:BuGn}},
    {stata colorpalette BuPu:{bf:BuPu}},
    {stata colorpalette GnBu:{bf:GnBu}},
    {stata colorpalette Greens:{bf:Greens}},
    {stata colorpalette Greys:{bf:Greys}},
    {stata colorpalette OrRd:{bf:OrRd}},
    {stata colorpalette Oranges:{bf:Oranges}},
    {stata colorpalette PuBu:{bf:PuBu}},
    {stata colorpalette PuBuGn:{bf:PuBuGn}},
    {stata colorpalette PuRd:{bf:PuRd}},
    {stata colorpalette Purples:{bf:Purples}},
    {stata colorpalette RdPu:{bf:RdPu}},
    {stata colorpalette Reds:{bf:Reds}},
    {stata colorpalette YlGn:{bf:YlGn}},
    {stata colorpalette YlGnBu:{bf:YlGnBu}},
    {stata colorpalette YlOrBr:{bf:YlOrBr}},
    {stata colorpalette YlOrRd:{bf:YlOrRd}}
    {p_end}
{p2col:}diverging (3-11): {stata colorpalette BrBG:{bf:BrBG}},
    {stata colorpalette PRGn:{bf:PRGn}},
    {stata colorpalette PiYG:{bf:PiYG}},
    {stata colorpalette PuOr:{bf:PuOr}},
    {stata colorpalette RdBu:{bf:RdBu}},
    {stata colorpalette RdGy:{bf:RdGy}},
    {stata colorpalette RdYlBu:{bf:RdYlBu}},
    {stata colorpalette RdYlGn:{bf:RdYlGn}},
    {stata colorpalette Spectral:{bf:Spectral}}
    {p_end}
{p2col:}CMYK variants: add keyword {cmd:cmyk} (e.g. {stata colorpalette Accent cmyk:{bf:Accent cmyk}})
    {p_end}
{p2col:{cmd:carto} [{it:scheme}]}color schemes from {browse "http://carto.com/carto-colors/":carto.com/carto-colors}
    {p_end}
{p2col:}qualitative (3-12): {stata colorpalette carto Antique:{bf:Antique}},
    {stata colorpalette carto Bold:{bf:Bold}} (default),
    {stata colorpalette carto Pastel:{bf:Pastel}},
    {stata colorpalette carto Prism:{bf:Prism}},
    {stata colorpalette carto Safe:{bf:Safe}} (CVD-friendly),
    {stata colorpalette carto Vivid:{bf:Vivid}}
    {p_end}
{p2col:}sequential (2-7): {stata colorpalette carto Burg:{bf:Burg}},
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
    {p_end}
{p2col:}diverging (2-7): {stata colorpalette carto ArmyRose:{bf:ArmyRose}},
    {stata colorpalette carto Fall:{bf:Fall}},
    {stata colorpalette carto Geyser:{bf:Geyser}},
    {stata colorpalette carto Temps:{bf:Temps}},
    {stata colorpalette carto TealRose:{bf:TealRose}},
    {stata colorpalette carto Tropic:{bf:Tropic}},
    {stata colorpalette carto Earth:{bf:Earth}}
    {p_end}
{p2col:{cmd:ptol} [{it:scheme}]}color schemes from {browse "http://personal.sron.nl/~pault/colourschemes.pdf":Tol (2012)},
    where {it:scheme} is
    {stata colorpalette ptol qualitative:{bf:qualitative}} (1-12; default),
    {stata colorpalette ptol rainbow:{bf:rainbow}} (4-12), or
    {stata colorpalette ptol diverging:{bf:diverging}} (3-11)
    {p_end}
{p2col:{cmd:lin} [{it:scheme}]}semantic colors from {browse "http://dx.doi.org/10.1111/cgf.12127":Lin et al. (2013)},
    where {it:scheme} is
    {stata colorpalette lin carcolor:{bf:carcolor}} (6; default),
    {stata colorpalette lin food:{bf:food}} (7),
    {stata colorpalette lin features:{bf:features}} (5),
    {stata colorpalette lin activities:{bf:activities}} (5),
    {stata colorpalette lin fruits:{bf:fruits}} (7),
    {stata colorpalette lin vegetables:{bf:vegetables}} (7),
    {stata colorpalette lin drinks:{bf:drinks}} (7), or
    {stata colorpalette lin brands:{bf:brands}} (7)
    {p_end}
{p2col:}algorithm-selected variants: add keyword {cmd:algorithm} (e.g. {stata colorpalette lin carcolor algorithm:{bf:lin carcolor algorithm}})
    {p_end}
{p2col:{cmd:spmap} [{it:scheme}]}color schemes by {browse "http://ideas.repec.org/c/boc/bocode/s456812.html":Pisati (2007)},
    where {it:scheme} is
    {stata colorpalette spmap blues:{bf:blues}} (2-99; default),
    {stata colorpalette spmap greens:{bf:greens}} (2-99),
    {stata colorpalette spmap greys:{bf:greys}} (2-99),
    {stata colorpalette spmap reds:{bf:reds}} (2-99),
    {stata colorpalette spmap rainbow:{bf:rainbow}} (2-99),
    {stata colorpalette spmap heat:{bf:heat}} (2-16),
    {stata colorpalette spmap terrain:{bf:terrain}} (2-16), or
    {stata colorpalette spmap topological:{bf:topological}} (2-16)
    {p_end}
{p2col:{cmd:sfso} [{it:scheme}]}color schemes by the Swiss Federal Statistics Office (2017), where {it:scheme} is as follows
    {p_end}
{p2col:}qualitative: {stata colorpalette sfso parties:{bf:parties}} (11),
    {stata colorpalette sfso languages:{bf:languages}} (5)
    {p_end}
{p2col:}sequential (7): {stata colorpalette sfso blue:{bf:blue}} (default)
    {p_end}
{p2col:}sequential (6): {stata colorpalette sfso brown:{bf:brown}},
    {stata colorpalette sfso orange:{bf:orange}},
    {stata colorpalette sfso red:{bf:red}},
    {stata colorpalette sfso pink:{bf:pink}},
    {stata colorpalette sfso purple:{bf:purple}},
    {stata colorpalette sfso violet:{bf:violet}},
    {stata colorpalette sfso ltblue:{bf:ltblue}},
    {stata colorpalette sfso turquoise:{bf:turquoise}},
    {stata colorpalette sfso green:{bf:green}},
    {stata colorpalette sfso olive:{bf:olive}},
    {stata colorpalette sfso black:{bf:black}}
    {p_end}
{p2col:}diverging (10): {stata colorpalette sfso votes:{bf:votes}}
    {p_end}
{p2col:}CMYK variants: add keyword {cmd:cmyk} (e.g. {stata colorpalette sfso blue cmyk:{bf:sfso blue cmyk}})
    {p_end}
{p2col:{cmd:HTML} [{it:scheme}]}HTML colors in groups from {browse "http://www.w3schools.com/colors/colors_groups.asp":www.w3schools.com},
    where {it:scheme} is
    {stata colorpalette HTML pink:{bf:pink}} (6),
    {stata colorpalette HTML purple:{bf:purple}} (19),
    {stata colorpalette HTML red:{bf:red}} (9),
    {stata colorpalette HTML orange:{bf:orange}} (5),
    {stata colorpalette HTML yellow:{bf:yellow}} (11),
    {stata colorpalette HTML green:{bf:green}} (22),
    {stata colorpalette HTML cyan:{bf:cyan}} (8),
    {stata colorpalette HTML blue:{bf:blue}} (16),
    {stata colorpalette HTML brown:{bf:brown}} (18),
    {stata colorpalette HTML white:{bf:white}} (17), or
    {stata colorpalette HTML gray:{bf:gray}} (10; alias: {stata colorpalette HTML grey:{bf:grey}});
    all 140 HTML colors (alphabetically sorted) will be returned if {it:scheme} is omitted (alias: {stata colorpalette webcolors:{bf:webcolors}})
    {p_end}
{p2col:{cmd:w3} [{it:scheme}]}W3.CSS color schemes from {browse "http://www.w3schools.com/w3css/w3css_color_material.asp":www.w3schools.com},
    where {it:scheme} is as follows
    {p_end}
{p2col:}qualitative collections: {stata colorpalette w3 default:{bf:default}} (30 {browse "http://www.w3schools.com/w3css/w3css_color_material.asp":Default Colors}),
    {stata colorpalette w3 flat:{bf:flat}} (20 {browse "http://www.w3schools.com/w3css/w3css_color_flat.asp":Flat UI Colors}),
    {stata colorpalette w3 metro:{bf:metro}} (17 {browse "http://www.w3schools.com/w3css/w3css_color_metro.asp":Metro UI Colors}),
    {stata colorpalette w3 win8:{bf:win8}} (22 {browse "http://www.w3schools.com/w3css/w3css_color_win8.asp":Windows 8 Colors}),
    {stata colorpalette w3 ios:{bf:ios}} (12 {browse "http://www.w3schools.com/w3css/w3css_color_ios.asp":iOS Colors}),
    {stata colorpalette w3 highway:{bf:highway}} (7 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Highway Colors}),
    {stata colorpalette w3 safety:{bf:safety}} (6 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":US Safety Colors}),
    {stata colorpalette w3 signal:{bf:signal}} (10 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":European Signal Colors}),
    {stata colorpalette w3 2019:{bf:2019}} (32 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2019}),
    {stata colorpalette w3 2018:{bf:2018}} (30 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2018}),
    {stata colorpalette w3 2017:{bf:2017}} (20 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Fashion Colors 2017}),
    {stata colorpalette w3 vivid:{bf:vivid}} (21 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Vivid Colors}),
    {stata colorpalette w3 food:{bf:food}} (40 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Food Colors}),
    {stata colorpalette w3 camo:{bf:camo}} (15 {browse "http://www.w3schools.com/w3css/w3css_color_libraries.asp":Camouflage Colors}),
    {stata colorpalette w3 ana:{bf:ana}} (44 {browse "http://www.w3schools.com/colors/colors_fs595.asp":Army Navy Aero Colors}),
    {stata colorpalette w3 traffic:{bf:traffic}} (9 {browse "http://www.w3schools.com/colors/colors_ral.asp":Traffic Colors})
    {p_end}
{p2col:}sequential themes (11): {stata colorpalette w3 amber:{bf:amber}},
    {stata colorpalette w3 black:{bf:black}},
    {stata colorpalette w3 blue:{bf:blue}},
    {stata colorpalette w3 blue-grey:{bf:blue-grey}},
    {stata colorpalette w3 brown:{bf:brown}},
    {stata colorpalette w3 cyan:{bf:cyan}},
    {stata colorpalette w3 dark-grey:{bf:dark-grey}},
    {stata colorpalette w3 deep-orange:{bf:deep-orange}},
    {stata colorpalette w3 deep-purple:{bf:deep-purple}},
    {stata colorpalette w3 green:{bf:green}},
    {stata colorpalette w3 grey:{bf:grey}},
    {stata colorpalette w3 indigo:{bf:indigo}},
    {stata colorpalette w3 khaki:{bf:khaki}},
    {stata colorpalette w3 light-blue:{bf:light-blue}},
    {stata colorpalette w3 light-green:{bf:light-green}},
    {stata colorpalette w3 lime:{bf:lime}},
    {stata colorpalette w3 orange:{bf:orange}},
    {stata colorpalette w3 pink:{bf:pink}},
    {stata colorpalette w3 purple:{bf:purple}},
    {stata colorpalette w3 red:{bf:red}},
    {stata colorpalette w3 teal:{bf:teal}},
    {stata colorpalette w3 yellow:{bf:yellow}}
    {p_end}
{p2col:{it:wesanderson}}Wes Anderson palettes from
    {browse "http://wesandersonpalettes.tumblr.com/":wesandersonpalettes.tumblr.com}
    ({browse "http://github.com/karthik/wesanderson":source})
    {p_end}
{p2col:}qualitative: {stata colorpalette BottleRocket1:{bf:BottleRocket1}} (7),
    {stata colorpalette BottleRocket2:{bf:BottleRocket2}} (5),
    {stata colorpalette Rushmore1:{bf:Rushmore1}} (5),
    {stata colorpalette Royal1:{bf:Royal1}} (4),
    {stata colorpalette Royal2:{bf:Royal2}} (5),
    {stata colorpalette Zissou1:{bf:Zissou1}} (5),
    {stata colorpalette Darjeeling1:{bf:Darjeeling1}} (5),
    {stata colorpalette Darjeeling2:{bf:Darjeeling2}} (5),
    {stata colorpalette Chevalier1:{bf:Chevalier1}} (4),
    {stata colorpalette FantasticFox1:{bf:FantasticFox1}} (5),
    {stata colorpalette Moonrise1:{bf:Moonrise1}} (4),
    {stata colorpalette Moonrise2:{bf:Moonrise2}} (4),
    {stata colorpalette Moonrise3:{bf:Moonrise3}} (5),
    {stata colorpalette Cavalcanti1:{bf:Cavalcanti1}} (5),
    {stata colorpalette GrandBudapest1:{bf:GrandBudapest1}} (4),
    {stata colorpalette GrandBudapest2:{bf:GrandBudapest2}} (4),
    {stata colorpalette IsleofDogs1:{bf:IsleofDogs1}} (6),
    {stata colorpalette IsleofDogs2:{bf:IsleofDogs2}} (5),
    {stata colorpalette FrenchDispatch1:{bf:FrenchDispatch1}} (5)
    {p_end}
{p2col:}sequential: {stata colorpalette Zissou1:{bf:Zissou1}} (5)
    {p_end}

{pstd}
    The palette names can be abbreviated and typed in lowercase letters (for example,
    {cmd:"BuGn"} could be typed as {cmd:"bugn"}, {cmd:"lin carcolor algorithm"} could be
    typed as {cmd:"lin car a"}). If abbreviation is ambiguous, the first matching name
    in the sorted list of palettes (including all palette types) will be used. Numbers
    in parentheses refer to the palette size (number of colors; a range
    means that the palette comes in different sizes).

{pstd}
    {browse "http://colorbrewer2.org/":ColorBrewer} is a set of color schemes developed by
    {browse "http://doi.org/10.1559/152304003100010929":Brewer et al. (2003)}; also
    see Brewer (2016). These colors are licensed under Apache License Version 2.0; see
    the copyright notes at
    {browse "http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_updates.html":ColorBrewer_updates.html}.

{marker colormaps}{...}
{dlgtab:Colormaps}

{pstd}
    Colormaps are palettes whose colors are obtained by
    {help colrspace##lsmap:linear segmentation} around anchor points or by
    interpolation from a dense grid of RGB values. The syntax for colormaps is

        {it:S}{cmd:.}[{cmd:add_}]{cmd:palette(}[{cmd:"}{it:name}{cmd:"}{cmd:,} {it:n}{cmd:,} {it:range}]{cmd:)}

{pstd}
    where

{phang}
    {it:name} selects the colormap. See {help colrspace##colormapslist:below}
    for available names.

{phang}
    {it:n} is the number of colors to be retrieved from the colormap. The default
    is 15.

{phang}
     {it:range} = {cmd:(}{it:lb}[{cmd:,} {it:ub}]{cmd:)} specifies the range of
     the colormap to be used, with {it:lb} and {it:ub} within [0,1] (values
     smaller than 0 or larger than 1 will be interpreted as 0 or 1, respectively). The
     default is {cmd:(0,1)}. This default can also be selected by typing
     {cmd:.} (missing). If {it:lb} is larger than {it:ub}, the colors are
     retrieved in reverse order. Argument {it:range} has not effect for
     cyclic (circular) colormaps.

{marker colormapslist}{...}
{pstd}
    Currently available colormaps are as follows:

{p2colset 9 28 30 2}{...}
{p2col:{it:viridis}}perceptually uniform colormaps from
{browse "http://matplotlib.org/stable/tutorials/colors/colormaps.html":matplotlib.org}
    (also see {browse "http://bids.github.io/colormap/":bids.github.io/colormap})
    {p_end}
{p2col:}sequential: {stata colorpalette viridis:{bf:viridis}},
    {stata colorpalette magma:{bf:magma}},
    {stata colorpalette inferno:{bf:inferno}},
    {stata colorpalette plasma:{bf:plasma}},
    {stata colorpalette cividis:{bf:cividis}} (CVD-friendly)
    {p_end}
{p2col:}cyclic: {stata colorpalette twilight:{bf:twilight}},
    {stata colorpalette twilight shifted:{bf:twilight shifted}}
    {p_end}
{p2col:{it:seaborn}}perceptually uniform colormaps from
{browse "http://seaborn.pydata.org/":seaborn.pydata.org}
    {p_end}
{p2col:}sequential: {stata colorpalette rocket:{bf:rocket}},
    {stata colorpalette mako:{bf:mako}},
    {stata colorpalette flare:{bf:flare}},
    {stata colorpalette crest:{bf:crest}}
    {p_end}
{p2col:}diverging: {stata colorpalette vlag:{bf:vlag}},
    {stata colorpalette icefire:{bf:icefire}}
    {p_end}
{p2col:{cmd:matplotlib} [{it:map}]}further colormaps from
    {browse "http://matplotlib.orghttp://matplotlib.org/stable/tutorials/colors/colormaps.html":matplotlib.org}
    ({browse "http://dx.doi.org/10.1109/MCSE.2007.55":Hunter 2007}),
    where {it:map} is
    {stata colorpalette matplotlib autumn:{bf:autumn}},
    {stata colorpalette matplotlib spring:{bf:spring}},
    {stata colorpalette matplotlib summer:{bf:summer}},
    {stata colorpalette matplotlib winter:{bf:winter}},
    {stata colorpalette matplotlib bone:{bf:bone}},
    {stata colorpalette matplotlib cool:{bf:cool}},
    {stata colorpalette matplotlib copper:{bf:copper}},
    {stata colorpalette matplotlib coolwarm:{bf:coolwarm}},
    {stata colorpalette matplotlib hot:{bf:hot}},
    {stata colorpalette matplotlib jet:{bf:jet}} (default), or
    {stata colorpalette matplotlib turbo:{bf:turbo}}
    {p_end}
{p2col:{cmd:CET} [{it:map}]}perceptually uniform colormaps by {browse "http://arxiv.org/abs/1509.03700":Kovesi (2015)},
    where {it:map} is as follows (see {browse "http://colorcet.com/gallery.html":colorcet.com/gallery.html} for an overview)
    {p_end}
{p2col:}linear: {stata colorpalette CET L01:{bf:L01}},
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
{p2col:}rainbow: {stata colorpalette CET R1:{bf:R1}},
    {stata colorpalette CET R2:{bf:R2}},
    {stata colorpalette CET R3:{bf:R3}},
    {stata colorpalette CET R4:{bf:R4}}
    {p_end}
{p2col:}isoluminant: {stata colorpalette CET I1:{bf:I1}},
    {stata colorpalette CET I2:{bf:I2}},
    {stata colorpalette CET I3:{bf:I3}}
    {p_end}
{p2col:}diverging: {stata colorpalette CET D01:{bf:D01}},
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
{p2col:}circular: {stata colorpalette CET C1:{bf:C1}},
    {stata colorpalette CET C2:{bf:C2}},
    {stata colorpalette CET C3:{bf:C3}},
    {stata colorpalette CET C4:{bf:C4}},
    {stata colorpalette CET C5:{bf:C5}},
    {stata colorpalette CET C6:{bf:C6}},
    {stata colorpalette CET C7:{bf:C7}}
    {p_end}
{p2col:}CVD-friendly: {stata colorpalette CET CBD1:{bf:CBD1}},
    {stata colorpalette CET CBL1:{bf:CBL1}},
    {stata colorpalette CET CBL2:{bf:CBL2}},
    {stata colorpalette CET CBC1:{bf:CBC1}},
    {stata colorpalette CET CBC2:{bf:CBC2}}
    {p_end}
{p2col:{cmd:scico} [{it:map}]}{browse "http://doi.org/10.5281/zenodo.5501399":version 7.0.1}
    variants of the (perceptually uniform and CVD-friendly) Scientific colour
    maps by {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)},
    where {it:map} is as follows
    {p_end}
{p2col:}sequential: {stata colorpalette scico batlow:{bf:batlow}} (default),
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
{p2col:}diverging: {stata colorpalette scico broc:{bf:broc}},
    {stata colorpalette scico cork:{bf:cork}},
    {stata colorpalette scico vik:{bf:vik}},
    {stata colorpalette scico lisbon:{bf:lisbon}},
    {stata colorpalette scico tofino:{bf:tofino}},
    {stata colorpalette scico berlin:{bf:berlin}},
    {stata colorpalette scico roma:{bf:roma}},
    {stata colorpalette scico bam:{bf:bam}},
    {stata colorpalette scico vanimo:{bf:vanimo}}
    {p_end}
{p2col:}cyclic: {stata colorpalette scico romaO:{bf:romaO}},
    {stata colorpalette scico bamO:{bf:bamO}},
    {stata colorpalette scico brocO:{bf:brocO}},
    {stata colorpalette scico corkO:{bf:corkO}},
    {stata colorpalette scico vikO:{bf:vikO}}
    {p_end}
{p2col:{cmd:scico8} [{it:map}]}{browse "http://doi.org/10.5281/zenodo.8409685":version 8.0.1}
    variants of the Scientific colour maps by
    {browse "http://www.fabiocrameri.ch/colourmaps/":Crameri (2018)};
    {cmd:scico8} provides additional sequential maps
    {stata colorpalette scico8 glasgow:{bf:glasgow}},
    {stata colorpalette scico8 lipari:{bf:lipari}},
    {stata colorpalette scico8 navia:{bf:navia}}, and
    {stata colorpalette scico8 naviaW:{bf:naviaW}},
    additional diverging map
    {stata colorpalette scico8 managua:{bf:managua}},
    as well as updated variants of
    {stata colorpalette scico8 batlowK:{bf:batlowK}},
    {stata colorpalette scico8 lajolla:{bf:lajolla}},
    {stata colorpalette scico8 bamako:{bf:bamako}},
    {stata colorpalette scico8 bilbao:{bf:bilbao}},
    {stata colorpalette scico8 grayC:{bf:grayC}},
    {stata colorpalette scico8 tokyo:{bf:tokyo}}, and
    {stata colorpalette scico8 acton:{bf:acton}};
    all other maps are identical to the ones in {cmd:scico}
    {p_end}

{pmore}
    The names can be abbreviated; if abbreviation is ambiguous, the first
    matching name in the sorted list of palettes (including all palette types)
    will be used.

{pstd}
    Example:

        . {stata "mata: A = B = ColrSpace()"}
        . {stata `"mata: A.palette("viridis")"'}
        . {stata `"mata: B.palette("magma", ., (.5,1))"'}
        . {stata "colorpalette: mata(A) / mata(B)"}

{marker generators}{...}
{dlgtab:Color generators}

{pstd}
    The syntax for the color generators is

        {it:S}{cmd:.}[{cmd:add_}]{cmd:palette(}[{cmd:"}{it:name}{cmd:"}{cmd:,} {it:n}{cmd:,} {it:H}{cmd:,} {it:C}{cmd:,} {it:L}{cmd:,} {it:P}]{cmd:)}

{pstd}
    where

{phang}
    {it:name} selects the type of color generator; see below.

{phang}
    {it:n} specifies the number of colors to be generated. The default is 15.

{phang}
    {it:H} is a real vector specifying one or two hues in degrees of the
    color wheel.

{phang}
    {it:C} is a real vector specifying one or two chroma levels. For {cmd:hue} only
    the first level is relevant.

{phang}
    {it:L} is a real vector specifying one or two luminance/lightness levels. For
    {cmd:hue} only the first level is relevant.

{phang}
    {it:P} is a real vector specifying one or two power parameters. For {cmd:hue}
    only the first parameter is relevant: {it:P}!=0 causes {cmd:hue}
    to travel counter-clockwise around the color wheel.

{pstd}
    The available color generators are as follows:

{phang}
    {stata colorpalette hue:{bf:hue}}{break}
    HCL colors with evenly spaced hues. The algorithm has been
    modeled after function {cmd:hue_pal()} from R's {cmd:scales} package by
    Hadley Wickham (see {browse "http://github.com/hadley/scales"}). The default
    parameters are {it:H} = {cmd:(15, 375)}, {it:C} = {cmd:100}, and {it:L} = {cmd:65}.
    If the difference between the two values of {it:H} is a multiple
    of 360, the second value will be reduced by 360/{it:n} (so that the space between
    the last and the first color is the same as between the other colors).
    {p_end}

{phang}{cmd:HCL} {it:scheme}{break}
    Qualitative, diverging, or sequential
    colors in the HCL space (radial CIE L*u*v*). The algorithm has been modeled after R's {cmd:colorspace} package by
    {browse "http://CRAN.R-project.org/package=colorspace":Ihaka et al. (2016)}; also
    see {browse "http://dx.doi.org/10.1016/j.csda.2008.11.033":Zeileis et al. (2009)}
    and {browse "http://hclwizard.org":hclwizard.org}. {it:scheme} can be one of
    the following.
    {p_end}
{p2colset 9 22 22 2}{...}
{p2col:qualitative:}{stata colorpalette hcl qualitative:{bf:qualitative}},
    {stata colorpalette hcl intense:{bf:intense}},
    {stata colorpalette hcl dark:{bf:dark}},
    {stata colorpalette hcl light:{bf:light}},
    {stata colorpalette hcl pastel:{bf:pastel}}
    {p_end}
{p2col:sequential:}{stata colorpalette hcl sequential:{bf:sequential}},
    {stata colorpalette hcl blues:{bf:blues}},
    {stata colorpalette hcl greens:{bf:greens}},
    {stata colorpalette hcl grays:{bf:grays}},
    {stata colorpalette hcl oranges:{bf:oranges}},
    {stata colorpalette hcl purples:{bf:purples}},
    {stata colorpalette hcl reds:{bf:reds}},
    {stata colorpalette hcl heat:{bf:heat}},
    {stata colorpalette hcl heat2:{bf:heat2}},
    {stata colorpalette hcl terrain:{bf:terrain}},
    {stata colorpalette hcl terrain2:{bf:terrain2}},
    {stata colorpalette hcl viridis:{bf:viridis}},
    {stata colorpalette hcl plasma:{bf:plasma}},
    {stata colorpalette hcl redblue:{bf:redblue}}
    {p_end}
{p2col:diverging:}{stata colorpalette hcl diverging:{bf:diverging}},
    {stata colorpalette hcl bluered:{bf:bluered}},
    {stata colorpalette hcl bluered2:{bf:bluered2}},
    {stata colorpalette hcl bluered3:{bf:bluered3}},
    {stata colorpalette hcl greenorange:{bf:greenorange}},
    {stata colorpalette hcl browngreen:{bf:browngreen}},
    {stata colorpalette hcl pinkgreen:{bf:pinkgreen}},
    {stata colorpalette hcl purplegreen:{bf:purplegreen}}

{phang}{cmd:LCh} {it:scheme}{break}
    Qualitative, diverging, or sequential colors in the LCh space (radial CIE
    L*a*b*). The algorithm has been modeled in analogy to {cmd:HCL}.
    {it:scheme} can be one of the following.
    {p_end}
{p2col:qualitative:}{stata colorpalette LCh qualitative:{bf:qualitative}},
    {stata colorpalette LCh intense:{bf:intense}},
    {stata colorpalette LCh dark:{bf:dark}},
    {stata colorpalette LCh light:{bf:light}},
    {stata colorpalette LCh pastel:{bf:pastel}}
    {p_end}
{p2col:sequential:}{stata colorpalette LCh sequential:{bf:sequential}},
    {stata colorpalette LCh blues:{bf:blues}},
    {stata colorpalette LCh greens:{bf:greens}},
    {stata colorpalette LCh grays:{bf:grays}},
    {stata colorpalette LCh oranges:{bf:oranges}},
    {stata colorpalette LCh purples:{bf:purples}},
    {stata colorpalette LCh reds:{bf:reds}},
    {stata colorpalette LCh heat:{bf:heat}},
    {stata colorpalette LCh heat2:{bf:heat2}},
    {stata colorpalette LCh terrain:{bf:terrain}},
    {stata colorpalette LCh terrain2:{bf:terrain2}},
    {stata colorpalette LCh viridis:{bf:viridis}},
    {stata colorpalette LCh plasma:{bf:plasma}},
    {stata colorpalette LCh redblue:{bf:redblue}}
    {p_end}
{p2col:diverging:}{stata colorpalette LCh diverging:{bf:diverging}},
    {stata colorpalette LCh bluered:{bf:bluered}},
    {stata colorpalette LCh bluered2:{bf:bluered2}},
    {stata colorpalette LCh bluered3:{bf:bluered3}},
    {stata colorpalette LCh greenorange:{bf:greenorange}},
    {stata colorpalette LCh browngreen:{bf:browngreen}},
    {stata colorpalette LCh pinkgreen:{bf:pinkgreen}},
    {stata colorpalette LCh purplegreen:{bf:purplegreen}}

{phang}{cmd:JMh} {it:scheme}{break}
    Qualitative, diverging, or sequential colors in the J'M'h space. The
    algorithm has been modeled in analogy to {cmd:HCL}. {it:scheme} can be
    one of the following.
    {p_end}
{p2col:qualitative:}{stata colorpalette JMh qualitative:{bf:qualitative}},
    {stata colorpalette JMh intense:{bf:intense}},
    {stata colorpalette JMh dark:{bf:dark}},
    {stata colorpalette JMh light:{bf:light}},
    {stata colorpalette JMh pastel:{bf:pastel}}
    {p_end}
{p2col:sequential:}{stata colorpalette JMh sequential:{bf:sequential}},
    {stata colorpalette JMh blues:{bf:blues}},
    {stata colorpalette JMh greens:{bf:greens}},
    {stata colorpalette JMh grays:{bf:grays}},
    {stata colorpalette JMh oranges:{bf:oranges}},
    {stata colorpalette JMh purples:{bf:purples}},
    {stata colorpalette JMh reds:{bf:reds}},
    {stata colorpalette JMh heat:{bf:heat}},
    {stata colorpalette JMh heat2:{bf:heat2}},
    {stata colorpalette JMh terrain:{bf:terrain}},
    {stata colorpalette JMh terrain2:{bf:terrain2}},
    {stata colorpalette JMh viridis:{bf:viridis}},
    {stata colorpalette JMh plasma:{bf:plasma}},
    {stata colorpalette JMh redblue:{bf:redblue}}
    {p_end}
{p2col:diverging:}{stata colorpalette JMh diverging:{bf:diverging}},
    {stata colorpalette JMh bluered:{bf:bluered}},
    {stata colorpalette JMh bluered2:{bf:bluered2}},
    {stata colorpalette JMh bluered3:{bf:bluered3}},
    {stata colorpalette JMh greenorange:{bf:greenorange}},
    {stata colorpalette JMh browngreen:{bf:browngreen}},
    {stata colorpalette JMh pinkgreen:{bf:pinkgreen}},
    {stata colorpalette JMh purplegreen:{bf:purplegreen}}

{phang}{cmd:HSV} {it:scheme}{break}
    Qualitative, diverging, or sequential colors in the HSV space. The
    algorithm has been modeled in analogy to {cmd:HCL}. {it:scheme} can be
    one of the following.
    {p_end}
{p2col:qualitative:}{stata colorpalette HSV qualitative:{bf:qualitative}},
    {stata colorpalette HSV intense:{bf:intense}},
    {stata colorpalette HSV dark:{bf:dark}},
    {stata colorpalette HSV light:{bf:light}},
    {stata colorpalette HSV pastel:{bf:pastel}},
    {stata colorpalette HSV rainbow:{bf:rainbow}}
    {p_end}
{p2col:sequential:}{stata colorpalette HSV sequential:{bf:sequential}},
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
{p2col:diverging:}{stata colorpalette HSV diverging:{bf:diverging}},
    {stata colorpalette HSV bluered:{bf:bluered}},
    {stata colorpalette HSV bluered2:{bf:bluered2}},
    {stata colorpalette HSV bluered3:{bf:bluered3}},
    {stata colorpalette HSV greenorange:{bf:greenorange}},
    {stata colorpalette HSV browngreen:{bf:browngreen}},
    {stata colorpalette HSV pinkgreen:{bf:pinkgreen}},
    {stata colorpalette HSV purplegreen:{bf:purplegreen}}

{phang}{cmd:HSL} {it:scheme}{break}
    Qualitative, diverging, or sequential colors in the HSL space. The
    algorithm has been modeled in analogy to {cmd:HCL}. {it:scheme} can be
    one of the following: {stata colorpalette HSL qualitative:{bf:qualitative}},
    {stata colorpalette HSL sequential:{bf:sequential}},
    {stata colorpalette HSL diverging:{bf:diverging}}

{pstd}
    The names of the generators and schemes can be abbreviated and typed in lowercase
    letters. If abbreviation is ambiguous, the first matching name in the
    sorted list of palettes and generators (including all palette types) will be used.

{pstd}
    Given {it:n} (number of colors), {it:H} = {cmd:(}h1{cmd:,} h2{cmd:)} (hues),
    {it:C} = {cmd:(}c1{cmd:,} c2{cmd:)} (chroma levels),
    {it:L} = {cmd:(}l1{cmd:,} l2{cmd:)} (luminance levels),
    {it:P} = {cmd:(}p1{cmd:,} p2{cmd:)} (power coefficients), the {cmd:HCL} generator
    creates HCL colors i = 1,...,{it:n} according to the following
    formulas.

{p2colset 9 23 23 2}{...}
{p2col:qualitative:}HCL[i] =
    {cmd:(}h1 + (h2-h1) * (i-1)/({it:n}-1){cmd:,} c1{cmd:,} l1{cmd:)}

{p2col:sequential:}HCL[i] =
    {cmd:(}h2 - (h2-h1) * j,
    c2 - (c2-c1) * j^p1{cmd:,}
    l2 - (l2-l1) * j^p2{cmd:)}{break}
    with j = ({it:n}-{it:i})/({it:n}-1)

{p2col:diverging:}HCL[i] =
    {cmd:(}cond(j>0, h1, h2){cmd:,}
    c1 * abs(j)^p1{cmd:,}
    l2 - (l2-l1) * abs(j)^p2{cmd:)}{break}
    with j = ({it:n}-2*{it:i}+1)/({it:n}-1)

{pstd}
    The {cmd:LCh}, {cmd:JMh}, {cmd:HSV}, and {cmd:HSL} generator use analogous
    formulas. For qualitative colors, if h2 is omitted, it is set to
    h2 = h1+360*({it:n}-1)/{it:n}. See file
    {help colrspace_library_generators:colrspace_library_generators.sthlp} for
    the parameter settings of the different generators.

{pstd}
    Examples:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.palette("hue", 5)"'}
        . {stata "colorpalette mata(S)"}

        . {stata `"mata: S.palette("HCL diverging", 30)"'}
        . {stata "colorpalette: mata(S)"}

        . {stata `"mata: S.palette("HCL diverging", 30, (30, 100), 70, (50, 98))"'}
        . {stata "colorpalette: mata(S)"}


{marker opint}{...}
{title:Set/retrieve opacity and intensity}

{dlgtab:Set opacity}

{pstd}
    To set the opacity of the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:opacity}[{cmd:_added}]{cmd:(}{it:opacity}[{cmd:,} {it:noreplace}]{cmd:)}

{pstd}
    {it:S}{cmd:.opacity()} sets opacity for all existing colors; use
    {it:S}{cmd:.opacity_added()} if you only want to set opacity for the
    colors that have been added last. Furthermore, use
    {it:S}{cmd:.add_opacity()} or {it:S}{cmd:.add_opacity_added()}
    to leave the existing colors unchanged and append a copy of the colors with the
    new opacity settings. Arguments are as follows.

{phang}
    {it:opacity} is a real vector of opacity values as percentages in [0,100]. A value of 0
    makes the color fully transparent, a value of 100 makes the color fully
    opaque. If the number of specified opacity values is smaller than the
    number of existing colors, the opacity values will be recycled;
    if the number of opacity values is larger than the number of colors, the
    colors will be recycled. To skip assigning opacity to a particular
    color, you may set the corresponding element in {it:opacity} to
    {cmd:.} (missing).

{phang}
    {it:noreplace}!=0 specifies that existing opacity values should not be
    replaced. By default, {it:S}{cmd:.opacity()} resets opacity for all colors
    irrespective of whether they already have an opacity value or not.

{pstd}
    Alternatively, you may type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:alpha}[{cmd:_added}]{cmd:(}{it:alpha}[{cmd:,} {it:noreplace}]{cmd:)}

{pstd}
    where {it:alpha} contains opacity values specified as proportions in [0,1].

{dlgtab:Retrieve opacity}

{pstd}
    To retrieve a real colvector containing the opacity values (as percentages)
    of the colors in {it:S}, type

        {it:opacity} = {it:S}{cmd:.opacity}[{cmd:_added}]{cmd:()}

{pstd}
    {it:opacity} will be equal to {cmd:.} (missing) for colors that do not have
    an opacity value. {it:S}{cmd:.opacity()} returns the opacity values of all
    colors; {it:S}{cmd:.opacity_added()} only returns the opacity values of the
    colors that have been added last.

{pstd}
    Alternatively, you may type

        {it:alpha} = {it:S}{cmd:.alpha}[{cmd:_added}]{cmd:()}

{pstd}
    to retrieve opacity values as proportions.

{marker intensity}{...}
{dlgtab:Set intensity}

{pstd}
    To set the intensity adjustment multipliers of the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:intensity}[{cmd:_added}]{cmd:(}{it:intensity}[{cmd:,} {it:noreplace}]{cmd:)}

{pstd}
    {it:S}{cmd:.intensity()} sets the intensity multipliers for all existing colors; use
    {it:S}{cmd:.intensity_added()} if you only want to set intensity for the
    colors that have been added last. Furthermore, use
    {it:S}{cmd:.add_intensity()} and {it:S}{cmd:.add_intensity_added()}
    to leave the existing colors unchanged and append a copy of the colors with the
    new intensity settings. Arguments are as follows.

{phang}
    {it:intensity} is a real vector of intensity adjustment multipliers in [0,255]. A multiplier
    smaller than 1 makes the color lighter, a multiplier larger than one make the
    color darker. If the number of specified intensity multipliers is smaller than
    the number of existing colors, the intensity multipliers will be
    recycled; if the number of intensity multipliers is larger than the number of
    colors, the colors will be recycled. To skip assigning an intensity multiplier to
    a particular color, you may set the corresponding element in {it:intensity}
    to {cmd:.} (missing).

{phang}
    {it:noreplace}!=0 specifies that existing intensity adjustment multipliers should not be
    replaced. By default, {it:S}{cmd:.intensity()} resets the intensity multipliers for all colors
    irrespective of whether they already have an intensity multiplier or not.

{pstd}
    Note that {it:S}{cmd:.intensity()} does not manipulate the stored
    coordinates of a color, it just adds an extra piece of information. This
    extra information, the intensity multiplier, is added to a color
    specification when exporting the colors using {it:S}{cmd:.colors()}. If you want
    to actually transform the stored color values instead of just recording
    an intensity multiplier, you can use function
    {helpb colrspace##intensify:{it:S}.intensify()}.

{dlgtab:Retrieve intensity}

{pstd}
    To retrieve a real colvector containing the intensity adjustment
    multipliers of the colors in {it:S}, type

        {it:intensity} = {it:S}{cmd:.intensity}[{cmd:_added}]{cmd:()}

{pstd}
    {it:intensity} will be equal to {cmd:.} (missing) for colors that do not have
    an intensity multiplier. {it:S}{cmd:.intensity()} returns the intensity multipliers of all
    colors; {it:S}{cmd:.intensity_added()} only returns the intensity multipliers of the
    colors that have been added last.

{dlgtab:Examples}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.palette("s2", 4)"'}
        . {stata "mata: S.opacity((., 80, ., 60))"}
        . {stata "mata: S.intensity((.7, ., .8, .))"}
        . {stata "mata: S.Colors()"}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("cranberry")"'}
        . {stata "mata: S.intensity(range(1,.1,.10))"}
        . {stata "colorpalette mata(S)"}


{marker recycle}{...}
{title:Recycle, select, and order colors}

{dlgtab:Recycling}

{pstd}
    To recycle the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:recycle}[{cmd:_added}]{cmd:(}{it:n}{cmd:)}

{pstd}
    where {it:n} is a real scalar specifying the number of desired colors.
    {it:S}{cmd:.recycle()} will create {it:n} colors by recycling the colors
    until the desired number of colors is reached. If {it:n} is smaller than
    the number of existing colors, {it:S}{cmd:.recycle()} will select the first
    {it:n} colors. {it:S}{cmd:.recycle()} operates on all existing colors; use
    {it:S}{cmd:.recycle_added()} if you only want to recycle the colors added
    last. Furthermore, use {it:S}{cmd:.add_recycle()} or
    {it:S}{cmd:.add_recycle_added()} to leave the existing colors unchanged and
    append the recycled colors.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow")"'}
        . {stata "mata: S.recycle(7)"}
        . {stata "mata: S.colors()"}
        . {stata "mata: S.recycle(2)"}
        . {stata "mata: S.colors()"}

{marker select}{...}
{dlgtab:Selecting and ordering}

{pstd}
    To select (and order) colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:select}[{cmd:_added}]{cmd:(}{it:p}{cmd:)}

{pstd}
    where {it:p} is a real vector of the positions of the colors to be selected
    (permutation vector). Positive numbers refer to colors from the start;
    negative numbers refer to colors from the end (numbers out of range will
    ignored). Colors not covered in {it:p} will be dropped and the selected
    colors will be ordered as specified in {it:p}. {it:S}{cmd:.select()}
    operates on all existing colors; use {it:S}{cmd:.select_added()} if you
    only want to manipulate the colors added last. Furthermore, use
    {it:S}{cmd:.add_select()} or {it:S}{cmd:.add_select_added()} to leave the
    existing colors unchanged and append the selected colors.

{pstd}
    To drop individual colors in {it:S} (without changing the order of the
    remaining colors), type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:drop}[{cmd:_added}]{cmd:(}{it:p}{cmd:)}

{pstd}
    where {it:p} is a real vector of the positions of the colors to be dropped
    (permutation vector). Positive numbers refer to colors from the start;
    negative numbers refer to colors from the end (numbers out of range will
    ignored). {it:S}{cmd:.drop()} operates on all existing colors; use
    {it:S}{cmd:.drop_added()} if you only want to manipulate the colors added
    last. Furthermore, use {it:S}{cmd:.add_drop()} or
    {it:S}{cmd:.add_drop_added()} to leave the existing colors unchanged and
    append a copy of the colors that have not been dropped.

{pstd}
    To order the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:order}[{cmd:_added}]{cmd:(}{it:p}{cmd:)}

{pstd}
    where {it:p} is a real vector specifying the desired order of the colors
    (permutation vector). Positive numbers refer to colors from the start;
    negative numbers refer to colors from the end (numbers out of range will
    ignored). Colors not covered in
    {it:p} will be placed last, in their original order. {it:S}{cmd:.order()}
    operates on all existing colors; use {it:S}{cmd:.order_added()} if you only
    want to manipulate the colors added last. Furthermore, use
    {it:S}{cmd:.add_order()} or {it:S}{cmd:.add_order_added()} to leave the
    existing colors unchanged and append the reordered colors.

{pstd}
    To reverse the order of the colors in {it:S}, type:

        {it:S}{cmd:.}[{cmd:add_}]{cmd:reverse}[{cmd:_added}]{cmd:()}

{pstd}
    {it:S}{cmd:.reverse()} operates on all existing colors; use
    {it:S}{cmd:.reverse_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_reverse()} or
    {it:S}{cmd:.add_reverse_added()} to leave the existing colors unchanged and
    append the reversed colors. {it:S}{cmd:.reverse()} is equivalent to
    {it:S}{cmd:.order(}{it:S}{cmd:.N()::1)} or
    {it:S}{cmd:.select(}{it:S}{cmd:.N()::1)}.

{pstd}
    To shift the positions of colors up or down, wrapping positions around at
    the top and bottom, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:shift}[{cmd:_added}]{cmd:(}{it:k}{cmd:)}

{pstd}
    where {it:k} specifies the size of the shift. If {it:k} is in (-1,1),
    the colors are shifted by trunc({it:k}*n) positions, where n is the total
    number of colors (proportional shift); if
    abs({it:k})>=1, the colors are shifted by trunc({it:k}) positions. Specify
    {it:k}>0 ({it:k}<0) for a shift in upward (downward)
    direction. {it:S}{cmd:.shift()} operates on all existing colors; use
    {it:S}{cmd:.shift_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_shift()} or
    {it:S}{cmd:.add_shift_added()} to leave the existing colors unchanged and
    append the shifted colors.

{pstd}
    Examples:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow blue green")"'}
        . {stata "mata: S.select((4,3,4))"}
        . {stata "mata: S.colors()"}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow blue green")"'}
        . {stata "mata: S.drop(-2)"}       {it:(drop second last)}
        . {stata "mata: S.colors()"}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow blue green")"'}
        . {stata "mata: S.order((4,3,4))"}
        . {stata "mata: S.colors()"}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow blue green")"'}
        . {stata "mata: S.reverse()"}
        . {stata "mata: S.colors()"}

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow blue green")"'}
        . {stata "mata: S.shift(2)"}
        . {stata "mata: S.colors()"}


{marker ipolate}{...}
{title:Interpolate and mix}

{dlgtab:Interpolation}

{pstd}
    To apply linear interpolation to the colors in {it:S}, type:

        {it:S}{cmd:.}[{cmd:add_}]{cmd:ipolate}[{cmd:_added}]{cmd:(}{it:n}[{cmd:,} {it:space}{cmd:,} {it:range}{cmd:,} {it:power}{cmd:,} {it:positions}{cmd:,} {it:padded}]{cmd:)}

{pstd}
    Opacity values and intensity adjustment multipliers, if existing, will also be interpolated.
    {it:S}{cmd:.ipolate()} takes all existing colors as input and replaces them with
    the interpolated colors; use {it:S}{cmd:.ipolate_added()} if you only want to interpolate the
    colors added last. Furthermore, use {it:S}{cmd:.add_ipolate()} or {it:S}{cmd:.add_ipolate_added()}
    to leave the existing colors unchanged and append the interpolated
    colors. Arguments are as follows.

{phang}
    {it:n} is a real scalar specifying the number of destination colors. {it:S}{cmd:.ipolate()}
    will interpolate the existing (origin) colors to {it:n} new
    colors (thus increasing or decreasing the number of colors, depending on whether
    {it:n} is larger or smaller than the number of origin colors).

{phang}
    {it:space} selects the color space in which the colors are
    interpolated. {it:space} can be {cmd:"RGB"}, {cmd:"lRGB"}, {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"CMYK"}, {cmd:"XYZ"},
    {cmd:"xyY"}, {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"},
    {cmd:"HCL"}, {cmd:"CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:"},
    {cmd:"JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:"}, or
    {cmd:"Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:"}
    (lowercase spelling allowed). The default is
    {cmd:"Jab"}. This default can also be selected by typing {cmd:""}. When
    interpolating from one hue to the next (relevant for {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"LCh"}, {cmd:"HCL"}, {cmd:"JMh"}, and {cmd:"CAM02"} when
    {it:mask} contains {cmd:h}), {it:S}{cmd:.ipolate()} will travel around the
    color wheel in the direction in which the two hues are closer to each other
    (based on the original order of colors in {it:S}; the rule may be violated
    if colors are reordered through argument {it:positions}).

{phang}
    {it:range} = {cmd:(}{it:lb}[{cmd:,} {it:ub}]{cmd:)} specifies range of the
    destination colors. The default is {cmd:(0,1)}. This default can also be
    selected by typing {cmd:.} (missing). If {it:lb} is larger than {it:ub},
    the destination colors will be arranged in reverse order. Extrapolation
    will be applied if the specified range exceeds [0,1].

{phang}
    {it:power} is a real scalar affecting the distribution of the destination
    colors across {it:range}. The default is to distribute them evenly. This
    default can also be selected by typing {cmd:.} (missing) or setting
    {it:power} to 1. A {it:power} value larger than 1 squishes the positions
    towards {it:lb}. If interpolating between two colors, this means that the
    first color will dominate most of the interpolation range (slow to fast
    transition). A value between 0 and 1 squishes the positions towards
    {it:ub}, thus making the second color the dominant color for most of the
    range (fast to slow transition). Another way to think of the effect of
    {it:power} is that it moves the center of the color gradient up (if {it:power}
    is larger than 1) or down (if {it:power} is between 0 and 1).

{phang}
    {it:positions} is a real vector specifying the positions of the origin
    colors. The default is to place them on a regular grid from 0
    and 1. This default can also be selected by typing {cmd:.} (missing). If
    {it:positions} has less elements than there are colors, default
    positions are used for the remaining colors. If the same position is
    specified for multiple colors, these colors will be averaged before
    applying interpolation.

{phang}
    {it:padded}!=0 requests padded interpolation. By default, if {it:padded} is
    omitted or equal to 0, the first color and the last color are taken as the
    end points of the interpolation range; these colors thus remain unchanged
    (as long as default settings are used for {it:range} and {it:position}). If
    {it:padded}!=0, the positions of the colors are interpreted as interval
    midpoints, such that the interpolation range is padded by half an interval on
    each side. This causes the destination colors to be spread out slightly
    more (less) than the origin colors, if the number of destination colors is
    larger (smaller) than the number of origin colors.

{pstd}
    Circular interpolation is used if {it:S}{cmd:.pclass()} is equal to
    {cmd:"circular"} or {cmd:"cyclic"}. In this case, arguments {it:range},
    {it:power}, {it:positions}, and {it:padded} will be ignored. For regular
    (noncircular) interpolation, which is applied if {it:S}{cmd:.pclass()}
    is different from {cmd:"circular"} or {cmd:"cyclic"}, these arguments
    can be used to fine-tune the interpolation.

{pstd}
    Examples:

        . {stata "mata: Jab = ColrSpace()"}
        . {stata `"mata: Jab.colors("#337ab7 #f0ad4e")"'}
        . {stata `"mata: JMh = J(1, 1, Jab)"'}             {it:(make copy)}
        . {stata `"mata: Jab.ipolate(30)"'}
        . {stata `"mata: JMh.ipolate(30, "JMh")"'}
        . {stata "colorpalette: mata(Jab) / mata(JMh)"}

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.colors("#fafa6e #2A4858")"'}
        . {stata `"mata: B = C = D = J(1, 1, A)"'}         {it:(make copies)}
        . {stata `"mata: A.ipolate(30, "HCL")"'}
        . {stata `"mata: B.ipolate(30, "HCL", (.1,.9))"'}  {it:(select range)}
        . {stata `"mata: C.ipolate(30, "HCL", ., 1.5)"'}   {it:(make 1st color dominant)}
        . {stata `"mata: D.ipolate(30, "HCL", ., .6)"'}    {it:(make 2nd color dominant)}
        . {stata "colorpalette: mata(A) / mata(B) / mata(C) / mata(D)"}

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.colors("black red yellow")"'}
        . {stata `"mata: B = C = J(1, 1, A)"'}
        . {stata `"mata: A.ipolate(30)"'}                       {it:(red in middle)}
        . {stata `"mata: B.ipolate(30, "", ., ., (0, .3, 1))"'} {it:(shift left)}
        . {stata `"mata: C.ipolate(30, "", ., ., (0, .7, 1))"'} {it:(shift right)}
        . {stata "colorpalette: mata(A) / mata(B) / mata(C)"}

{marker mix}{...}
{dlgtab:Mixing}

{pstd}
    To mix (i.e. average) the colors in {it:S}, type:

        {it:S}{cmd:.}[{cmd:add_}]{cmd:mix}[{cmd:_added}]{cmd:(}[{it:space}{cmd:,} {it:w}]{cmd:)}

{pstd}
    Opacity values and intensity adjustment multipliers, if defined, will also be mixed
    (i.e. averaged). {it:S}{cmd:.mix()} takes all existing colors as input and
    replaces them with the mixed color; use {it:S}{cmd:.mix_added()} if you
    only want to mix the colors added last. Furthermore, use
    {it:S}{cmd:.add_mix()} or {it:S}{cmd:.add_mix_added()} to leave the
    existing colors unchanged and append the mixed color. Arguments are as
    follows.

{phang}
    {it:space} selects the color space in which the colors are
    mixed. {it:space} can be {cmd:"RGB"}, {cmd:"lRGB"}, {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"CMYK"}, {cmd:"XYZ"},
    {cmd:"xyY"}, {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"},
    {cmd:"HCL"}, {cmd:"CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:"},
    {cmd:"JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:"}, or
    {cmd:"Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:"}
    (lowercase spelling allowed). The default is {cmd:"Jab"}. This default can
    also be selected by typing {cmd:""}. When mixing hues (relevant for
    {cmd:"HSV"}, {cmd:"HSL"}, {cmd:"LCh"}, {cmd:"HCL"}, {cmd:"JMh"}, and
    {cmd:"CAM02"} when {it:mask} contains {cmd:h}), {it:S}{cmd:.mix()} will
    compute the mean of angles as described at
    {browse "http://en.wikipedia.org/wiki/Mean_of_circular_quantities":Wikipedia (2018e)}
    (using weighted sums of the cartesian coordinates if weights are specified); this
    is slightly different from the procedure employed by {it:S}{cmd:.ipolate()}.

{phang}
    {it:w} is a real vector containing weights. Color mixing works by
    transforming the colors to the selected color space, taking the means of
    the attributes across colors, and then transforming the resulting "average"
    color back to the original space. {it:w} specifies the weights given to the
    individual colors when computing the means. If {it:w} contains less
    elements than there are colors, the weights will be recycled. Omit {it:w},
    or specify {it:w} as {cmd:1} or as {cmd:.} (missing) to use unweighted
    means.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("black red yellow")"'}
        . {stata "mata: S.get()"}
        . {stata `"mata: S.mix("lRGB")"'}
        . {stata "mata: S.get()"}
        . {stata `"mata: S.colors("black red yellow")"'}
        . {stata `"mata: S.mix("lRGB", (.5, 1, 1))"'}
        . {stata "mata: S.get()"}


{marker intensify}{...}
{title:Intensify, saturate, luminate}

{dlgtab:Intensify}

{pstd}
    To adjust the intensity of the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:intensify}[{cmd:_added}]{cmd:(}{it:m}{cmd:)}

{pstd}
    where {it:m} is a real vector of intensity adjustment multipliers in [0,255]. A
    multiplier smaller than 1 makes the color lighter, a multiplier larger than one make the
    color darker. If the number of specified multipliers is smaller than
    the number of colors, the multipliers will be
    recycled; if the number of multipliers is larger than the number of
    colors, the colors will be recycled. To skip adjusting the intensity of a
    particular color, you may set the corresponding multiplier to
    {cmd:.} (missing). {it:S}{cmd:.intensify()} operates on all existing colors; use
    {it:S}{cmd:.intensify_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_intensify()} or
    {it:S}{cmd:.add_intensify_added()} to leave the existing colors unchanged and
    append the manipulated colors.

{pstd}
    {cmd:ColrSpace} uses the same algorithm as is used in official Stata to adjust
    the color intensity. Applying {it:S}{cmd:.intensify()} thus results in colors
    that look the same as colors that have been specified using intensity
    multiplier syntax (see help {help colorstyle##intensity:{it:colorstyle}}). The
    algorithm works by increasing or decreasing the RGB
    values proportionally, with rounding to the nearest
    integer and adjustment to keep all values within [0,255].

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("navy maroon forest_green")"'}
        . {stata `"mata: S.select((1,1,2,2,3,3))"'}     {it:(duplicate colors)}
        . {stata `"mata: S.intensify((., .5))"'}
        . {stata "colorpalette mata(S), rows(2)"}

{pstd}
    If intensity multipliers have been set by
    {helpb colrspace##string:{it:S}.colors()} or
    {helpb colrspace##intensity:{it:S}.intensity()}, you can type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:Intensify}[{cmd:_added}]{cmd:()}

{pstd}
    (note the capital I) to adjust the colors by these multipliers and
    then clear the information on intensity multipliers. {it:S}{cmd:.Intensify()}
    operates on all existing colors; use
    {it:S}{cmd:.Intensify_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_Intensify()} or
    {it:S}{cmd:.add_Intensify_added()} to leave the existing colors unchanged and
    append the manipulated colors.

{marker saturate}{...}
{dlgtab:Saturate}

{pstd}
    To change the saturation (colorfulness) of the colors in {it:S}, type:

        {it:S}{cmd:.}[{cmd:add_}]{cmd:saturate}[{cmd:_added}]{cmd:(}{it:d}[{cmd:,} {it:method}{cmd:,} {it:level}]{cmd:)}

{pstd}
    {it:S}{cmd:.saturate()} operates on all existing colors; use
    {it:S}{cmd:.saturate_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_saturate()} or
    {it:S}{cmd:.add_saturate_added()} to leave the existing colors unchanged and
    append the manipulated colors. Arguments are as follows.

{phang}
    {it:d} is a real vector of saturation adjustments addends. Positive values
    increase saturation, negative values decrease saturation. If the number of
    specified addends is smaller than the number of colors, the addends will be
    recycled; if the number of addends is larger than the number of colors, the
    colors will be recycled. To skip adjusting a particular color, you may set
    the corresponding addend to {cmd:.} (missing). Typically, reasonable
    addends are in a range of about +/- 50.

{phang}
    {it:method} selects the color space in which the colors are
    manipulated. It can be {cmd:"LCh"}, {cmd:"HCL"}, {cmd:"JCh"} (CIECAM02 JCh),
    or {cmd:"JMh"} (lowercase spelling allowed). The
    default is {cmd:"LCh"}. This default can also be selected by typing
    {cmd:""}. {it:S}{cmd:.saturate()} works by converting the colors to
    the selected color space, adding {it:d} to the C channel (or M'
    in case of J'M'h), and then converting the colors back (after resetting
    negative chroma values to zero).

{phang}
    {it:level}!=0 specifies that {it:d} provides chroma levels, not addends. In
    this case, the C channel will be set to {it:d}. Reasonable values typically
    lie in a range of 0-100, although higher values are possible. Negative values will
    be reset to 0.

{pstd}
    Example:

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.palette("RdYlGn")"'}
        . {stata `"mata: B = J(1, 1, A)"'}           {it:(make copy of A)}
        . {stata `"mata: B.saturate(25)"'}
        . {stata "colorpalette: mata(A) / mata(B)"}

{pstd}
    {it:S}{cmd:.saturate()} has been inspired by the
    {cmd:saturate()} and {cmd:desaturate()} functions in Gregor Aisch's
    {browse "http://gka.github.io/chroma.js/":chroma.js}.

{marker luminate}{...}
{dlgtab:Luminate}

{pstd}
    To change the luminance of the colors in {it:S}, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:luminate}[{cmd:_added}]{cmd:(}{it:d}[{cmd:,} {it:method}{cmd:,} {it:level}]{cmd:)}

{pstd}
    {it:S}{cmd:.luminate()} operates on all existing colors; use
    {it:S}{cmd:.luminate_added()} if you only want to manipulate the colors
    added last. Furthermore, use {it:S}{cmd:.add_luminate()} or
    {it:S}{cmd:.add_luminate_added()} to leave the existing colors unchanged and
    append the manipulated colors. Arguments are as follows.

{phang}
    {it:d} is a real vector of luminance adjustments addends. Positive values
    increase luminance, negative values decrease luminance. If the number of
    specified addends is smaller than the number of colors, the addends will be
    recycled; if the number of addends is larger than the number of colors, the
    colors will be recycled. To skip adjusting a particular color, you may set
    the corresponding addend to {cmd:.} (missing). Typically, reasonable
    addends are in a range of about +/- 50.

{phang}
    {it:method} selects the color space in which the colors are manipulated. It
    can be {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"}, {cmd:"HCL"}, {cmd:"JCh"}
    (CIECAM02 JCh), {cmd:"JMh"} or {cmd:"Jab"} (lowercase spelling
    allowed). The default is {cmd:"JMh"}. This default can also be selected by
    typing {cmd:""}. {it:S}{cmd:.luminate()} works by converting the colors to
    the selected color space, adding {it:d} to the L channel (or J in case of
    CIECAM02 JCh, J' in case of J'M'h or J'a'b'), and then converting the
    colors back (after resetting negative luminance values to zero). Results
    will be identical between {cmd:"Lab"} and {cmd:"LCh"}, between {cmd:"Luv"}
    and {cmd:"HCL"}, and between {cmd:"JMh"} and {cmd:"Jab"}.

{phang}
    {it:level}!=0 specifies that {it:d} provides luminance levels, not addends. In
    this case, the L channel will be set to {it:d}. Reasonable values typically
    lie in a range of 0-100. Negative values will be reset to 0.

{pstd}
    Example:

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.palette("ptol", 10)"'}
        . {stata `"mata: B = J(1, 1, A)"'}           {it:(make copy of A)}
        . {stata `"mata: B.luminate(20)"'}
        . {stata "colorpalette, lc(black): mata(A) / mata(B)"}

{pstd}
    {it:S}{cmd:.luminate()} has been inspired by the
    {cmd:darken()} and {cmd:brighten()} functions in Gregor Aisch's
    {browse "http://gka.github.io/chroma.js/":chroma.js}.


{marker gray}{...}
{title:Grayscale conversion}

{pstd}
    To convert the colors in {it:S} to gray, type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:gray}[{cmd:_added}]{cmd:(}[{it:p}{cmd:,} {it:method}]{cmd:)}

{pstd}
    {it:S}{cmd:.gray()} transforms all existing colors; use
    {it:S}{cmd:.gray_added()} if you only want to transform the colors
    added last. Furthermore, use {it:S}{cmd:.add_gray()} or
    {it:S}{cmd:.add_gray_added()} to leave the existing colors unchanged and
    append the transformed colors. Arguments are as follows.

{phang}
    {it:p} is a real vector of proportions of gray, with {it:p} in [0,1]. The
    default is {it:p} = {cmd:1} (complete conversion to gray). If the number of
    specified proportions is smaller than the number of colors, the proportions
    will be recycled; if the number of proportions is larger than the number of
    colors, the colors will be recycled. To skip converting a particular color,
    you may set the corresponding proportion to {cmd:.} (missing).

{phang}
    {it:method} specifies the color space in which the colors are
    manipulated. It can be {cmd:"LCh"}, {cmd:"HCL"}, {cmd:"JCh"} (CIECAM02 JCh),
    or {cmd:"JMh"} (lowercase spelling allowed). The
    default is {cmd:"LCh"}. This default can also be selected by typing
    {cmd:""}. Grayscale conversion works by converting the colors the selected
    color space, reducing the C channel (or M' in case of J'M'h) towards zero,
    and then converting the colors back.

{pstd}
    Opacity settings and intensity adjustment multipliers are ignored when
    converting the colors.

{pstd}
    Example:

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.palette("s1")"'}
        . {stata `"mata: B = C = J(1, 1, A)"'}      {it:(make copies)}
        . {stata `"mata: B.gray(.7)"'}
        . {stata `"mata: C.gray()"'}
        . {stata "colorpalette, lc(black): mata(A) / mata(B) / mata(C)"}

{pstd}
    Grayscale conversion is also supported by {it:S}{cmd:.convert()}; see
    {help colrspace##convert:Convert colors without storing}.


{marker cvd}{...}
{title:Color vision deficiency simulation}

{pstd}
    To convert the colors in {it:S} such that they look how they would appear to
    people suffering from color vision deficiency (color blindness), type

        {it:S}{cmd:.}[{cmd:add_}]{cmd:cvd}[{cmd:_added}]{cmd:(}[{it:p}{cmd:,} {it:method}]{cmd:)}

{pstd}
    {it:S}{cmd:.cvd()} transforms all existing colors; use
    {it:S}{cmd:.cvd_added()} if you only want to transform the colors
    added last. Furthermore, use {it:S}{cmd:.add_cvd()} or
    {it:S}{cmd:.add_cvd_added()} to leave the existing colors unchanged and
    append the transformed colors. Arguments are as follows.

{phang}
    {it:p} is a real vector of deficiency severities, with {it:p} in [0,1]. The
    default is {it:p} = {cmd:1} (maximum severity, i.e. deuteranopia,
    protanopia, or tritanopia, respectively). If the number of specified
    severities is smaller than the number of colors, the severities will be
    recycled; if the number of severities is larger than the number of colors,
    the colors will be recycled. To skip converting a particular color, you may
    set the corresponding severity to {cmd:.} (missing).

{phang}
    {it:method} specifies the type of color vision deficiency. It can be
    {cmd:"deuteranomaly"}, {cmd:"protanomaly"}, or {cmd:"tritanomaly"}
    (abbreviations allowed). The default is {cmd:"deuteranomaly"}. This default
    can also be selected by typing {cmd:""}. See
    {browse "http://en.wikipedia.org/wiki/Color_blindness":Wikipedia (2019a)} for basic
    information on the different types of color blindness.

{pstd}
    {cmd:ColrSpace} implements color vision deficiency simulation based on
    {browse "http://doi.org/10.1109/TVCG.2009.113":Machado et al. (2009)}, using the
    transformation matrices provided at
    {browse "http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html":www.inf.ufrgs.br/~oliveira}
    (employing linear interpolation between matrices for intermediate severity values). The
    transformations matrix for a specific combination of (scalar) {it:p} and {it:method} can be
    retrieved as follows:

        {it:M} = {it:S}{cmd:.cvd_M(}[{it:p}{cmd:,} {it:method}]{cmd:)}

{pstd}
    Opacity settings and intensity adjustment multipliers are ignored when
    converting the colors.

{pstd}
    Example:

        . {stata "mata: A = ColrSpace()"}
        . {stata `"mata: A.palette("s2", 5)"'}
        . {stata `"mata: d = D = p = P = T = J(1, 1, A)"'}  {it:(make copies)}
        . {stata `"mata: d.cvd(.5);      d.name("deuteranomaly")"'}
        . {stata `"mata: D.cvd();        D.name("deuteranopia")"'}
        . {stata `"mata: p.cvd(.5, "p"); p.name("protanomaly")"'}
        . {stata `"mata: P.cvd(1, "p");  P.name("protanopia")"'}
        . {stata `"mata: T.cvd(1, "t");  T.name("tritanopia")"'}
        . {stata "colorpalette, lc(black): m(A) / m(d) / m(D) / m(p) / m(P) / m(T)"}

{pstd}
    Color vision deficiency simulation is also supported by {it:S}{cmd:.convert()}; see
    {help colrspace##convert:Convert colors without storing}.


{marker delta}{...}
{title:Color differences and contrast ratios}

{dlgtab:Color differences}

{pstd}
    To compute differences between colors in {it:S}, type

        {it:D} = {it:S}{cmd:.delta}[{cmd:_added}]{cmd:(}[{it:P}{cmd:,} {cmd:"}{it:method}{cmd:"}{cmd:,} {it:noclip}]{cmd:)}

{pstd}
    where {it:P} is a {it:r} x 2 matrix with each row selecting two colors to
    be compared. For example, {it:P} = {cmd:(3,5)} would compare the 3rd and
    the 5th color; {it:P} = {cmd:(1,2) \ (3,5)} would make two comparisons: 1st
    to 2nd and 3rd to 5th. The default, if {it:P} is omitted, is to make
    {it:n}-1 consecutive comparisons, where {it:n} is the number of existing
    colors: 1st to 2nd, 2nd to 3rd, ..., ({it:n}-1)th
    to {it:n}th; this is equivalent to {it:P} =
    {cmd:((1::}{it:S}{cmd:.N()-1),(2::}{it:S}{cmd:.N()))}. This default
    can also be selected by typing {cmd:.} (missing). {it:S}{cmd:.delta()}
    operates on all existing colors, that is, {it:P} selects among all colors;
    in {it:S}{cmd:.delta_added()} {it:P} only selects among the colors added
    last. Further options are as follows.

{phang}
    {it:method} selects the method used to compute the color differences. It can
    be one of the following (lowercase spelling and abbreviations allowed):

{p2colset 13 28 30 2}{...}
{p2col:{cmd:Jab} [{it:{help colrspace##JMh:coefs}}]}compute the differences
    from the perceptually uniform CIECAM J'a'b' space as described by
    {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013, chapter 2.6.1)}
{p_end}
{p2col:{cmd:E76}}1976 CIELAB Delta E definition (euclidean distance
    in the CIE L*a*b* color space)
{p_end}
{p2col:{cmd:E94}}1994 CIELAB Delta E definition (based on the description by
    {browse "http://www.brucelindbloom.com/Eqn_DeltaE_CIE94.html":Lindbloom 2017b},
    but using a modification to make the differences symmetric as suggested
    by {browse "http://doi.org/10.1002/0470024275":Hunt 2004:670} to avoid asymmetry)
{p_end}
{p2col:{cmd:E2000}}2000 CIELAB Delta E definition (based on the description given by
    {browse "http://www.brucelindbloom.com/Eqn_DeltaE_CIE2000.html":Lindbloom 2017c})
{p_end}
{p2col:{it:space}}compute the differences as euclidean distances in the
    respective color space, where {it:space} may be {cmd:RGB}, {cmd:RGB1},
    {cmd:lRGB}, {cmd:XYZ}, {cmd:XYZ1},
    {cmd:xyY1}, {cmd:Lab}, {cmd:LCh}, {cmd:Luv}, {cmd:HCL},
    {cmd:JCh} (CIECAM02 JCh), or
    {cmd:JMh}{space 1}[{it:{help colrspace##JMh:coefs}}]
{p_end}

{pmore}
    The default is {cmd:Jab}. This default can also be selected by typing
    {cmd:""}. For background information on color difference also see
    {browse "http://en.wikipedia.org/wiki/Color_difference":Wikipedia (2019b)}.

{phang}
    {it:noclip}!=0 prevents converting the colors to valid RGB values before
    computing the differences. By default, {it:S}{cmd:.delta()} translates the
    colors to linear RGB and clips the coordinates at 0 and 1, before converting
    the colors to the color space selected by {it:method}, so that the
    computed differences are consistent with how the colors are perceived
    on an RGB device. Specify {it:noclip}!=0 to skip this extra step.

{pstd}
    Opacity settings and intensity adjustment multipliers are ignored when computing the
    color differences.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("#337ab7 #f0ad4e")"'}
        . {stata `"mata: S.ipolate(6, "", (0, .5))"'}
        . {stata `"mata: S.delta((J(5,1,1), (2::6)))"'}     {it:(compare 1st to other colors)}

          {it:(illustrate using a graph ...)}
        . {stata `"mata: D = S.delta((J(5,1,1), (2::6)))"'}
        . {stata `"mata: D = `"""' :+ "{&Delta}E' = " :+ strofreal(D,"%9.3g") :+ `"""'"'}
        . {stata `"mata: D = strofreal(1::5) :+ " 3 " :+  D"'}
        . {stata `"mata: st_local("D", invtokens(D'))"'}
        . {stata `"colorpalette mata(S), order(1 1 1 1 1) gropts(text(`D'))"'}

{marker contrast}{...}
{dlgtab:Contrast ratios}

{pstd}
    To compute contrast ratios between colors in {it:S}, type

        {it:R} = {it:S}{cmd:.contrast}[{cmd:_added}]{cmd:(}[{it:P}]{cmd:)}

{pstd}
    where {it:P} is a {it:r} x 2 matrix with each row selecting two colors to
    be compared. For example, {it:P} = {cmd:(3,5)} would compare the 3rd and
    the 5th color; {it:P} = {cmd:(1,2) \ (3,5)} would make two comparisons: 1st
    to 2nd and 3rd to 5th. The default, if {it:P} is omitted, is to make
    {it:n}-1 consecutive comparisons, where {it:n} is the number of existing
    colors: 1st to 2nd, 2nd to 3rd, ..., ({it:n}-1)th
    to {it:n}th; this is equivalent to {it:P} =
    {cmd:((1::}{it:S}{cmd:.N()-1),(2::}{it:S}{cmd:.N()))}. This default
    can also be selected by typing {cmd:.} (missing). {it:S}{cmd:.contrast()}
    operates on all existing colors, that is, {it:P} selects among all colors;
    in {it:S}{cmd:.contrast_added()} {it:P} only selects among the colors added last.

{pstd}
    The contrast ratios are computed according to the Web Content Accessibility
    Guidelines (WCAG) 2.0 at
    {browse "http://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef":www.w3.org}. Let
    Y0 be the Y attribute of the lighter color, and Y1 be the
    Y attribute of the darker color, in CIE XYZ space (in Y_white =
    100 scaling). The contrast ratio is then defined as (Y0 + 5) / (Y1 + 5).
    Typically, a contrast ratio of at least 4.5 is recommended between
    foreground text and background fill.

{pstd}
    Opacity settings and intensity adjustment multipliers are ignored when computing the
    contrast ratios.

{pstd}
    Example: Say, you want to print text inside bars and want the
    text and the bar fill to have the same basic color. One idea is to use colors
    with reduced intensity for the fill and print the text in the original
    color. {it:S}{cmd:.contrast()} may be helpful for finding out by how much you
    need to reduce intensity so that there is enough contrast between text and
    bar fill.

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.colors("navy maroon")"'}
        . {stata `"mata: S.add_intensify(.6)"'}
        . {stata `"mata: S.contrast((1,3) \ (2,4))"'} {it:(not enough contrast)}
        . {stata `"mata: C = S.Colors()"'}
        . {stata `"mata: t = `" 2 "Text", c(%s) box m(medium) bc(%s)"'"'}
        . {stata `"mata: st_local("t1", sprintf("1"+t, C[1], C[3]))"'}
        . {stata `"mata: st_local("t2", sprintf("2"+t, C[2], C[4]))"'}
        . {stata `"colorpalette mata(S), gropts(text(`t1') text(`t2'))"'}

        . {stata `"mata: S.select((1,2))"'}
        . {stata `"mata: S.add_intensify((.4,.3))"'}
        . {stata `"mata: S.contrast((1,3) \ (2,4))"'} {it:(contrast now ok)}
        . {stata `"mata: C = S.Colors()"'}
        . {stata `"mata: t = `" 2 "Text", c(%s) box m(medium) bc(%s)"'"'}
        . {stata `"mata: st_local("t1", sprintf("1"+t, C[1], C[3]))"'}
        . {stata `"mata: st_local("t2", sprintf("2"+t, C[2], C[4]))"'}
        . {stata `"colorpalette mata(S), gropts(text(`t1') text(`t2'))"'}


{marker io}{...}
{title:Import/export colors in various spaces}

{dlgtab:Import colors}

{pstd}
    As an alternative to {help colrspace##string:{it:S}{bf:.colors()}}, colors can be imported into
    {it:S} using the following functions:

        {it:S}{cmd:.set(}{it:C}[{cmd:,} {it:space}]{cmd:)}
        {it:S}{cmd:.add(}{it:C}[{cmd:,} {it:space}]{cmd:)}
        {it:S}{cmd:.reset}[{cmd:_added}]{cmd:(}{it:C}[{cmd:,} {it:space}{cmd:,} {it:p}]{cmd:)}

{pstd}
    {it:S}{cmd:.set()} replaces preexisting colors by the new
    colors; use {it:S}{cmd:.add()} if you want to append the new colors to the
    existing colors. {it:S}{cmd:.reset()} can be used to reset the values of
    colors, without reinitializing opacity and intensity
    adjustment; {it:S}{cmd:.reset_added()} is like {it:S}{cmd:.reset()}
    but only operates on the colors that have been added last. The arguments are as
    follows.

{phang}
    {it:C} provides the color values. In case of {it:space} = {cmd:"HEX"},
    {it:C} is a string vector of length {it:n} containing {it:n} hex RGB
    values; in case of {it:space} = {cmd:"CMYK"}, {cmd:"CMYK1"}, {cmd:"RGBA"},
    or {cmd:"RGBA1"}, {it:C} is a {it:n} x 4 real matrix; in case of {it:space} =
    {bind:{cmd:"CAM02} {it:mask}{cmd:"}}, {it:C} is a {it:n} x
    {cmd:strlen(}{it:mask}{cmd:)} real matrix; in all
    other cases, {it:C} is a {it:n} x 3 real matrix of {it:n} color values
    in the respective space. In case of {it:S}{cmd:.reset()} the number of colors in
    {it:C} must match the length of {it:p}.

{phang}
    {it:space} is a string scalar specifying the color space of {it:C}. It can
    be {cmd:"HEX"}, {cmd:"RGB"}, {cmd:"RGB1"}, {cmd:"lRGB"}, {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"CMYK"}, {cmd:"CMYK1"}, {cmd:"XYZ"}, {cmd:"XYZ1"},
    {cmd:"xyY"}, {cmd:"xyY1"}, {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"},
    {cmd:"HCL"}, {cmd:"CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:"},
    {cmd:"JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:"},
    {cmd:"Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:"}, {cmd:"RGBA"}, or {cmd:"RGBA1"}
    (lowercase spelling allowed). The default is {cmd:"RGB"}. This default can
    also be selected by typing {cmd:""}.

{phang}
    {it:p} is a real vector of the positions of the colors to be modified.
    Positive numbers refer to colors from the start; negative numbers refer to
    colors from the end. {it:S}{cmd:.reset()} aborts with error if {it:p}
    addresses positions that do not exists. If {it:p} is omitted, the default
    is to modify all colors. This default can also be selected by typing
    {cmd:.} (missing).

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.set((100,150,200) \ (200,50,50), "RGB")"'}
        . {stata `"mata: S.add((100,50,50) \ (200,50,50) \ (300,50,50), "HCL")"'}
        . {stata "colorpalette mata(S)"}
        . {stata `"mata: S.reset((100,50,50) \ (100,-20,10), "Luv", (2,-1))"'}
        . {stata "colorpalette mata(S)"}
        . {stata `"mata: S.set((100,150,200,.8) \ (200,50,50,.7) \ (100,200,50,1), "RGBA")"'}
        . {stata "colorpalette mata(S)"}

{marker get}{...}
{dlgtab:Export colors}

{pstd}
    To retrieve the colors from {it:S} and return them in a particular
    color space, type

        {it:C} = {it:S}{cmd:.get}[{cmd:_added}]{cmd:(}[{it:space}]{cmd:)}

{pstd}
    where {it:space} is a string scalar specifying the color space. It can
    be {cmd:"HEX"}, {cmd:"RGB"}, {cmd:"RGB1"}, {cmd:"lRGB"}, {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"CMYK"}, {cmd:"CMYK1"}, {cmd:"XYZ"}, {cmd:"XYZ1"},
    {cmd:"xyY"}, {cmd:"xyY1"}, {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"},
    {cmd:"HCL"}, {cmd:"CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:"},
    {cmd:"JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:"},
    {cmd:"Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:"}, {cmd:"RGBA"}, or {cmd:"RGBA1"}
    (lowercase spelling allowed). The default is {cmd:"RGB"}. This default can
    also be selected by typing {cmd:""}. {it:S}{cmd:.get()} returns all
    colors; {it:S}{cmd:.get_added()} only returns the colors that have been
    added last.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.palette("s2",5)"'}
        . {stata `"mata: S.Colors()"'}
        . {stata `"mata: S.get()"'}
        . {stata `"mata: S.get("RGB1")"'}
        . {stata `"mata: S.get("lRGB")"'}
        . {stata `"mata: S.get("XYZ")"'}
        . {stata `"mata: S.get("Lab")"'}
        . {stata `"mata: S.get("Jab")"'}
        . {stata `"mata: S.get("Jab LCD")"'}
        . {stata `"mata: S.get("CAM02 QsH")"'}
        . {stata `"mata: S.opacity((100,90,80,70,60))"'}
        . {stata `"mata: S.get("RGBa")"'}


{marker util}{...}
{title:Color converter and other utilities}

    {help colrspace##convert:Convert colors without storing}
    {help colrspace##cvalid:Check validity of color specification}
    {help colrspace##namedcolors:Obtain list of named colors}
    {help colrspace##pexists:Check whether palette exists}
    {help colrspace##palettes:Obtain list of available palettes}
    {help colrspace##colipolate:Interpolation}
    {help colrspace##colrecycl:Recycling}
    {help colrspace##lsmap:Linear segmented colormaps}
    {help colrspace##clip:Clipping}

{marker convert}{...}
{dlgtab:Convert colors without storing}

{pstd}
    Instead of storing colors in {it:S} using {help colrspace##io:{it:S}{bf:.set()}} and then
    retrieving the colors in a particular space using function {help colrspace##get:{it:S}{bf:.get()}},
    colors can also be converted directly from from one space to another using the
    {it:S}{cmd:.convert()} function. {it:S}{cmd:.convert()} will not
    store any colors or otherwise manipulate the content of {it:S}. The
    syntax is:

        {it:C} = {it:S}{cmd:.convert(}{it:C0}{cmd:,} {it:from}{cmd:,} {it:to}{cmd:)}

{pstd}
    where {it:C0} is a matrix of input colors values in color space {it:from},
    and {it:to} is a destination color space. {it:from} and {it:to} can be
    {cmd:"HEX"}, {cmd:"RGB"}, {cmd:"RGB1"}, {cmd:"lRGB"}, {cmd:"HSV"},
    {cmd:"HSL"}, {cmd:"CMYK"}, {cmd:"CMYK1"}, {cmd:"XYZ"}, {cmd:"XYZ1"},
    {cmd:"xyY"}, {cmd:"xyY1"}, {cmd:"Lab"}, {cmd:"LCh"}, {cmd:"Luv"},
    {cmd:"HCL"}, {cmd:"CAM02} [{help colrspace##CAM02:{it:mask}}]{cmd:"},
    {cmd:"JMh} [{it:{help colrspace##JMh:coefs}}]{cmd:"}, or
    {cmd:"Jab} [{it:{help colrspace##JMh:coefs}}]{cmd:"}
    (lowercase spelling allowed). The default is {cmd:"RGB"}. This default can
    also be selected by typing {cmd:""}. If {it:from} is {cmd:"HEX"}, {it:C0} is
    a string vector containing {it:n} hex colors. In all other cases, {it:C0} is a
    {it:n} x {it:c} real matrix of {it:n} color values in the respective
    coding scheme. See the diagram in {help colrspace##cspace:Color spaces}
    for the paths along which the colors will be translated.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata "mata: RGB = (25, 70, 120) \ (150, 60, 60)"}
        . {stata `"mata: S.convert(RGB, "RGB", "xyY")"'}
        . {stata `"mata: S.convert(RGB, "RGB", "JMh")"'}
        . {stata `"mata: Jab = S.convert(RGB, "RGB", "Jab")"'}
        . {stata `"mata: S.convert(Jab, "Jab", "HSV")"'}
        . {stata `"mata: HCL = S.convert(Jab, "Jab", "HCL")"'}
        . {stata `"mata: S.convert(HCL, "HCL", "RGB")"'}


{pstd}
    {it:S}{cmd:.convert()} can also be used for grayscale conversion or
    color vision deficiency simulation. The syntax is

        {it:C} = {it:S}{cmd:.convert(}{it:C0}{cmd:,} {it:from}{cmd:,} {cmd:"gray"}[{cmd:,} {it:p}{cmd:,} {it:method}]{cmd:)}

        {it:C} = {it:S}{cmd:.convert(}{it:C0}{cmd:,} {it:from}{cmd:,} {cmd:"cvd"}[{cmd:,} {it:p}{cmd:,} {it:method}]{cmd:)}

{pstd}
    where {it:p} is a real scalar in [0,1] specifying the proportion of gray
    or the severity of color vision deficiency. The default
    is {it:p} = {cmd:1} (complete conversion to gray, maximum CVD severity). This default can also be selected
    by typing {cmd:.} (missing). {it:method} selects the conversion method or
    CVD type; see {help colrspace##gray:Grayscale conversion} and
    {help colrspace##cvd:Color vision deficiency simulation}.

{marker cvalid}{...}
{dlgtab:Check validity of color specification}

{pstd}
    To check whether a color specification is valid you can type

        {it:color} = {it:S}{cmd:.cvalid(}{it:colorspec}{cmd:)}

{pstd}
    where {it:colorspec} is a single color specification as described
    for {help colrspace##string:{it:S}{bf:.colors()}}. If
    {it:colorspec} is valid, {it:color} will be set to the (expanded) name of
    the color, or the RGB code of the color if no color name is available. If
    {it:colorspec} is invalid, {it:color} will be set to empty string.

{marker namedcolors}{...}
{dlgtab:Obtain list of named colors}

{pstd}
    To obtain a list of named colors provided by {cmd:ColrSpace} (excluding Stata's system colors), type

        {it:list} = {it:S}{cmd:.namedcolors(}[{it:pattern}{cmd:,} {it:case}]{cmd:)}

{pstd}
    {it:list} will be a {it:n} x 2 string matrix
    with color names in the first column and hex codes in the second column. Specify
    {it:pattern} to filter the list; only color names matching the specified pattern
    will be listed in this case. The syntax for {it:pattern} is as explained in
    {helpb mf_strmatch:[M-5] strmatch()}. By default, case will be ignored; specify
    {it:case}!=0 for case-sensitive filtering.

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata `"mata: S.namedcolors("*lime*")"'}

        . {stata `"mata: S.Colors(S.namedcolors("*lime*")[,1])"'}
        . {stata "colorpalette mata(S)"}

{marker pexists}{...}
{dlgtab:Check whether palette exists}

{pstd}
    To check whether {it:name} matches an existing palette you can type

        {it:name} = {it:S}{cmd:.pexists(}{it:name}[{cmd:,} {it:libname}]{cmd:)}

{pstd}
    {it:name} will be set to the (expanded) name of the palette if a matching
    palette was found. If no matching palette is found, {it:name} will be set to
    empty string. See {help colrspace##palette:{it:S}{bf:.palette()}} for information
    on palettes.

{pstd}
    {it:libname} will be replaced by the name of the {cmd:ColrSpace} library
    in which the palette was found. If no matching palette is found,
    {it:libname} will be left unchanged.

{marker palettes}{...}
{dlgtab:Obtain list of available palettes}

{pstd}
    To obtain a list of available palettes, type

        {it:list} = {it:S}{cmd:.palettes(}[{it:pattern}{cmd:,} {it:case}]{cmd:)}

{pstd}
    {it:list} will be a {it:n} x 2 string matrix
    with palette names in the first column and library names in the second
    column. The library names provide information on the {cmd:ColrSpace} library
    in which a palette definition can be found. Specify
    {it:pattern} to filter the list; only palettes matching the specified pattern
    will be listed in this case. The syntax for {it:pattern} is as explained in
    {helpb mf_strmatch:[M-5] strmatch()}. By default, case will be ignored; specify
    {it:case}!=0 for case-sensitive filtering.

{marker colipolate}{...}
{dlgtab:Interpolation}

{pstd}
    In addition to {help colrspace##ipolate:{it:S}{bf:.ipolate()}}, {cmd:ColrSpace}
    also provides interpolation functions that do not involve translation
    between colorspaces and do not store any colors in {it:S}. These direct
    interpolation functions are

        {it:C} = {it:S}{cmd:.colipolate(}{it:C0}, {it:n}[{cmd:,} {it:range}{cmd:,} {it:power}{cmd:,} {it:positions}{cmd:,} {it:padded}]{cmd:)}

{pstd}
    for regular interpolation and

        {it:C} = {it:S}{cmd:.colipolate_c(}{it:C0}, {it:n}{cmd:)}

{pstd}
    for circular interpolation, where {it:C0} is an {it:n0} x {it:c} matrix of
    {it:n0} origin colors that are interpolated to {it:n} destination
    colors. Other arguments are as for
    {help colrspace##ipolate:{it:S}{bf:.ipolate()}}.

{marker colrecycl}{...}
{dlgtab:Recycling}

{pstd}
    In addition to {help colrspace##recycle:{it:S}{bf:.recycle()}}, {cmd:ColrSpace}
    also provides a recycling function that
    does not store any colors in {it:S}. This direct recycling function is

        {it:C} = {it:S}{cmd:.colrecycle(}{it:C0}, {it:n}{cmd:)}

{pstd}
    where {it:C0} is an {it:n0} x {it:c} matrix of {it:n0} input colors values
    that are recycled to {it:n} output colors.

{marker lsmap}{...}
{dlgtab:Linear segmented colormaps}

{pstd}
    Function

        {it:RGB1} = {it:S}{cmd:.lsmap(}{it:R}{cmd:,} {it:G}{cmd:,} {it:B}{cmd:,} {it:n}[{cmd:,} {it:range}]{cmd:)}

{pstd}
    can be used to create linear segmented colormaps. Some of the
    {help colrspace##colormaps:colormaps} above
    are implemented in terms of this function. {it:R}, {it:G}, and {it:B} are matrices
    specifying the anchor points of the segments (each row consist of three values: the
    anchor, the value of the color on the left of the anchor, and the value of the
    color on the right). See the corresponding
    {browse "http://matplotlib.org/tutorials/colors/colormap-manipulation.html#creating-linear-segmented-colormaps":tutorial page}
    at {browse "http://matplotlib.org/":matplotlib.org} for details. {it:S}{cmd:.lsmap()}
    does not check the consistency of the specified matrices and may return invalid
    results if consistency is violated.

{marker clip}{...}
{dlgtab:Clipping}

{pstd}
    Function

        {it:C} = {it:S}{cmd:.clip(}{it:C0}{cmd:,} {it:a}{cmd:,} {it:b})

{pstd}
    can be used for clipping, where {it:C0} is a real matrix of input values, {it:a} is a real scalar
    specifying the lower bound, and {it:b} is a real scalar specifying the upper
    bound. Values in {it:C0} smaller than {it:a} will be set to {it:a}; values larger
    than {it:b} will be set to {it:b}; values between {it:a} and {it:b} as well as
    missing values will be left as is.


{marker settings}{...}
{title:Overview of color space settings}

{pstd}
    To display an overview of the current color space settings of {it:S}, type

        {it:S}{cmd:.settings}{cmd:()}

{pstd}
    Example:

        . {stata "mata: S = ColrSpace()"}
        . {stata "mata: S.settings()"}

{pstd}
    To restore the {help colrspace##init:default color space settings}, type

        {it:S}{cmd:.clearsettings}{cmd:()}


{marker rgbspace}{...}
{title:Setting the RGB working space}

{pstd}
    To set the RGB working space, type

        {it:S}{cmd:.rgbspace("}{it:name}{cmd:")}

{pstd}
    where {it:name} is one of the following:

{p2colset 9 26 28 2}{...}
{p2col:{cmd:Adobe 1998}}Adobe RGB (1998){p_end}
{p2col:{cmd:Apple}}Apple RGB{p_end}
{p2col:{cmd:Best}}Best RGB{p_end}
{p2col:{cmd:Beta}}Beta RGB{p_end}
{p2col:{cmd:Bruce}}Bruce RGB{p_end}
{p2col:{cmd:CIE}}CIE 1931 RGB{p_end}
{p2col:{cmd:ColorMatch}}ColorMatch RGB{p_end}
{p2col:{cmd:Don 4}}Don RGB 4{p_end}
{p2col:{cmd:ECI v2}}ECI RGB v2{p_end}
{p2col:{cmd:Ekta PS5}}Ekta Space PS5{p_end}
{p2col:{cmd:Generic}}Generic RGB{p_end}
{p2col:{cmd:HDTV}}HDTV (HD-CIF){p_end}
{p2col:{cmd:NTSC}}NTSC RGB (1953){p_end}
{p2col:{cmd:PAL/SECAM}}PAL/SECAM RGB{p_end}
{p2col:{cmd:ProPhoto}}ProPhoto RGB{p_end}
{p2col:{cmd:SGI}}SGI RGB{p_end}
{p2col:{cmd:SMPTE-240M}}SMPTE-240M RGB{p_end}
{p2col:{cmd:SMPTE-C}}SMPTE-C RGB{p_end}
{p2col:{cmd:sRGB}}Standard RGB using primaries from {browse "http://www.brucelindbloom.com/WorkingSpaceInfo.html":Lindbloom (2017d)}{p_end}
{p2col:{cmd:sRGB2}}Standard RGB using equation F.8 (XYZ to RGB matrix) from {browse "http://www.sis.se/api/document/preview/562720/":IEC (2003)}{p_end}
{p2col:{cmd:sRGB3}}Standard RGB using equation F.7 (RGB to XYZ matrix) from {browse "http://www.sis.se/api/document/preview/562720/":IEC (2003)}{p_end}
{p2col:{cmd:Wide Gamut}}Adobe Wide Gamut RGB{p_end}
{p2col:{cmd:Wide Gamut BL}}Wide Gamut variant from {browse "http://www.brucelindbloom.com/WorkingSpaceInfo.html":Lindbloom (2017d)}{p_end}

{pstd}
    The names can be abbreviated and typed in lowercase letters. If
    abbreviation is ambiguous, the first matching name in the alphabetically
    ordered list will be used. See the
    {help colrspace_source##rgbspaces:{bf:ColrSpace} source code} for the definitions of the
    spaces. The definitions have been taken from
    {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf":Pascale (2003)} and
    {browse "http://www.brucelindbloom.com/WorkingSpaceInfo.html":Lindbloom (2017d)}. Also
    see {browse "http://en.wikipedia.org/wiki/RGB_color_space":Wikipedia (2018g)}. The
    default is {it:S}{cmd:.rgbspace("sRGB")}. This default can also be selected
    by typing {it:S}{cmd:.rgbspace("")}. Other color management systems
    may use slightly different definition of standard RGB. For example, the
    {cmd:colorspacious} Python library by Smith (2018) uses a definition equivalent to
    {cmd:"sRGB2"}. The advantage of {cmd:"sRGB"} is that RGB white (255, 255, 255)
    translates to the reference white in XYZ, which is not exactly true for
    {cmd:"sRGB2"} or {cmd:"sRGB3"}.

{marker gamma}{...}
{pstd}
    An RGB working space consists of three elements: the parameters of the
    gamma compression used to transform lRGB (linear RGB) to RGB, the reference
    white, and the working space primaries used to transform XYZ to lRGB. Instead
    of choosing a named RGB working space, the elements can also be set
    directly. To set the gamma compression parameters, type

        {it:S}{cmd:.rgb_gamma(}{it:args}{cmd:)}

{pstd}
    where {it:args} is

            {it:gamma}
        or  {it:gamma}{cmd:,} {it:offset}{cmd:,} {it:transition}{cmd:,} {it:slope}
        or  {cmd:(}{it:gamma}{cmd:,} {it:offset}{cmd:,} {it:transition}{cmd:,} {it:slope}{cmd:)}
        or  {cmd:"}{it:gamma}{cmd:"}
        or  {cmd:"}{it:gamma} {it:offset} {it:transition} {it:slope}{cmd:"}

{pstd}
    If only {it:gamma} is provided, simple gamma encoding C' = C^(1/{it:gamma})
    is applied. If {it:offset}, {it:transition}, and {it:slope} are also
    provided, the detailed gamma encoding C' = (1 + {it:offset}) *
    C^(1/{it:gamma}) - {it:offset} if C > {it:transition} and else C' = C *
    {it:slope} is used. A typical value for {it:gamma} is 2.2; see
    {browse "http://blog.johnnovak.net/2016/09/21/what-every-coder-should-know-about-gamma/":Novak (2016)}
    for an excellent explanation of gamma compression. Likewise,
    the reference white can be set by

        {it:S}{cmd:.rgb_white(}{it:args}{cmd:)}

{pstd}
    where {it:args} is as described in
    {help colrspace##xyzwhite:Setting the XYZ reference white}. If the
    reference white of the RGB working space differs from the XYZ reference
    white, {cmd:ColrSpace} applies {help colrspace##chadapt:chromatic adaption} when translating
    between XYZ and lRGB. Furthermore, to set the working space primaries, type

        {it:S}{cmd:.rgb_xy(}{it:xy}{cmd:)}

{pstd}
    where {it:xy} is a 3 x 2 matrix containing the red, green, and blue xy
    primaries. {cmd:ColrSpace} uses the method described in
    {browse "http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html":Lindbloom (2017e)} to
    compute the lRGB-to-XYZ transformation matrix from the white point and the
    primaries, and sets the XYZ-to-lRGB matrix to the inverse of the lRGB-to-XYZ
    matrix. Alternatively, you can type

        {it:S}{cmd:.rgb_M(}{it:M}{cmd:)}

{pstd}
    where {it:M} is a 3 x 3 matrix, to directly set the lRGB-to-XYZ matrix to {it:M} and
    the XYZ-to-lRGB matrix to {help mf_luinv:{bf:luinv(}{it:M}{bf:)}}, or

        {it:S}{cmd:.rgb_invM(}{it:invM}{cmd:)}

{pstd}
    to set the XYZ-to-lRGB matrix to {it:invM} and the lRGB-to-XYZ matrix to
    {help mf_luinv:{bf:luinv(}{it:invM}{bf:)}}. To retrieve the current settings, you can type

        {it:gamma} = {it:S}{cmd:.rgb_gamma()}
        {it:white} = {it:S}{cmd:.rgb_white()}
           {it:xy} = {it:S}{cmd:.rgb_xy()}
            {it:M} = {it:S}{cmd:.rgb_M()}
         {it:invM} = {it:S}{cmd:.rgb_invM()}


{marker xyzwhite}{...}
{title:Setting the XYZ reference white}

{pstd}
    To set the reference white for the CIE XYZ color space, type

        {it:S}{cmd:.xyzwhite(}{it:args}{cmd:)}

{pstd}
    where {it:args} is

            {it:X}{cmd:,} {it:Y}{cmd:,} {it:Z}
        or  {cmd:(}{it:X}{cmd:,} {it:Y}{cmd:,} {it:Z}{cmd:)}
        or  {cmd:"}{it:X} {it:Y} {it:Z}{cmd:"}
        or  {it:x}, {it:y}
        or  {cmd:(}{it:x}, {it:y}{cmd:)}
        or  {cmd:"}{it:x} {it:y}{cmd:"}
        or  {cmd:"}{it:name}{cmd:"}

{pstd}
    where {it:X}, {it:Y}, and {it:Z} are the XYZ coordinates of the
    white point (with {it:Y} = 100), {it:x} and {it:y} are the xyY coordinates of
    the white point (assuming {it:Y} = 100), and {it:name}
    is one of the following:

{p2colset 9 38 40 2}{...}
{p2col:CIE 1931 2°{space 2}CIE 1964 10°}Description{p_end}
{p2col:observer{space 5}observer}{p_end}
{p2col:{cmd:A}{space 12}{cmd:A 10 degree}}Incandescent/Tungsten 2856K{p_end}
{p2col:{cmd:B}{space 12}{cmd:B 10 degree}}Direct sunlight at noon 4874K (obsolete){p_end}
{p2col:{cmd:B BL}}B 2 degree variant from {browse "http://www.brucelindbloom.com/Eqn_ChromAdapt.html":Lindbloom (2017a)}{p_end}
{p2col:{cmd:C}{space 12}{cmd:C 10 degree}}North sky daylight 6774K (obsolete){p_end}
{p2col:{cmd:D50}{space 10}{cmd:D50 10 degree}}Horizon light 5003K (used for color rendering){p_end}
{p2col:{cmd:D55}{space 10}{cmd:D55 10 degree}}Mid-morning/mid-afternoon daylight 5503K (used for photography){p_end}
{p2col:{cmd:D65}{space 10}{cmd:D65 10 degree}}Noon daylight 6504K (new version of north sky daylight){p_end}
{p2col:{cmd:D75}{space 10}{cmd:D75 10 degree}}North sky daylight 7504K{p_end}
{p2col:{cmd:9300K}}High eff. blue phosphor monitors 9300K{p_end}
{p2col:{cmd:E}}Uniform energy illuminant 5454K{p_end}
{p2col:{cmd:F1}{space 11}{cmd:F1 10 degree}}Daylight fluorescent 6430K{p_end}
{p2col:{cmd:F2}{space 11}{cmd:F2 10 degree}}Cool white fluorescent 4200K{p_end}
{p2col:{cmd:F3}{space 11}{cmd:F3 10 degree}}White fluorescent 3450K{p_end}
{p2col:{cmd:F4}{space 11}{cmd:F4 10 degree}}Warm white fluorescent 2940K{p_end}
{p2col:{cmd:F5}{space 11}{cmd:F5 10 degree}}Daylight fluorescent 6350K{p_end}
{p2col:{cmd:F6}{space 11}{cmd:F6 10 degree}}Lite white fluorescent 4150K {p_end}
{p2col:{cmd:F7}{space 11}{cmd:F7 10 degree}}Broad-band daylight fluorescent, 6500K {p_end}
{p2col:{cmd:F8}{space 11}{cmd:F8 10 degree}}D50 simulator, Sylvania F40 design 50, 5000K{p_end}
{p2col:{cmd:F9}{space 11}{cmd:F9 10 degree}}Cool white deluxe fluorescent 4150K{p_end}
{p2col:{cmd:F10}{space 10}{cmd:F10 10 degree}}Philips TL85, Ultralume 50, 5000K{p_end}
{p2col:{cmd:F11}{space 10}{cmd:F11 10 degree}}Narrow-band white fluorescen, Philips TL84, Ultralume 40, 4000K{p_end}
{p2col:{cmd:F12}{space 10}{cmd:F12 10 degree}}Philips TL83, Ultralume 30, 3000K{p_end}

{pstd}
    The names can be abbreviated and typed in lowercase letters (for example,
    {cmd:"D55 10 degree"} could be typed as {cmd:"d55 10"}). If
    abbreviation is ambiguous, the first matching name in the alphabetically
    ordered list will be used. See the
    {help colrspace_source##illuminants:{bf:ColrSpace} source code} for the definitions of the
    white points. The definitions have been taken from
    {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf":Pascale (2003)},
    {browse "http://www.brucelindbloom.com/Eqn_ChromAdapt.html":Lindbloom (2017a)}, and
    {browse "http://en.wikipedia.org/wiki/Standard_illuminant":Wikipedia (2018h)}. The
    default is {it:S}{cmd:.xyzwhite("D65")}. This default can also be selected
    by typing {it:S}{cmd:.xyzwhite(.)} or {it:S}{cmd:.xyzwhite("")}. To retrieve a
    1 x 3 rowvector containing the XYZ coordinates of the current white point, you
    can type

        {it:white} = {it:S}{cmd:.xyzwhite()}


{marker viewcond}{...}
{title:Setting the CIECAM02 viewing conditions}

{pstd}
    To set the CIECAM02 viewing conditions, type

        {it:S}{cmd:.viewcond(}{it:args}{cmd:)}

{pstd}
    where {it:args} is

            {it:Y_b}{cmd:,} {it:L_A}{cmd:,} {it:F}{cmd:,} {it:c}{cmd:,} {it:N_c}
        or  {it:Y_b}{cmd:,} {it:L_A}{cmd:,} {cmd:(}{it:F}{cmd:,} {it:c}{cmd:,} {it:N_c}{cmd:)}
        or  {cmd:(}{it:Y_b}{cmd:,} {it:L_A}{cmd:,} {it:F}{cmd:,} {it:c}{cmd:,} {it:N_c}{cmd:)}
        or  {cmd:"}{it:Y_b} {it:L_A} {it:F} {it:c} {it:N_c}{cmd:"}
        or  {it:Y_b}{cmd:,} {it:L_A}{cmd:,} {cmd:"}{it:surround}{cmd:"}
        or  {cmd:"}{it:Y_b} {it:L_A} {it:surround}{cmd:"}

{pstd}
    with {it:surround} equal to {cmd:average} ({it:F} = 1, {it:c} = .69, {it:N_c} = 1),
    {cmd:dim} ({it:F} = .9, {it:c} = .59, {it:N_c} = .9),
    or {cmd:dark} ({it:F} = .8, {it:c} = .525, {it:N_c} = .8) (abbreviations allowed). The default is
    {it:Y_b} = 20, {it:L_A} = 64/(5*pi()), and average surround. These defaults can also be selected by
    typing {it:S}{cmd:.viewcond(.)} or {it:S}{cmd:.viewcond("")}, or by setting {it:Y_b} to {cmd:.},
    {it:L_A} to {cmd:.}, and {it:surround} to {cmd:.} or empty string. To
    retrieve a 1 x 5 rowvector of the current viewing condition parameters, type

        {it:viewcond} = {it:S}{cmd:.viewcond()}

{pstd}
    See {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013)}
    for details on CIECAM02 viewing conditions.


{marker ucscoefs}{...}
{title:Setting the default coefficients for J'M'h and J'a'b'}

{pstd}
    To set the default uniform color space coefficients for J'M'h and J'a'b', type

        {it:S}{cmd:.ucscoefs(}{it:args}{cmd:)}

{pstd}
    where {it:args} is

            {it:K_L}{cmd:,} {it:c_1}{cmd:,} {it:c_2}
        or  {cmd:(}{it:K_L}{cmd:,} {it:c_1}{cmd:,} {it:c_2}{cmd:)}
        or  {cmd:"}{it:K_L} {it:c_1} {it:c_2}{cmd:"}
        or  {cmd:"}{it:name}{cmd:"}

{pstd}
    with {it:name} equal to {cmd:UCS} ({it:K_L} = 1, {it:c_1} = .007, {it:c_2} = .0228),
    {cmd:LCD} ({it:K_L} = .77, {it:c_1} = .007, {it:c_2} = .0053),
    or {cmd:SCD} ({it:K_L} = 1.24, {it:c_1} = .007, {it:c_2} = .0363)
    (abbreviations and lowercase letters allowed). To to retrieve a 1 x 3 rowvector
    of the current default coefficients, type

        {it:ucscoefs} = {it:S}{cmd:.ucscoefs()}

{pstd}
    See {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013, chapter 2.6.1)}
    and {browse "http://doi.org/10.1002/col.20227":Luo et al. (2006)} for details on these coefficients.


{marker chadapt}{...}
{title:Setting the chromatic adaption method}

{pstd}
    To set the chromatic adaption method type

        {it:S}{cmd:.chadapt(}{it:method}{cmd:)}

{pstd}
    where {it:method} is {cmd:"Bfd"} (Bradford), {cmd:"identity"} (XYZ Scaling),
    {cmd:"vKries"} (Von Kries), or {cmd:"CAT02"} (abbreviations and lowercase
    letters allowed). The default is {it:S}{cmd:.chadapt("Bfd")}, which can also be selected
    by typing {it:S}{cmd:.chadapt("")}. The Bradford, XYZ Scaling, and Von Kries methods use the
    procedure described in
    {browse "http://www.brucelindbloom.com/Eqn_ChromAdapt.html":Lindbloom (2017a)},
    the {cmd:"CAT02"} method uses the procedure described in
    {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013)}
    (page 33). To retrieve a string scalar containing the current method, type

        {it:method} = {it:S}{cmd:.chadapt()}

{pstd}
    {cmd:ColrSpace} uses chromatic adaption internally whenever such a
    translation is necessary. However, you can also apply chromatic adaption
    manually by typing

        {it:XYZnew} = {it:S}{cmd:.XYZ_to_XYZ(}{it:XYZ}{cmd:,} {it:from}{cmd:,} {it:to}{cmd:)}

{pstd}
    where {it:XYZ} is an {it:n} x 3 matrix of XYZ values to be adapted, {it:from} is the
    origin whitepoint, and {it:to} is the destination whitepoint; any single-argument whitepoint
    specification as described in {help colrspace##xyzwhite:Setting the XYZ reference white}
    is allowed. Function {it:S}{cmd:.XYZ_to_XYZ()} does not change or store any colors in {it:S}.

{pstd}
    To retrieve the predefined transformation matrices on which chromatic adaption is based,
    type

        {it:M} = {it:S}{cmd:.tmatrix(}[{it:name}]{cmd:)}

{pstd}
    where {it:name} is {cmd:"Bfd"}, {cmd:"identity"},
    {cmd:"vKries"}, {cmd:"CAT02"}, or {cmd:"HPE"} (Hunt-Pointer-Estevez)
    (abbreviations and lowercase letters allowed). The default is {it:S}{cmd:.tmatrix("Bfd")},
    which can also be selected by typing {it:S}{cmd:.tmatrix("")}. The
    {cmd:"HPE"} matrix is not used for chromatic adaption but has been included in
    {it:S}{cmd:.tmatrix()} for convenience. It is used when translating colors
    from XYZ to CIECAM02; see {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":Luo and Li (2013)}.


{marker src}{...}
{title:Source code and certification script}

{pstd}
    {cmd:lcolrspace.mlib} has been compiled in Stata 14.2. The source code can be
    found in file {help colrspace_source:colrspace_source.sthlp}. Palette definitions,
    parameters of color generators, and definitions of named colors are kept in
    additional source files. These files are
    {help colrspace_library_palettes:colrspace_library_palettes.sthlp},
    {help colrspace_library_lsmaps:colrspace_library_lsmaps.sthlp},
    {help colrspace_library_rgbmaps:colrspace_library_rgbmaps.sthlp},
    {help colrspace_library_generators:colrspace_library_generators.sthlp}, and
    {help colrspace_library_namedcolors:colrspace_library_namedcolors.sthlp}.

{pstd}
    You can extend the set of available palettes and colors by providing
    personal library files. These files should be stored somewhere along the
    {helpb adopath} (for example in the {cmd:PERSONAL} directory), so Stata can
    find them, and they must be named as above but with a "_personal"
    suffix (e.g. "colrspace_library_palettes_personal.sthlp"). Each library file
    has its peculiar syntax; see the explanations in the file headers.

{pstd}
    A certification script testing internal consistency and comparing results to some
    test values and results from the {cmd:colorspacious} Python library by
    Smith (2018) (see file
    {browse "http://github.com/njsmith/colorspacious/blob/master/colorspacious/gold_values.py":gold_values.py}
    at Github) as well as to results obtained from the color calculators at
    {browse "http://colorizer.org/":colorizer.org} and
    {browse "http://www.brucelindbloom.com/index.html?ColorCalculator.html":www.brucelindbloom.com},
    can be found at
    {browse "http://fmwww.bc.edu/repec/bocode/c/colrspace_cscript.do"}.


{marker ref}{...}
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
    Brewer, C.A., G.W. Hatchard, M.A. Harrower. 2003. {browse "http://doi.org/10.1559/152304003100010929":ColorBrewer in Print: A Catalog of Color Schemes for Maps}.
    Cartography and Geographic Information Science 30(1): 5–32.
    {p_end}
{phang}
    Brewer, C.A. 2016. Designing Better Maps. A Guide for GIS Users. 2nd ed. Redlands, CA: Esri Press.
    {p_end}
{phang}
    Briatte, F. 2013. SCHEME-BURD: Stata module to provide a
    ColorBrewer-inspired graphics scheme with qualitative and blue-to-red
    diverging colors. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s457623.html"}.
    {p_end}
{phang}
    Crameri, F. 2018. Scientific colour maps. Zenodo. {browse "http://doi.org/10.5281/zenodo.1243862":DOI: 10.5281/zenodo.1243862}.
    {p_end}
{phang}
    Hunt, R.W.G. 2004. {browse "http://doi.org/10.1002/0470024275":The Reproduction of Colour}. 6th ed. John Wiley & Sons.
    {p_end}
{phang}
    Hunter, J.D. 2007. {browse "http://dx.doi.org/10.1109/MCSE.2007.55":Matplotlib: A 2D graphics environment}. Computing
    in Science & Engineering 9(3): 90-95.
    {p_end}
{phang}
    Ihaka, R., P. Murrell, K. Hornik, J.C. Fisher, R. Stauffer, A. Zeileis.
    2016. colorspace: Color Space Manipulation. R package version 1.3-2.
    Available from {browse "http://CRAN.R-project.org/package=colorspace"}.
    {p_end}
{phang}
    International Electrotechnical Commission (IEC). 2003. International
    Standard IEC 61966-2-1:1999/AMD1:2003. Amendment 1 – Multimedia systems and
    equipment – Color measurement and management – Part 2-1: Color management –
    Default RGB color space - sRGB. Available from
    {browse "http://www.sis.se/api/document/preview/562720/"}.
    {p_end}
{phang}
    Juul, S. 2003. {browse "http://www.stata-journal.com/article.html?article=gr0002":Lean mainstream schemes for Stata 8 graphics}. The Stata
    Journal 3(3): 295-301.
    {p_end}
{phang}
    Kovesi, P. 2015. Good Colour Maps: How to Design Them. {browse "http://arxiv.org/abs/1509.03700":arXiv:1509.03700} [cs.GR].
    {p_end}
{phang}
    Lin, S., J. Fortuna, C. Kulkarni, M. Stone,
    J. Heer. 2013. {browse "http://dx.doi.org/10.1111/cgf.12127":Selecting Semantically-Resonant Colors for Data Visualization}. Computer
    Graphics Forum 32(3pt4): 401-410.
    {p_end}
{phang}
    Lindbloom, B.J. 2017a. Chromatic Adaptation. Revision 06 Apr 2017. Available from
    {browse "http://www.brucelindbloom.com/Eqn_ChromAdapt.html"}.
    {p_end}
{phang}
    Lindbloom, B.J. 2017b. Delta E (CIE 1994). Revision 07 Apr 2017. Available from
    {browse "http://www.brucelindbloom.com/Eqn_DeltaE_CIE94.html"}.
    {p_end}
{phang}
    Lindbloom, B.J. 2017c. Delta E (CIE 2000). Revision 08 Apr 2017. Available from
    {browse "http://www.brucelindbloom.com/Eqn_DeltaE_CIE2000.html"}.
    {p_end}
{phang}
    Lindbloom, B.J. 2017d. RGB Working Space Information. Revision 06 Apr 2017. Available from
    {browse "http://www.brucelindbloom.com/WorkingSpaceInfo.html"}.
    {p_end}
{phang}
    Lindbloom, B.J. 2017e. RGB/XYZ Matrices. Revision 07 Apr 2017. Available from
    {browse "http://www.brucelindbloom.com/Eqn_RGB_XYZ_Matrix.html"}.
    {p_end}
{phang}
    Luo, R.M., G. Cui, C. Li. 2006.
    {browse "http://doi.org/10.1002/col.20227":Uniform Colour Spaces Based on CIECAM02 Colour Appearance Model}. COLOR
    research and application 31(4): 320–330.
    {p_end}
{phang}
    Luo, M.R., C. Li. 2013. {browse "http://doi.org/10.1007/978-1-4419-6190-7_2":CIECAM02 and Its Recent Developments}. P. 19-58
    in: C. Fernandez-Maloigne (ed.). Advanced Color Image Processing and Analysis. New
    York: Springer.
    {p_end}
{phang}
    Machado, G.M., M.M. Oliveira, L.A.F. Fernandes. 2009.
    {browse "http://doi.org/10.1109/TVCG.2009.113":A Physiologically-based Model for Simulation of Color Vision Deficiency}. IEEE
    Transactions on Visualization and Computer Graphics 15(6): 1291-1298.
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
    Novak, J. 2016. What every coder should know about gamma. 2016 Sep 21. Available from
    {browse "http://blog.johnnovak.net/2016/09/21/what-every-coder-should-know-about-gamma/"}.
    {p_end}
{phang}
    Okabe, M., K. Ito. 2002. Color Universal Design (CUD). How to make figures and presentations that
    are friendly to Colorblind people. Available from
    {browse "http://jfly.iam.u-tokyo.ac.jp/color/"}.
    {p_end}
{phang}
    Pascale, D. 2003. A review of RGB color spaces ... from xyY to
    R'G'B'. Montreal: The BabelColor Company. Available from
    {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf"}.
    {p_end}
{phang}
    Pisati, M. 2007. SPMAP: Stata module to visualize spatial data. Available
    from {browse "http://ideas.repec.org/c/boc/bocode/s456812.html"}.
    {p_end}
{phang}
    SFSO (Swiss Federal Statistical Office). 2017. Layoutrichtlinien. Gestaltungs und
    Redaktionsrichtlinien für Publikationen, Tabellen und grafische
    Assets. Version 1.1.1. Neuchâtel: Bundesamt f{c u:}r Statistik.
    {p_end}
{phang}
    Smith, N.J. 2018. colorspacious 1.1.2: A powerful, accurate, and easy-to-use
    Python library for doing colorspace conversions. Available from
    {browse "http://pypi.org/project/colorspacious"} (DOI
    {browse "http://doi.org/10.5281/zenodo.1214904":10.5281/zenodo.1214904}).
    {p_end}
{phang}
    Tol, P. 2012. Colour Schemes. SRON Technical Note, Doc. no. SRON/EPS/TN/09-002. Available
    from {browse "http://personal.sron.nl/~pault/colourschemes.pdf"}.
    {p_end}
{phang}
    Wikipedia. 2018a. CIE 1931 color space. Revision 22 October 2018. Available from
    {browse "http://en.wikipedia.org/wiki/CIE_1931_color_space"}.
    {p_end}
{phang}
    Wikipedia. 2018b. CIELAB color space. Revision 28 November 2018. Available from
    {browse "http://en.wikipedia.org/wiki/CIELAB_color_space"}.
    {p_end}
{phang}
    Wikipedia. 2018c. CIELUV. Revision 27 August 2018. Available from
    {browse "http://en.wikipedia.org/wiki/CIELUV"}.
    {p_end}
{phang}
    Wikipedia. 2018d. HSL and HSV. Revision 6 November 2018. Available from
    {browse "http://en.wikipedia.org/wiki/HSL_and_HSV"}.
    {p_end}
{phang}
    Wikipedia. 2018e. Mean of circular quantities. Revision 23 November 2018. Available from
    {browse "http://en.wikipedia.org/wiki/Mean_of_circular_quantities"}.
    {p_end}
{phang}
    Wikipedia. 2018f. RGB color model. Revision 22 October 2018. Available from
    {browse "http://en.wikipedia.org/wiki/RGB_color_model"}.
    {p_end}
{phang}
    Wikipedia. 2018g. RGB color space. Revision 8 June 2018. Available from
    {browse "http://en.wikipedia.org/wiki/RGB_color_space"}.
    {p_end}
{phang}
    Wikipedia. 2018h. Standard illuminant. Revision 18 July 2018. Available from
    {browse "http://en.wikipedia.org/wiki/Standard_illuminant"}.
    {p_end}
{phang}
    Wikipedia. 2019a. Color blindness. Revision 7 January 2019. Available from
    {browse "http://en.wikipedia.org/wiki/Color_blindness"}.
    {p_end}
{phang}
    Wikipedia. 2019b. Color difference. Revision 9 January 2019. Available from
    {browse "http://en.wikipedia.org/wiki/Color_difference"}.
    {p_end}
{phang}
    Wikipedia. 2019c. Web colors. Revision 6 January 2019. Available from
    {browse "http://en.wikipedia.org/wiki/Web_colors"}.
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
    Jann, B. 2022. ColrSpace: A Mata class for color management. University
    of Bern Social Sciences Working Papers No. 42. Available from
    {browse "http://ideas.repec.org/p/bss/wpaper/42.html"}.
    {p_end}
{phang}
    Jann, B. 2019. colrspace: Stata module providing a class-based color
    management system in Mata. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458597.html"}.
    {p_end}


{marker alsosee}{...}
{title:Also see}

{psee}
    Online:  help for {helpb colorpalette} (if installed), {help colorstyle}

