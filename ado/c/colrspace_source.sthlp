*! version 1.2.2  10dec2025  Ben Jann
* {smcl}
* {title:lcolrspace.mlib source code}
*
* {help colrspace_source##class:Class and struct definitions}
* {help colrspace_source##new:Constructor}
* {help colrspace_source##util:Utilities and common functions}
* {help colrspace_source##illuminants:Illuminants}
* {help colrspace_source##rgbspaces:RGB working spaces}
* {help colrspace_source##viewcond:CIECAM02 viewing conditions}
* {help colrspace_source##chadapt:Chromatic adaption}
* {help colrspace_source##string:String input/output}
* {help colrspace_source##set:Set or retrieve colors}
* {help colrspace_source##opacity:Set or retrieve opacity and intensity}
* {help colrspace_source##modify:Color modification}
* {help colrspace_source##order:- recycle, select, drop, order, shift}
* {help colrspace_source##ipolate:- interpolate, mix}
* {help colrspace_source##intens:- intensify, saturate, luminate}
* {help colrspace_source##gray:- grayscale conversion}
* {help colrspace_source##cvd:- color vision deficiency simulation}
* {help colrspace_source##diff:Color differences and contrast ratios}
* {help colrspace_source##convert:Translate colors between spaces}
* {help colrspace_source##translate:Elementary translators}
* {help colrspace_source##web:Named web colors}
* {help colrspace_source##palettes:Palettes}
* {help colrspace_source##gen:Color generators}
* 
* {bf:Setup (Stata locals)} {hline}
* {asis}

version 14.2
mata mata set matastrict on

// class & struct
local MAIN   ColrSpace
local Main   class `MAIN' scalar
local DATA   `MAIN'_DATA
local Data   struct `DATA' scalar
local CAM02  `MAIN'_CAM02
local Cam02  struct `CAM02' scalar
local DICT   AssociativeArray
local Dict   class `DICT' scalar
// real
local RS     real scalar
local RR     real rowvector
local RC     real colvector
local RV     real vector
local RM     real matrix
// counters
local Int    real scalar
local IntR   real rowvector
local IntC   real colvector
local IntV   real vector
local IntM   real matrix
// string
local SS     string scalar
local SR     string rowvector
local SC     string colvector
local SV     string vector
local SM     string matrix
// transmorphic
local T      transmorphic
local TS     transmorphic scalar
local TV     transmorphic vector
local TM     transmorphic matrix
// pointer
local PV     pointer vector
local PS     pointer scalar
local PData  pointer (`Data') scalar
local PDict  pointer (`Dict') scalar
// boolean
local Bool   real scalar
local BoolC  real colvector
local TRUE   1
local FALSE  0

* {smcl}
* {marker class}{bf:Class and struct definitions} {hline}
* {asis}

mata:

struct `DATA' {
    // meta data
    `SS'    name          // name of color collection
    `SS'    pclass        // class: qualitative/categorical, sequential, diverging
    `SS'    note          // description of color collection
    `SS'    source        // source of color collection
    // colors
    `Int'   n             // number of colors
    `RM'    RGB           // N x 3 matrix of RGB 0-1 codes
    `RC'    alpha         // N x 1 vectors of opacity values in [0,1]
    `RC'    intensity     // N x 1 vectors of intensity values in [0,1]
    `SC'    names         // N x 1 string vector of color names
    `SC'    info          // N x 1 string vector of color information
    `BoolC' stok          // N x 1 vector: Stata compatible "name"
    `Bool'  isip          // 1 if colors were interpolated; 0 else
}

struct `CAM02' {
    // viewing conditions
    `RS'    Yb         // background luminance factor
    `RS'    LA         // adapting field luminance
    `RS'    F, c, Nc   // surround conditions
    `RV'    KLc1c2     // default J'a'b' coefficients
    // various parameters/constants
    `Bool'  set        // already set?
    `RC'    D
    `RS'    FL, n, z, Ncb, Nbb, Aw
    `RM'    CAT02, iCAT02, HPE, iHPE, HUE
}

class `MAIN' {
    // initialization
    public:
        void    clear()         // remove data, set n of colors to 0
        void    clearindex()    // clear palettes and namedcolors index
        void    clearsettings() // set default color space settings
    private:
        void    new()         // initialize class with default settings
        `SC'    SPACES        // main list of supported color metrics
        `SC'    SPACES2       // additional color metrics
        `SM'    EDGELIST      // edge list for color conversions

    // Data containers
    private:
       `Data'   data          // main data container
       `Data'   data1         // temporary data container
      `PData'   S             // pointer to relevant data container
        `Int'   N1            // number of colors added last
        void    replacedata() // replace data by data1
        void    updatedata()  // replace last N1 colors in data by data1
        void    appenddata()  // append data1 to data
        void    removeadded() // remove last N1 colors from data
        void    copydata()    // copy data1 to data
        void    copyadded()   // copy last N1 colors from data to data1
    
    // Utilities
    public:
        void    describe()    // describe contents
        void    settings()    // describe settings
        `RS'    N()           // number of colors
        `RS'    N_added()     // number of added colors
         `T'    name()        // set or return name of palette
         `T'    pclass()      // set or return type of palette
         `T'    note()        // set or return palette description
         `T'    source()      // set or return palette source
      `Bool'    isipolate()   // 1 if interpolated; 0 else
        `RM'    colipolate()  // interpolate columns
        `RM'    colipolate_c() // interpolate columns (circular)
        `TM'    colrecycle()  // recycle columns
        `RM'    lsmap()       // create linear segmented colormap
        `RM'    clip()        // clip values
    private:
        `SS'    gettok()      // split first token from rest
      `Bool'    smatch(), _smatch() // check abbreviated string
        `SS'    findkey(), _findkey() // find key in list
        void    assert_cols() // assert number of columns
        void    assert_size() // assert number of columns and rows
        `RV'    _colipolate_setrange() // parse interpolation range argument
        void    _colipolate_fromto()   // create from-to interpolation mapping
        `RM'    _colipolate_pos()      // handle unordered from positions
        void    _colipolate_collapse() // collapse input in case of ties
        `RM'    _colipolate()          // apply interpolation
        `RC'    _lsmap()      // function used by lsmap()
        `RV'    _clip()       // clip vector
        `RS'    __clip()      // clip value
        `SR'    _tokens()     // modified version of tokens

    // XYZ reference white
    public:
         `T'    xyzwhite()    // set/retrieve XYZ whitepoint
    private:
      `Dict'    illuminants
        void    illuminants()
        `RR'    white
        `RR'    _white(), _white_set(), _white_get()

    // RGB working spaces
    public:
        void    rgbspace()    // set RGB working space
         `T'    rgb_gamma()   // set/retrieve gamma compression parameters
         `T'    rgb_white()   // set/retrieve RGB reference white
         `T'    rgb_xy()      // set/retrieve primaries of RGB working space
         `T'    rgb_M()       // set/retrieve RGB-XYZ transformation matrix
         `T'    rgb_invM()    // set/retrieve XYZ-RGB transformation matrix
    private:
        `RV'    rgb_gamma
        `RR'    rgb_white
        `RM'    rgb_xy, rgb_M, rgb_invM

    // CIECAM02 viewing conditions and J'a'b' coefficients
    public:
         `T'    viewcond()    // set/retrieve CAM02 viewing conditions
         `T'    ucscoefs()    // parse coefficients for J'M'h and J'a'b'
    private:
     `Cam02'    C02
        `RS'    viewcond_parsenum()
        void    surround()
        `RR'    surround_get()
        `RR'    ucscoefs_get()
    
    // Chromatic adaption
    public:
        `RM'    XYZ_to_XYZ()  // chromatic adaption
         `T'    chadapt()     // set/retrieve chromatic adaption method
        `RM'    tmatrix()     // retrieve chromatic adaption matrix
    private:
        `SS'    chadapt       // chromatic adaption setting

    // String IO
    public:
        `SS'    cvalid()       // check whether color specification is valid
         `T'    colors()       // parse or return string colors (scalar)
       `Int'    _colors()      // like colors(), but with return code
        void    add_colors()   // append colors
       `Int'    _add_colors()  // like add_colors(), but with return code
        `SS'    colors_added() // return added colors (scalar)
         `T'    Colors()       // parse or return string colors (vector)
       `Int'    _Colors()      // like Colors(), but with return code
        void    add_Colors()   // append colors
       `Int'    _add_Colors()  // like add_Colors(), but with return code
        `SC'    Colors_added() // return added colors (vector)
         `T'    names()        // parse or return color names (scalar)
         `T'    names_added()
         `T'    Names()        // parse or return color names (vector)
         `T'    Names_added()
         `T'    info()         // parse or return color info (scalar)
         `T'    info_added()
         `T'    Info()         // parse or return color info (vector)
         `T'    Info_added()
    private:
        `SS'    colors_get(), names_get(), info_get()
        `SC'    Colors_get(), _Colors_get(), Names_get(), Info_get()
        void    colors_set(), names_set(), info_set()
        void    Colors_set(), Names_set(), _Names_set(), Info_set(), _Info_set()
       `Int'    _colors_set(), _Colors_set()
       `Int'    parse_split(), _parse_split(), parse_convert(), _parse_convert()
      `Bool'    parse_named()
        `SS'    parse_stcolorstyle()
        `SR'    parse_namedcolor()

    // Set or retrieve colors
    public:
        void    set(), add(), reset(), reset_added()
        `TM'    get(), get_added()
    private:
        void    _set(), _reset(), __set()
        `TM'    _get()
        void    rgb_set()     // set RGB and reinitialize all other containers
        void    rgb_reset()   // reset RGB leaving other containers as is
        void    info_reset()  // reset info based on specified colors
        `SC'    _info_reset()

    // Set or retrieve opacity and intensity
    public:
         `T'    opacity(), alpha(), intensity()
        void    add_opacity(), add_alpha(), add_intensity()
         `T'    opacity_added(), alpha_added(), intensity_added()
        void    add_opacity_added(), add_alpha_added(), add_intensity_added()
    private:
        `RC'    opacity_get(), alpha_get(), intensity_get()
        void    opacity_set(), alpha_set(), intensity_set()
        void    _alpha(), _intensity()

    // Recycle, select, shift, order, drop
    public:
        void    recycle(), add_recycle(), recycle_added(), add_recycle_added()
        void    select(), add_select(), select_added(), add_select_added()
        void    drop(), add_drop(), drop_added(), add_drop_added()
        void    order(), add_order(), order_added(), add_order_added()
        void    reverse(), add_reverse(), reverse_added(), add_reverse_added()
        void    shift(), add_shift(), shift_added(), add_shift_added()
    private:
        void    _recycle(), _select(), __select(), _drop(), _order(),
                _reverse(), _shift()

    // Color interpolation and mixing
    public:
        void    ipolate(), add_ipolate(), ipolate_added(), add_ipolate_added()
        void    mix(), add_mix(), mix_added(), add_mix_added()
    private:
        void    _ipolate(), _mix()
        `RM'    _ipolate_get(), _mix_get()
        `RC'    _ipolate_halign()

    // Intensify, saturate, luminate
    public:
        void    intensify(), add_intensify(), intensify_added(), add_intensify_added()
        void    Intensify(), add_Intensify(), Intensify_added(), add_Intensify_added()
        void    saturate(), add_saturate(), saturate_added(), add_saturate_added()
        void    luminate(), add_luminate(), luminate_added(), add_luminate_added()
    private:
        void    _intensify(), _Intensify(), _saturate(), _luminate()
        `RR'    __intensify()

    // Grayscale conversion
    public:
        void    gray(), add_gray(), gray_added(), add_gray_added()
    private:
        void    _gray()
        `RM'    GRAY()

    // Color vision deficiency
    public:
        void    cvd(), add_cvd(), cvd_added(), add_cvd_added()
        `RM'    cvd_M()       // retrieve CVD transformation matrix
    private:
        void    _cvd()
        `RM'    CVD()
        `RM'    _cvd_M(), cvd_M_d(), cvd_M_p(), cvd_M_t()

    // Color differences and contrast ratios
    public:
        `RC'    contrast(), contrast_added(), delta(), delta_added()
    private:
        `RC'    _contrast(), _delta()
        `RC'    delta_jab(), delta_E94(), delta_E2000(), delta_euclid()

    // Translation between color spaces (without storing the colors)
    public:
        `TM'    convert()
    private:
      `Bool'    convert_parse()
        `SM'    convert_getpath()
        `TM'    convert_run()
    
    // Elementary translators
    private:
        // HEX
        `SC'    RGB_to_HEX()
        `RM'    HEX_to_RGB()
        `SS'    _RGB_to_HEX()
        `RR'    _HEX_to_RGB()
        // CMYK
        `RM'    CMYK1_to_CMYK(), CMYK_to_CMYK1()
        `RM'    RGB1_to_CMYK1(), CMYK1_to_RGB1()
        `RR'    _RGB1_to_CMYK1(), _CMYK1_to_RGB1()
        // RGB
        `RM'    RGB1_to_RGB(), RGB_to_RGB1()
        `RM'    RGB1_to_lRGB(), lRGB_to_RGB1()
        // CIE XYZ / CIE xyY
        `RM'    lRGB_to_XYZ(), XYZ_to_lRGB(), XYZ_to_XYZ1(), XYZ1_to_XYZ()
        `RM'    XYZ_to_xyY(), xyY_to_XYZ(), xyY_to_xyY1(), xyY1_to_xyY()
        // CIE L*a*b* / CIE LCh
        `RM'    XYZ_to_Lab(),  Lab_to_XYZ(), Lab_to_LCh(), LCh_to_Lab()
        `RR'    _XYZ_to_Lab(),   _Lab_to_XYZ()
        `RS'    _XYZ_to_Lab_f(), _Lab_to_XYZ_f()
        // CIE L*u*v* / HCL
        `RM'    XYZ_to_Luv(),  Luv_to_XYZ(), Luv_to_HCL(), HCL_to_Luv()
        `RR'    _XYZ_to_Luv(), _Luv_to_XYZ()
        `RS'    _XYZ_to_Luv_u(), _XYZ_to_Luv_v()
        // CIE CAM02 / J'a'b'
        `RM'    XYZ_to_CAM02(), CAM02_to_XYZ()
        `RR'    _XYZ_to_CAM02(), _CAM02_to_XYZ()
        `RM'    CAM02_to_CAM02()
        void    CAM02_to_CAM02_Q(), CAM02_setup()
        `RS'    CAM02_H(), CAM02_invH()
        `RM'    CAM02_to_Jab(), Jab_to_CAM02()
        `RM'    CAM02_to_JMh(), JMh_to_CAM02()
        // HSV
        `RM'    RGB1_to_HSV(),  HSV_to_RGB1()
        `RR'    _RGB1_to_HSV(), _HSV_to_RGB1()
        // HSL
        `RM'    RGB1_to_HSL(),  HSL_to_RGB1()
        `RR'    _RGB1_to_HSL(), _HSL_to_RGB1()
    
    // Named colors
    public:
        `SM'    namedcolors()
    private:
     `PDict'    namedcolors
        void    namedcolorindex()
      `Bool'    _namedcolorindex()
    
    // Palettes
    public:
        `SS'    pexists()
        `SM'    palettes()
        void    palette(), add_palette()
    private:
     `PDict'    palettes
       `Int'    parse_palette()
        void    paletteindex(), _paletteindex(), _paletteindex_mkalias(),
                _palette_tput()
        `TS'    _palette_tget()
      `Bool'    Palette()
        `SC'    Palette_read(), _Palette_read()
        void    Palette_palettes(), Palette_namedcolors(), Palette_lsmaps(),
                Palette_generators(), Palette_rgbmaps(), Palette_internal(),
                Palette_htmlcolors(), Palette_spmap()
    
    // Color generators
    private:
        void    generate()
        void    generate_HUE(), generate_qual(), generate_seq(), generate_div(),
                generate_HSV_heat0(), generate_HSV_terrain0()
}

end

* {smcl}
* {marker new}{bf:Constructor} {hline}
* {asis}

mata:

void `MAIN'::new()
{
    clear()
    clearsettings()
    SPACES   = ("CMYK1", "RGB1", "lRGB", "HSV", "HSL", "XYZ", "xyY", 
                "Lab", "LCh", "Luv", "HCL", "CAM02", "JMh", "Jab")'
    SPACES2  = ("HEX", "CMYK", "RGB", "XYZ1", "xyY1")'
    EDGELIST = ("RGB1", "RGB"  ) \ ("RGB"  , "HEX"  ) \
               ("RGB1", "CMYK1") \ ("CMYK1", "CMYK" ) \
               ("RGB1", "HSV"  ) \ ("RGB1" , "HSL"  ) \
               ("RGB1", "lRGB" ) \ ("lRGB" , "XYZ"  ) \
               ("XYZ" , "XYZ1" ) \ ("XYZ"  , "xyY"  ) \ ("xyY"  , "xyY1" ) \ 
               ("XYZ" , "Lab"  ) \ ("Lab"  , "LCh"  ) \
               ("XYZ" , "Luv"  ) \ ("Luv"  , "HCL"  ) \
               ("XYZ" , "CAM02") \ ("CAM02", "JMh"  ) \ ("CAM02", "Jab"  )
    EDGELIST = EDGELIST \ EDGELIST[,(2,1)] \ ("RGB1", "GRAY") \ ("RGB1", "CVD")
}

void `MAIN'::clear()
{
    data = data1 = `DATA'()
    S = &data
    rgb_set(J(0, 3, .))
    N1 = 0
}

void `MAIN'::clearindex()
{
    palettes = NULL
    rmexternal("ColrSpace_paletteindex")
    namedcolors = NULL
    rmexternal("ColrSpace_namedcolorindex")
}

void `MAIN'::clearsettings()
{
    rgbspace("")
    xyzwhite("")
    viewcond(.)
    ucscoefs("")
    chadapt("")
}

void `MAIN'::replacedata()
{
    N1 = data1.n
    swap(data, data1)
    data1 = `DATA'()
}

void `MAIN'::updatedata()
{
    removeadded()
    appenddata()
}

void `MAIN'::appenddata()
{
    // append colors
    N1             = data1.n
    data.n         = data.n + data1.n
    data.RGB       = data.RGB       \ data1.RGB
    data.alpha     = data.alpha     \ data1.alpha
    data.intensity = data.intensity \ data1.intensity
    data.names     = data.names     \ data1.names
    data.info      = data.info      \ data1.info
    data.stok      = data.stok      \ data1.stok
    // update metadata (if set in appended colors)
    if (data1.pclass!="")  data.pclass = data1.pclass
    if (data1.name!="")    data.name   = data1.name
    if (data1.note!="")    data.note   = data1.note
    if (data1.source!="")  data.source = data1.source
    if (data1.isip)        data.isip   = data1.isip
    // clear temporary container
    data1 = `DATA'()
}

void `MAIN'::removeadded()
{
    `Int' n

    if (N1==0) return
    n = data.n - N1
    if (n==0) {
        data.n         = 0
        data.RGB       = J(0, 3, .)
        data.alpha     = J(0, 1, .)
        data.intensity = J(0, 1, .)
        data.names     = J(0, 1, "")
        data.info      = J(0, 1, "")
        data.stok      = J(0, 1, `FALSE')
        return
    }
    data.n         = n
    data.RGB       = data.RGB[|1,1 \ n,.|]
    data.alpha     = data.alpha[|1 \ n|]
    data.intensity = data.intensity[|1 \ n|]
    data.names     = data.names[|1 \ n|]
    data.info      = data.info[|1 \ n|]
    data.stok      = data.stok[|1 \ n|]
}

void `MAIN'::copydata()
{
    data1 = data
}

void `MAIN'::copyadded()
{
    `Int' r
    
    data1 = data
    if (data1.n==N1) return
    r = data1.n - N1 + 1
    if (r<=1) {
        data1.n         = 0
        data1.RGB       = J(0, 3, .)
        data1.alpha     = J(0, 1, .)
        data1.intensity = J(0, 1, .)
        data1.names     = J(0, 1, "")
        data1.info      = J(0, 1, "")
        data1.stok      = J(0, 1, `FALSE')
        return
    }
    data1.n         = N1
    data1.RGB       = data1.RGB[|r,1 \ .,.|]
    data1.alpha     = data1.alpha[|r \ .|]
    data1.intensity = data1.intensity[|r \ .|]
    data1.names     = data1.names[|r \ .|]
    data1.info      = data1.info[|r \ .|]
    data1.stok      = data1.stok[|r \ .|]
}

end

* {smcl}
* {marker util}{bf:Some utilities} {hline}
* {asis}

mata:

void `MAIN'::describe(| `Bool' shrt)
{
    `Int' i, n, l
    `SS'  ifmt
    `SM'  M
    
    display("")
    printf(`"{txt}  name()      = {res}"%s"\n"', name())
    printf(`"{txt}  pclass()    = {res}"%s"\n"', pclass())
    printf(`"{txt}  note()      = {res}"%s"\n"', note())
    printf(`"{txt}  source()    = {res}"%s"\n"', source())
    printf(`"{txt}  isipolate() = {res}%g\n"'  , isipolate())
    printf(`"{txt}  N()         = {res}%g\n"'  , n = N())
    printf(`"{txt}  N_added()   = {res}%g\n"'  , N_added())
    if (n==0) return
    if (args()==1 & shrt) return
    l = trunc(log10(n))+1
    ifmt = "%"+strofreal(l)+"s"
    M = abbrev((Colors(1), Names(), Info()),23)
    display("")
    printf(" "*(2+l+2) + "{txt}%-24s%-24s%-24s\n","Colors()","Names()","Info()")
    printf("{txt}  {hline " + strofreal(72+2+l) +"}\n")
    for (i=1;i<=n;i++) {
        printf("{txt}  "+ifmt+"  {res}%-24s%-24s%-24s\n"',
            strofreal(i),M[i,1], M[i,2], M[i,3])
    }
}

void `MAIN'::settings()
{
    `RM' M
    
    M = rgb_gamma()
    printf("\n{txt}  rgb_gamma():  {it:gamma} = {res}%g\n",M[1])
    if (length(M)>1) {
        printf("{txt}               {it:offset} = {res}%g\n",M[2])
        printf("{txt}           {it:transition} = {res}%g\n",M[3])
        printf("{txt}                {it:slope} = {res}%g\n",M[4])
    }
    M = rgb_white()
    printf("\n{txt}  rgb_white():      {it:X} = {res}%g\n",M[1])
    printf("{txt}                    {it:Y} = {res}%g\n",M[2])
    printf("{txt}                    {it:Z} = {res}%g\n",M[3])
    M = rgb_xy()
    if (length(M)) {
        printf("\n{txt}  rgb_xy():     {it:red x} = {res}%g\n",M[1,1])
        printf("{txt}                {it:red y} = {res}%g\n",M[1,2])
        printf("{txt}              {it:green x} = {res}%g\n",M[2,1])
        printf("{txt}              {it:green y} = {res}%g\n",M[2,2])
        printf("{txt}               {it:blue x} = {res}%g\n",M[3,1])
        printf("{txt}               {it:blue y} = {res}%g\n",M[3,2])
    }
    else {
        M = rgb_M()
        printf("\n{txt}  rgb_M():     {it:red SX} = {res}%g\n",M[1,1])
        printf("{txt}               {it:red SY} = {res}%g\n",M[2,1])
        printf("{txt}               {it:red SZ} = {res}%g\n",M[3,1])
        printf("{txt}             {it:green SX} = {res}%g\n",M[1,2])
        printf("{txt}             {it:green SY} = {res}%g\n",M[2,2])
        printf("{txt}             {it:green SZ} = {res}%g\n",M[3,2])
        printf("{txt}              {it:blue SX} = {res}%g\n",M[1,3])
        printf("{txt}              {it:blue SY} = {res}%g\n",M[2,3])
        printf("{txt}              {it:blue SZ} = {res}%g\n",M[3,3])
    }
    M = xyzwhite()
    printf("\n{txt}  xyzwhite():       {it:X} = {res}%g\n",M[1])
    printf("{txt}                    {it:Y} = {res}%g\n",M[2])
    printf("{txt}                    {it:Z} = {res}%g\n",M[3])
    M = viewcond()
    printf("\n{txt}  viewcond():     {it:Y_b} = {res}%g\n",M[1])
    printf("{txt}                  {it:L_A} = {res}%g\n",M[2])
    printf("{txt}                    {it:F} = {res}%g\n",M[3])
    printf("{txt}                    {it:c} = {res}%g\n",M[4])
    printf("{txt}                  {it:N_c} = {res}%g\n",M[5])
    M = ucscoefs()
    printf("\n{txt}  ucscoefs():     {it:K_L} = {res}%g\n",M[1])
    printf("{txt}                  {it:c_1} = {res}%g\n",M[2])
    printf("{txt}                  {it:c_2} = {res}%g\n",M[3])
    printf(`"\n{txt}  chadapt():   {it:method} = {res}"%s"\n"',chadapt())
}

`RS' `MAIN'::N() return(data.n)

`RS' `MAIN'::N_added() return(N1)

`T' `MAIN'::name(| `SS' s)
{
    if (args()==0) return(data.name)
    data.name = s
}

`T' `MAIN'::pclass(| `SS' s)
{
    if (args()==0) return(data.pclass)
    data.pclass = s
}

`T' `MAIN'::note(| `SS' s)
{
    if (args()==0) return(data.note)
    data.note = s
}

`T' `MAIN'::source(| `SS' s)
{
    if (args()==0) return(data.source)
    data.source = s
}

`Bool' `MAIN'::isipolate() return(data.isip)

`SS' `MAIN'::gettok(`SS' s0, | `SS' rest)
{
    // gets the first word and returns remainder in -rest-; removes 
    // blanks around first word and around rest
    `SS'  s
    `Int' p
    
    s = strtrim(s0)
    if (p = strpos(s, " ")) {
        rest = strtrim(substr(s, p+1, .))
        return(substr(s, 1, p-1))
    }
    rest = ""
    return(s)
}

`Bool' `MAIN'::smatch(`SS' s, `SS' s0)
{   // replaces s by s0 if _smatch(s, s0) is true
    if (_smatch(s, s0)) {
        s = s0
        return(`TRUE')
    }
    return(`FALSE')
}

`Bool' `MAIN'::_smatch(`SS' s, `SS' s0)
{   // checks whether s matches s0, word by word, allowing abbreviation
    // assumes s to be lower case and checks against lowercase(s0)
    // abbreviation to "nothing" also counts as a match; i.e. empty s will 
    // match anything
    `Int'  i, c, c0
    `SR'   S, S0
    `SS'   si
    
    S = tokens(s); c = cols(S)
    if (c==0) return(`TRUE') // s is empty
    S0 = tokens(s0); c0 = cols(S0)
    if (c0==0) return(`FALSE') // no match if s0 is empty
    if (c>c0)  return(`FALSE') // no match if s has more words than s0
    if (c0==1) {
        if (S==substr(strlower(S0), 1, strlen(S))) return(`TRUE')
        return(`FALSE')
    }
    if (c<c0) S = S, J(1, c0-c, "")
    for (i=c0; i; i--) {
        si = S[i]
        if (si!=substr(strlower(S0[i]), 1, strlen(si))) return(`FALSE')
    }
    return(`TRUE')
}

`SS' `MAIN'::findkey(`SS' s0, `SC' keys, | `SS' def)
{   // finds matching key allowing abbreviation and differences in
    // capitalization; in case of multiple matches, capitalization and
    // alphabetical order are considered to determine the best match
    // returns def if s0 is empty
    // returns empty string if key not found
    `Int'  i
    `IntC' p
    `SS'   s
    
    if (strtrim(s0)=="") return(def)
    i = rows(keys)
    if (i==0) return("")
    // find all matching keys ignoring case and allowing abbreviation
    p = J(i,1,.)
    s = strlower(s0)
    for (; i; i--) p[i] = _smatch(s, keys[i])
    p = selectindex(p)
    i = length(p)
    // no match
    if (i<1)  return("")
    // unique match
    if (i==1) return(keys[p])
    // find best match
    return(_findkey(s0, keys, p))
}

`SS' `MAIN'::_findkey(`SS' s, `SC' keys, `IntC' p) // p will be modified
{   // select candidates based on capitalization, prioritizing from
    // left to right; among remaining candidates, select topmost candidate
    // based on alphabetical order
    `Int'  i, i1, l, r
    `IntC' I, m
    
    I = J(rows(keys),1,1)
    l = strlen(s)
    for (i=1;i<=l;i++) {
        if (substr(s,i,1)==" ") continue // move to next token in s
        // skip blanks before token in each candidate
        while (any(m=(substr(keys[p],I[p],1):==" "))) I[p] = I[p] + m
        // determine end of current token in s
        i1 = i + strpos(substr(s,i+1,.)+" ", " ") - 1
        // check case within token, character by character
        for (;i<=i1;i++) {
            m = selectindex(substr(s,i,1):==substr(keys[p],I[p],1))
            r = rows(m)
            if (r) {
                if (r==1) return(keys[p[m]]) // unique match; done
                if (r<rows(p)) p = p[m] // update list of candidates
            } // note: all will be kept if none has a match
            I[p] = I[p] :+ 1 // move to next character in each candidate
        }
        // move to next token in each candidate
        if (i<l) I[p] = I[p] :+ strpos(substr(keys[p],I[p],.):+" ", " ")
    }
    // no unique match; select top candidate based on alphabetical order
    return(sort(keys[p],1)[1])
}

void `MAIN'::assert_cols(`T' M, `RS' c)
{
    if (cols(M)!=c) {
        printf("{err}input must have %g columns\n", c)
        exit(3200)
    }
}

void `MAIN'::assert_size(`T' M, `RS' r, `RS' c)
{
    if (rows(M)!=r | cols(M)!=c) {
        printf("{err}input must be %g x %g\n", r, c)
        exit(3200)
    }
}

`RM' `MAIN'::colipolate(`RM' C, `Int' n0, | `RV' range0, `RS' power, `RV' pos, 
    `Bool' pad)
{
    `Bool' haspos
    `Int'  r, n
    `RV'   range
    `RC'   from, to
    
    n = (n0<0 ? 0 : trunc(n0))
    if (args()<6) pad = `FALSE'
    if (power<=0) {
        printf("{err}power = %g not allowed; must be strictly positive\n", power)
        exit(3498)
    }
    haspos = length(pos)>0 & any(pos:<.)
    if (n>=.) return(C)                           // return unchanged C
    if (n==0) return(J(0, cols(C), missingof(C))) // return void
    r = rows(C)
    if (r==0) return(J(n, cols(C), missingof(C))) // return missing
    if (r==1) return(J(n, 1, C))                  // duplicate
    range = _colipolate_setrange(range0)
    if (r==n) {
        if (range==(0,1) & haspos==`FALSE') return(C) // no interpolation needed
        if (range==(1,0) & haspos==`FALSE') return(C[r::1,]) // reverse order
    }
    _colipolate_fromto(r, n, range, power, (haspos ? pos : .), pad, from=., to=.)
    if (haspos) return(_colipolate_pos(C, from, to))
    return(_colipolate(C, from, to))
}

`RM' `MAIN'::colipolate_c(`RM' C, `Int' n0)
{   // circular (cyclic) interpolation
    `Int'  r, n
    `RC'   from, to

    n = (n0<0 ? 0 : trunc(n0))
    if (n>=.) return(C)                           // return unchanged C
    if (n==0) return(J(0, cols(C), missingof(C))) // return void
    r = rows(C)
    if (r==0) return(J(n, cols(C), missingof(C))) // return missing
    if (r==1) return(J(n, 1, C))                  // duplicate
    if (r==n) return(C)
    from = (0::r+1)/r :- 1/(2*r)
    to   = (1::n)/n   :- 1/(2*n)
    return(_colipolate(C[r,.]\C\C[1,.], from, to))
}

`RV' `MAIN'::_colipolate_setrange(`RV' range0)
{
    `RV' range
    
    if      (length(range0)==0)   range = (0,1)
    else if (length(range0)==1)   range = (range0, 1)
    else                          range = (range0[1], range0[2])
    if (range[1]>=.) range[1] = 0
    if (range[2]>=.) range[1] = 1
    return(range)
}

void `MAIN'::_colipolate_fromto(`Int' r, `Int' n, `RV' range, `RS' power, 
    `RV' pos, `Bool' pad, `RC' from, `RC' to)
{
    `RS' i
    
    // from
    from = rangen(0, 1, r)
    if (pos!=.) {
        // import values from pos
        for (i=length(pos); i; i--) {
            if (i>r) continue
            if (pos[i]<.) from[i] = pos[i]
        }
    }
    if (pad) from = from * ((r-1)/r) :+ (1/(2*r))
    // to
    if (pad) range = range * ((n-1)/n) :+ (1/(2*n))
    if (n==1)      to = (range[1]+range[2])/2
    else if (n==2) to = (range[1], range[2])'
    else {
        to = rangen(0, 1, n)
        if (power<.) to = to:^power
        to = to * (range[2]-range[1]) :+ range[1]
    }
}

`RM' `MAIN'::_colipolate_pos(`RM' C0, `RC' from0, `RC' to) 
{   // note: function will only be called if length(from0)>=2
    `RC'  p, a, b, from
    `RM'  C

    a = from0[|1 \ rows(from0)-1|]; b = from0[|2 \ rows(from0)|]
    if (all(a:<b)) return(_colipolate(C0, from0, to)) // strictly ascending
    if (all(a:>b)) return(_colipolate(C0, from0, to)) // strictly descending
    p = ::order(from0, 1)
    from = from0[p,]; C = C0[p,]
    a = from[|1 \ rows(from)-1|]; b = from[|2 \ rows(from)|]
    if (any(a:==b)==0) return(_colipolate(C, from, to)) // no doubles
    _colipolate_collapse(C, from)
    if (rows(C)==1) return(J(rows(to), 1, C))
    return(_colipolate(C, from, to))
}

void `MAIN'::_colipolate_collapse(`RM' C, `RC' from)
{
    `Int' i, j, a, b
    
    i = j = b = rows(from)
    for (--i; i; i--) {
        if (from[i]!=from[b]) {
            from[j] = from[b]
            a = i+1
            if (a==b) C[j,] = C[b,]
            else      C[j,] = mean(C[|a,1 \ b,.|])
            j--; b = i
        }
    }
    from[j] = from[b]
    if (b==1) C[j,] = C[b,]
    else      C[j,] = mean(C[|1,1 \ b,.|])
    C = C[|j,1 \ .,.|]; from = from[|j \ .|]
}

`RM' `MAIN'::_colipolate(`RM' y0, `RC' x0, `RC' x1)
{
    `Int' i, j, k, l, n0, n1, c, reverse
    `RM'  y1
    
    n0 = rows(y0); c = cols(y0); n1 = rows(x1); reverse = 0
    if (x1[1]>x1[n1]) {
        reverse = 1
        x1 = x1[n1::1]
    }
    y1 = J(n1, c, .)
    i = 1
    for (j=1; j<=n1; j++) {
        while (x0[i]<x1[j]) {
            i++
            if (i>=n0) {
                i = n0
                break
            }
        }
        l = (i==1 ? 2 : i)
        for (k=c; k; k--) y1[j,k] = y0[l-1,k] + 
            (y0[l,k] - y0[l-1,k]) * (x1[j] - x0[l-1])/(x0[l] - x0[l-1])
    }
    if (reverse) {
        x1 = x1[n1::1]
        return(y1[n1::1,])
    }
    return(y1)
}

`TM' `MAIN'::colrecycle(`TM' M0, `Int' n0)
{
    `Int' i, r, n
    `TM'  M
    
    n = (n0<0 ? 0 : trunc(n0))
    if (n>=.) return(M0)
    if (n==0) return(J(0, cols(M0), missingof(M0)))
    r = rows(M0)
    if (r==0 | n==r) return(M0)
    if (n<r) return(M0[|1,1 \ n,cols(M0)|])
    M = M0 \ J(n-r, cols(M0), missingof(M0))
    for (i=(r+1); i<=n; i++) M[i,] = M[mod(i-1, r) + 1,]
    return(M)
}

`RM' `MAIN'::lsmap(`RM' R, `RM' G, `RM' B, `RS' n, | `RV' range0)
{                  // (consistency of input is not checked)
    `RV' range
    
    assert_cols(R, 3); assert_cols(G, 3); assert_cols(B, 3)
    range = clip(_colipolate_setrange(range0), 0, 1) // restrict range to [0,1]
    return((_lsmap(R, n, range[1], range[2]),
            _lsmap(G, n, range[1], range[2]),
            _lsmap(B, n, range[1], range[2])))
}

`RC' `MAIN'::_lsmap(`RM' xy, `RS' n, `RS' from, `RS' to)
{
    `Int' i, j, reverse
    `RC'  x, y

    if (n==0) return(J(0, 1, .))
    reverse = 0
    if (from>to) {
        reverse = 1
        x = rangen(to, from, n)
    }
    else x = rangen(from, to, n)
    y = J(n, 1, .)
    j = 1
    for (i=1; i<=n; i++) {
        while (xy[j+1,1]<x[i]) j++
        if (x[i]==xy[j,1]) y[i] = xy[j,3]
        else y[i] = xy[j,3] + (xy[j+1,2] - xy[j,3]) * (x[i] - xy[j,1]) / 
                                                      (xy[j+1,1] - xy[j,1])
    }
    if (reverse) return(y[n::1])
    return(y)
}

`RM' `MAIN'::clip(`RM' C0, `RS' a, `RS' b)
{
    `Int' i
    `RM'  C

    C = C0
    for (i=cols(C); i; i--) C[,i] = _clip(C[,i], a, b)
    return(C)
}

`RV' `MAIN'::_clip(`RV' C0, `RS' a, `RS' b)
{
    `Int' i
    `RM'  C

    C = C0
    for (i=length(C); i; i--) C[i] = __clip(C[i], a, b)
    return(C)
}

`RS' `MAIN'::__clip(`RS' c, `RS' a, `RS' b) 
    return(c<a ? a : (c<=b ? c : (c>=. ? c : b)))

`SR' `MAIN'::_tokens(`SS' c, `SS' wchar)
{   // modified version of tokens; omits delimiters and inserts empty elements 
    // between delimiters (as well as at the start if the string begins with a
    // delimiter); for example, _tokens(";a;;;b;c", ";") will result in
    // ("", "a", "", "", "b", "c")
    `Int'  i, j
    `Bool' gap
    `SR'   C

    if (strtrim(wchar)=="") return(tokens(c))
    C = tokens(c, wchar)
    gap = `TRUE'
    j = 0
    for (i=1; i<=length(C); i++) {
        if (strpos(wchar, C[i])) {
            if (gap) C[++j] = ""
            else     gap = `TRUE'
            continue
        }
        gap = `FALSE'
        C[++j] = C[i] 
    }
    if (j) C = C[|1 \ j|]
    return(C)
}

end

* {smcl}
* {marker illuminants}{bf:Illuminants (CIE 1931 2° and CIE 1964 10°)} {hline}
* Sources:
*   {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf"}
*   {browse "http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html"} (BL)
*   {browse "https://en.wikipedia.org/wiki/Standard_illuminant"} (Wiki)
* {asis}

mata:

void `MAIN'::illuminants()
{
    illuminants.notfound(J(1,0,.))
    // Incandescent / Tungsten, 2856 K
    illuminants.put("A"             , (109.85 , 100, 35.585))
    illuminants.put("A 10 degree"   , (111.144, 100, 35.2  ))
    // Direct Sunlight at Noon, 4874 K (obsolete)
    illuminants.put("B"             , ( 99.09 , 100, 85.324))  // default variant
    illuminants.put("B BL"          , ( 99.072, 100, 85.223))  // BL
    illuminants.put("B 10 degree"   , (0.34980, 0.35270))  // Wiki 
    // North Sky Daylight, 6774 K (obsolete)
    illuminants.put("C"             , ( 98.074, 100, 118.232))
    illuminants.put("C 10 degree"   , ( 97.285, 100, 116.145))
    // Daylight, used for Color Rendering, 5000 K (Wiki: Horizon Light, 5003 K)
    illuminants.put("D50"           , ( 96.422, 100,  82.521))
    illuminants.put("D50 10 degree" , ( 96.72 , 100,  81.427))
    // Daylight, used for Photography, 5500 K (Wiki: Mid-morning / Mid-afternoon Daylight, 5503 K)
    illuminants.put("D55"           , ( 95.682, 100,  92.149))
    illuminants.put("D55 10 degree" , ( 95.799, 100,  90.926))
    // New version of North Sky Daylight, 6504 K (Wiki: Noon Daylight)
    illuminants.put("D65"           , ( 95.047, 100, 108.883))
    illuminants.put("D65 10 degree" , ( 94.811, 100, 107.304))
    // Daylight, 7500 K (Wiki: North sky Daylight, 7504 K)
    illuminants.put("D75"           , ( 94.972, 100, 122.638))
    illuminants.put("D75 10 degree" , ( 94.416, 100, 120.641))
    // High eff. blue phosphor monitors, 9300 K
    illuminants.put("9300K"         , ( 97.135, 100, 143.929))
    // Uniform energy illuminant, 5400 K (Wiki: 5454 K)
    illuminants.put("E"             , (100    , 100, 100))
    // Wiki: Daylight Fluorescent, 6430 K
    illuminants.put("F1"            , (0.31310, 0.33727))
    illuminants.put("F1 10 degree"  , (0.31811, 0.33559))
    // Cool White Fluorescent (CWF), 4200 K (Wiki: 4230 K)
    illuminants.put("F2"            , ( 99.186, 100,  67.393))
    illuminants.put("F2 10 degree"  , (103.279, 100,  69.027))
    // Wiki: White Fluorescent, 3450 K
    illuminants.put("F3"            , (0.40910, 0.39430))
    illuminants.put("F3 10 degree"  , (0.41761, 0.38324))
    // Wiki: Warm White Fluorescent, 2940 K
    illuminants.put("F4"            , (0.44018, 0.40329))
    illuminants.put("F4 10 degree"  , (0.44920, 0.39074))
    // Wiki: Daylight Fluorescent, 6350 K
    illuminants.put("F5"            , (0.31379, 0.34531))
    illuminants.put("F5 10 degree"  , (0.31975, 0.34246))
    // Wiki: Lite White Fluorescent, 4150 K
    illuminants.put("F6"            , (0.37790, 0.38835))
    illuminants.put("F6 10 degree"  , (0.38660, 0.37847))
    // Broad-band Daylight Fluorescent, 6500 K (Wiki: D65 simulator, Daylight simulator)
    illuminants.put("F7"            , ( 95.041, 100, 108.747))
    illuminants.put("F7 10 degree"  , ( 95.792, 100, 107.686))
    // Wiki: D50 simulator, Sylvania F40 Design 50, 5000 K
    illuminants.put("F8"            , (0.34588, 0.35875))
    illuminants.put("F8 10 degree"  , (0.34902, 0.35939))
    // Wiki: Cool White Deluxe Fluorescent, 4150 K
    illuminants.put("F9"            , (0.37417, 0.37281))
    illuminants.put("F9 10 degree"  , (0.37829, 0.37045))
    // Wiki: Philips TL85, Ultralume 50, 5000 K
    illuminants.put("F10"           , (0.34609, 0.35986))
    illuminants.put("F10 10 degree" , (0.35090, 0.35444))
    // Narrow-band White Fluorescent, 4000 K (Wiki: Philips TL84, Ultralume 40)
    illuminants.put("F11"           , (100.962, 100,  64.35 ))
    illuminants.put("F11 10 degree" , (103.863, 100,  65.607))
    // Wiki: Philips TL83, Ultralume 30, 3000 K
    illuminants.put("F12"           , (0.43695, 0.40441))
    illuminants.put("F12 10 degree" , (0.44256, 0.39717))
}

`T' `MAIN'::xyzwhite(| `TV' X, `RS' Y, `RS' Z)
{
    if (args()==0) return(white)
    if      (args()==3) white = _white((X,Y,Z))
    else if (args()==2) white = _white((X,Y))
    else                white = _white(X)
    C02.set = 0 // reset CIECAM02 containers
}

`RR' `MAIN'::_white(`TV' white)
{
    if (length(white)==0) return(_white_set(_white_get("")))
    if (white==.)         return(_white_set(_white_get("")))
    if (isstring(white))  return(_white_set(_white_get(white)))
    return(_white_set(white))
}

`RR' `MAIN'::_white_set(`RV' white)
{
    if (missing(white)) exit(error(127)) // missings not allowed
    if (length(white)==2) { // xy
        if (cols(white)==1) return(xyY_to_XYZ((white', 100)))
        return(xyY_to_XYZ((white, 100)))
    }
    if (length(white)==3) { // XYZ
        if (cols(white)==1) return(white')
        return(white)
    }
    exit(error(503)) // wrong size
}

`RR' `MAIN'::_white_get(`SS' illuminant0)
{
    `SS' illuminant
    `RR' white
    
    if (illuminants.N()==0) illuminants()
    illuminant = strtrim(illuminant0)
    if (illuminant=="") illuminant = "D65" // default
    white = illuminants.get(illuminant)
    if (length(white)==0) {
        illuminant = findkey(illuminant, illuminants.keys())
        if (illuminant!="") white = illuminants.get(illuminant)
        else {
            white = strtoreal(tokens(illuminant0))
            if ((length(white)!=3 & length(white)!=2) | missing(white)) {
                display("{err}illuminant '" + illuminant0 + "' not allowed")
                exit(3498)
            }
        }
    }
    return(white)
}

end

* {smcl}
* {marker rgbspaces}{bf:RGB working spaces} {hline}
* Sources:
*   {browse "http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf"}
*   {browse "http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html"} (BL)
* {asis}

mata:

void `MAIN'::rgbspace(| `SS' space0)
{
    `SS' space
    
    space = strtrim(space0)
    if (space=="") space = "sRGB" // default
    space = strlower(space)
    if      (smatch(space, "Adobe 1998")) {     // Adobe RGB (1998)
        rgb_gamma(2.2) 
        rgb_white("D65") 
        rgb_xy((0.6400, 0.3300) \ (0.2100, 0.7100) \ (0.1500, 0.0600))
    }
    else if (smatch(space, "Apple")) {          // Apple RGB
        rgb_gamma(1.8)
        rgb_white("D65")
        rgb_xy((0.6250, 0.3400) \ (0.2800, 0.5950) \ (0.1550, 0.0700))
    }
    else if (smatch(space, "Best")) {           // Best RGB
        rgb_gamma(2.2)
        rgb_white("D50")
        rgb_xy((0.7347, 0.2653) \ (0.2150, 0.7750) \ (0.1300, 0.0350))
    }
    else if (smatch(space, "Beta")) {           // Beta RGB
        rgb_gamma(2.2)
        rgb_white("D50")
        rgb_xy((0.6888, 0.3112) \ (0.1986, 0.7551) \ (0.1265, 0.0352))
    }
    else if (smatch(space, "Bruce")) {          // Bruce RGB
        rgb_gamma(2.2)
        rgb_white("D65")
        rgb_xy((0.6400, 0.3300) \ (0.2800, 0.6500) \ (0.1500, 0.0600))
    }
    else if (smatch(space, "CIE")) {            // CIE RGB
        rgb_gamma(2.2)
        rgb_white("E")
        rgb_xy((0.7350, 0.2650) \ (0.2740, 0.7170) \ (0.1670, 0.0090))
    }
    else if (smatch(space, "ColorMatch")) {     // ColorMatch RGB
        rgb_gamma(1.8)
        rgb_white("D50")
        rgb_xy((0.6300, 0.3400) \ (0.2950, 0.6050) \ (0.1500, 0.0750))
    }
    else if (smatch(space, "Don 4")) {          // Don RGB 4
        rgb_gamma(2.2)
        rgb_white("D50")
        rgb_xy((0.6960, 0.3000) \ (0.2150, 0.7650) \ (0.1300, 0.0350))
    }
    else if (smatch(space, "ECI v2")) {         // ECI RGB v2
        rgb_gamma(3, 0.16, 216/24389 /*=0.08/(24389/2700)*/, 24389/2700)
        rgb_white("D50")
        rgb_xy((0.6700, 0.3300) \ (0.2100, 0.7100) \ (0.1400, 0.0800))
    }
    else if (smatch(space, "Ekta PS5")) {       // Ekta Space PS5
        rgb_gamma(2.2)
        rgb_white("D50")
        rgb_xy((0.6950, 0.3050) \ (0.2600, 0.7000) \ (0.1100, 0.0050))
    }
    else if (smatch(space, "Generic")) {        // Generic RGB (source?)
        rgb_gamma(1.8)
        rgb_white("D65")
        rgb_xy((0.6295, 0.3407) \ (0.2949, 0.6055) \ (0.1551, 0.0762))
    }
    else if (smatch(space, "HDTV")) {           // HDTV (HD-CIF)
        rgb_gamma(1/0.45, 0.099, 0.018, 4.5)
        rgb_white("D65")
        rgb_xy((0.6400, 0.3300) \ (0.3000, 0.6000) \ (0.1500, 0.0600))
    }
    else if (smatch(space, "NTSC")) {           // NTSC RGB
        rgb_gamma(1/0.45, 0.099, 0.018, 4.5)
        rgb_white("C")
        rgb_xy((0.6700, 0.3300) \ (0.2100, 0.7100) \ (0.1400, 0.0800))
    }
    else if (smatch(space, "PAL/SECAM")) {      // PAL/SECAM RGB
        rgb_gamma(1/0.45, 0.099, 0.018, 4.5)
        rgb_white("D65")
        rgb_xy((0.6400, 0.3300) \ (0.2900, 0.6000) \ (0.1500, 0.0600))
    }
    else if (smatch(space, "ProPhoto")) {       // ProPhoto RGB
        rgb_gamma(1.8)
        rgb_white("D50")
        rgb_xy((0.7347, 0.2653) \ (0.1596, 0.8404) \ (0.0366, 0.0001))
    }
    else if (smatch(space, "SGI")) {            // SGI
        rgb_gamma(1.47)
        rgb_white("D65")
        rgb_xy((0.6250, 0.3400) \ (0.2800, 0.5950) \ (0.1550, 0.0700))
    }
    else if (smatch(space, "SMPTE-240M")) {     // SMPTE-240M
        rgb_gamma(1/0.45, 0.112, 0.023, 4.0)
        rgb_white("D65")
        rgb_xy((0.6300, 0.3400) \ (0.3100, 0.5950) \ (0.1550, 0.0700))
    }
    else if (smatch(space, "SMPTE-C")) {        // SMPTE-C RGB
        rgb_gamma(1/0.45, 0.099, 0.018, 4.5)
        rgb_white("D65")
        rgb_xy((0.6300, 0.3400) \ (0.3100, 0.5950) \ (0.1550, 0.0700))
    }
    else if (smatch(space, "sRGB")) {          // sRGB (default variant)
        rgb_gamma(2.4, 0.055, 0.0031308, 12.92)
        rgb_white("D65")
        // using primaries from www.brucelindbloom.com
        rgb_xy((0.6400, 0.3300) \ (0.3000, 0.6000) \ (0.1500, 0.0600))
        // with this method the correspondence between RGB white and the 
        // white point is exact; this is not true for sRGB2 and sRGB3
    }
    else if (smatch(space, "sRGB2")) {           // sRGB (2nd variant)
        rgb_gamma(2.4, 0.055, 0.0031308, 12.92)
        rgb_white("D65")
        // using XYZ to RGB matrix given in IEC 61966-2-1
        rgb_invM(( 3.2406, -1.5372, -0.4986) \
                 (-0.9689,  1.8758,  0.0415) \
                 ( 0.0557, -0.2040,  1.0570))
    }
    else if (smatch(space, "sRGB3")) {          // sRGB (3rd variant)
        rgb_gamma(2.4, 0.055, 0.0031308, 12.92)
        rgb_white("D65")
        // using RGB to XYZ matrix given in IEC 61966-2-1
        rgb_M(( .4124, .3576, .1805) \
              ( .2126, .7152, .0722) \
              ( .0193, .1192, .9505))
    }
    else if (smatch(space, "Wide Gamut")) {     // Wide Gamut RGB
        rgb_gamma(2.2)
        rgb_white("D50")
        rgb_xy((0.7347, 0.2653) \ (0.1152, 0.8264) \ (0.1566, 0.0177))
    }
    else if (smatch(space, "Wide Gamut BL")) {  // Wide Gamut RGB (BL variant)
        rgb_gamma(2.2)
        rgb_white("D50")
        // using primaries from www.brucelindbloom.com
        rgb_xy((0.7350, 0.2650) \ (0.1150, 0.8260) \ (0.1570, 0.0180))
    }
    else {
        display("{err}rgbspace '" + space0 + "' not found")
        exit(3499)
    }
}

`T' `MAIN'::rgb_gamma(| `TV' gamma0, `RS' offset, `RS' transition, `RS' slope)
{
    `RV' gamma
    
    if (args()==0) return(rgb_gamma)
    if (args()==1) {
        if (isstring(gamma0)) gamma = strtoreal(tokens(gamma0))
        else                  gamma = gamma0
    }
    else if (args()==4) gamma = (gamma0, offset, transition, slope)
    else _error(3001)   // wrong number of args
    if (missing(gamma)) exit(error(127))   // missings not allowed
    if (length(gamma)!=1 & length(gamma)!=4) exit(error(503)) // wrong size
    rgb_gamma = gamma
    data.stok = J(data.n, 1, `FALSE')   // !!!
}

`T' `MAIN'::rgb_white(| `TV' X, `RS' Y, `RS' Z)
{
    if (args()==0) return(rgb_white)
    if      (args()==3) rgb_white = _white((X,Y,Z))
    else if (args()==2) rgb_white = _white((X,Y))
    else                rgb_white = _white(X)
    if (length(rgb_xy)) rgb_xy(rgb_xy) // recompute transformation matrices
    data.stok = J(data.n, 1, `FALSE')   // !!!
}

`T' `MAIN'::rgb_xy(| `RM' xy)
{
    if (args()==0) return(rgb_xy)
    assert_size(xy, 3, 2)
    if (missing(xy)) exit(error(127)) // missings not allowed
    // see http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    rgb_M = J(3,1,1)
    rgb_M = ((xy[,1]:/xy[,2]), rgb_M, (rgb_M:-xy[,1]:-xy[,2]):/xy[,2])'
    rgb_M = rgb_M :* (luinv(rgb_M) * rgb_white')'
    rgb_invM = luinv(rgb_M)
    rgb_xy = xy
    data.stok = J(data.n, 1, `FALSE')  // !!!
}

`T' `MAIN'::rgb_M(| `RM' M)
{
    if (args()==0) return(rgb_M / 100)
    assert_size(M, 3, 3)
    if (missing(M)) exit(error(127)) // missings not allowed
    rgb_M = M * 100
    rgb_invM = luinv(rgb_M)
    rgb_xy = J(0,0,.)
    data.stok = J(data.n, 1, `FALSE')   // !!!
}

`T' `MAIN'::rgb_invM(| `RM' invM)
{
    if (args()==0) return(rgb_invM * 100)
    assert_size(invM, 3, 3)
    if (missing(invM)) exit(error(127)) // missings not allowed
    rgb_invM = invM / 100
    rgb_M = luinv(rgb_invM)
    rgb_xy = J(0,0,.)
    data.stok = J(data.n, 1, `FALSE')   // !!!
}

end

* {smcl}
* {marker viewcond}{bf:CIECAM02 viewing conditions} {hline}
* Source:
*   Luo, M.R., C. Li (2013). CIECAM02 and its recent developments. P 19-58 in: 
*   C. Fernandez-Maloigne (ed). Advanced color image processing and analysis. 
*   New York: Springer. {browse "https://doi.org/10.1007/978-1-4419-6190-7_2"}
* {asis}

mata:

`T' `MAIN'::viewcond(| `TV' opt1, `RS' LA, `TV' F, `RS' c, `RS' Nc)
{
    `RS' Yb
    
    if (args()==0) return((C02.Yb, C02.LA, C02.F, C02.c, C02.Nc))
    if (args()==1) {
        if (length(opt1)==0 | opt1==.) Yb = .
        else if (isstring(opt1)) {
            Yb = viewcond_parsenum(gettok(opt1, opt1))
            LA = viewcond_parsenum(gettok(opt1, opt1))
            F  = opt1
        }
        else {
            if (length(opt1)!=5) exit(error(503)) // wrong size
            Yb = opt1[1]; LA = opt1[2]; F = opt1[(3,4,5)]; 
        }
    }
    else if (args()==5) {; Yb = opt1; F = (F, c, Nc); }
    else if (args()!=3) _error(3001) // wrong number of args
    else Yb = opt1
    C02.Yb = (Yb<. ? Yb : 20)
    C02.LA = (LA<. ? LA : (64/pi())/5)
    surround(F)
    C02.set = 0 // reset CIECAM02 containers
}

`RS' `MAIN'::viewcond_parsenum(`SS' s)
{
    `RS' r

    if (s=="" | s==".") return(.)
    r = strtoreal(s)
    if (missing(r)) {
        display("{err}'" + s + "' not allowed")
        exit(3498)
    }
    return(r)
}

void `MAIN'::surround(`TV' S0)
{
    `RV' S
    
    if      (length(S0)==0) S = surround_get("")
    else if (S0==.)         S = surround_get("")
    else if (isstring(S0))  S = surround_get(S0)
    else                    S = S0
    if (missing(S)) exit(error(127))   // missings not allowed
    if (length(S)!=3) exit(error(503)) // wrong size
    C02.F = S[1]; C02.c = S[2]; C02.Nc = S[3]
}

`RR' `MAIN'::surround_get(`SS' S0)
{
    `SS' S
    `RR' FcNc

    // presets
    S = strlower(S0)               /*  F   c    Nc  */
    if (smatch(S, "average")) return(( 1, .69 ,  1))
    if (smatch(S, "dim"))     return((.9, .59 , .9))
    if (smatch(S, "dark"))    return((.8, .525, .8))
    // custom values
    FcNc = strtoreal(tokens(S0))
    if (length(FcNc)!=3 | missing(FcNc)) {
        display("{err}surround '" + S0 + "' not allowed")
        exit(3498)
    }
    return(FcNc)
}

`T' `MAIN'::ucscoefs(| `TV' S0, `RS' c1, `RS' c2)
{
    `RV' S
    
    if (args()==0) return(C02.KLc1c2)
    if (args()==3)          S = (S0, c1, c2)
    else if (args()==2)     _error(3001)   // wrong number of args
    else if (length(S0)==0) S = ucscoefs_get("")
    else if (S0==.)         S = ucscoefs_get("")
    else if (isstring(S0))  S = ucscoefs_get(S0)
    else                    S = S0
    if (missing(S)) exit(error(127))   // missings not allowed
    if (length(S)!=3) exit(error(503)) // wrong size
    C02.KLc1c2 = S
}

`RR' `MAIN'::ucscoefs_get(`SS' S0)
{
    `SS' S
    `RR' KLc1c2

    // presets
    S = strlower(S0)          /*  KL    c1     c2   */
    if (smatch(S ,"ucs")) return(1   , 0.007, 0.0228)
    if (smatch(S ,"lcd")) return(0.77, 0.007, 0.0053)
    if (smatch(S ,"scd")) return(1.24, 0.007, 0.0363)
    // custom values
    KLc1c2 = strtoreal(tokens(S0))
    if (length(KLc1c2)!=3 | missing(KLc1c2)) {
        display("{err}ucscoefs '" + S0 + "' not allowed")
        exit(3498)
    }
    return(KLc1c2)
}

end

* {smcl}
* {marker chadapt}{bf:Chromatic adaption} {hline}
* Source: {browse "http://www.brucelindbloom.com/Eqn_ChromAdapt.html"}
* {asis}

mata:

`RM' `MAIN'::XYZ_to_XYZ(`RM' xyz, `TV' from, `TV' to)
{
    `Int' i
    `RS'  d
    `RC'  S, D
    `RM'  XYZ, M
    
    assert_cols(xyz, 3)
    S = _white(from)'; D = _white(to)'
    XYZ = xyz
    M = tmatrix(chadapt)
    if (chadapt=="CAT02") {
        d = __clip(C02.F * (1 - (1/3.6) * exp((-C02.LA - 42)/92)), 0, 1)
        M = luinv(M) * ((d * (S[2]:/D[2]) :* (M * D) :/ (M * S) :+ 1 :- d) :* M)
    }
    else  M = luinv(M) * (((M * D) :/ (M * S)) :* M)
        // equivalent to: luinv(M) * diag((M * D) :/ (M * S)) * M
    _transpose(M)
    for (i=rows(XYZ); i; i--) XYZ[i,] = XYZ[i,] * M
    return(XYZ)
}

`T' `MAIN'::chadapt(| `SS' S0)
{
    `SS' S
    
    if (args()==0) return(chadapt)
    S = strlower(S0)
    if      (smatch(S, "Bfd"))      chadapt = S
    else if (smatch(S, "identity")) chadapt = S 
    else if (smatch(S, "vKries"))   chadapt = S 
    else if (smatch(S, "CAT02"))    chadapt = S 
    else {
        display("{err}method '" + S0 + "' not allowed")
        exit(3498)
    }
}

`RM' `MAIN'::tmatrix(| `SS' mname0) 
{
    `SS' mname
    
    mname = strlower(mname0)
    if (smatch(mname, "Bfd")) {        // Bradford
        return(( 0.8951 ,  0.2664 , -0.1614 ) \
               (-0.7502 ,  1.7135 ,  0.0367 ) \
               ( 0.0389 , -0.0685 ,  1.0296 ))
    }
    if (smatch(mname, "identity")) {   // XYZ Scaling
        return(I(3))
    }
    if (smatch(mname, "vKries")) {     // Von Kries
        return(( 0.40024,  0.70760, -0.08081) \
               (-0.22630,  1.16532,  0.04570) \
               ( 0      ,  0      ,  0.91822))
    }
    if (smatch(mname, "CAT02")) {      // CAT02
        return(( 0.7328 ,  0.4296 , -0.1624 ) \
               (-0.7036 ,  1.6975 ,  0.0061 ) \
               ( 0.0030 ,  0.0136 ,  0.9834 ))
    }
    if (smatch(mname, "HPE")) {        // Hunt-Pointer-Estevez
        return(( 0.38971,  0.68898, -0.07868) \
               (-0.22981,  1.18340,  0.04641) \
               ( 0      ,  0      ,  1      ))
    }
    display("{err}tmatrix '"+ mname0 +  "' not found")
    exit(3499)
}

end

* {smcl}
* {marker string}{bf:String input/output (Stata interface)} {hline}
* {asis}

mata:

`SS' `MAIN'::cvalid(| `SS' c0)
{
    `SS' c
    
    S = &data1 // (use temporary data object)
    if (parse_split(c0)) return("")
    if (parse_convert()) return("")
    c = Names_get()
    if (c=="") c = Colors_get(0)
    return(c)
}

`T' `MAIN'::colors(| `TS' o1, `SS' o2)
{
    // get
    if (args()==0 | (args()==1 & isstring(o1)==0)) {
        S = &data
        if (args()==0) return(colors_get(`FALSE'))
        return(colors_get(o1))
    }
    // set
    S = &data1
    colors_set(o1, o2)
    replacedata()
}

void `MAIN'::add_colors(`SS' c, | `SS' wchar)
{
    S = &data1
    colors_set(c, wchar)
    appenddata()
}

`SS' `MAIN'::colors_added(| `Bool' rgbforce)
{
    S = &data1
    copyadded()
    if (args()==0) return(colors_get(`FALSE'))
    return(colors_get(rgbforce))
}

`Int' `MAIN'::_colors(`SS' c, | `SS' wchar)
{
    `Int' i
    
    S = &data1
    i = _colors_set(c, wchar)
    if (i==0) replacedata()
    return(i)
}

`Int' `MAIN'::_add_colors(`SS' c, | `SS' wchar)
{
    `Int' i
    
    S = &data1
    i = _colors_set(c, wchar)
    if (i==0) appenddata()
    return(i)
}

`T' `MAIN'::Colors(| `TV' o1)
{
    // get
    if (args()==0 | isstring(o1)==0) {
        S = &data
        if (args()==0) return(Colors_get(`FALSE'))
        return(Colors_get(o1))
    }
    // set
    S = &data1
    Colors_set(o1)
    replacedata()
}

void `MAIN'::add_Colors(`SV' C)
{
    S = &data1
    Colors_set(C)
    appenddata()
}

`SC' `MAIN'::Colors_added(| `Bool' rgbforce)
{
    S = &data1
    copyadded()
    if (args()==0) return(Colors_get(`FALSE'))
    return(Colors_get(rgbforce))
}

`Int' `MAIN'::_Colors(`SV' C)
{
    `Int' i
    
    S = &data1
    i = _Colors_set(C)
    if (i==0) replacedata()
    return(i)
}

`Int' `MAIN'::_add_Colors(`SV' C)
{
    `Int' i
    
    S = &data1
    i = _Colors_set(C)
    if (i==0) appenddata()
    return(i)
}

`SS' `MAIN'::colors_get(`Bool' rgbforce)
{
    `Int' i
    `SC'  C

     C = Colors_get(rgbforce)
     for (i = S->n; i; i--) {
         if (strpos(C[i], " ")) C[i] = `"""' + C[i] + `"""'
     }
     return(invtokens(C'))
}

`SC' `MAIN'::Colors_get(`Bool' rgbforce)
{
    return(_Colors_get(rgbforce,
        _get("RGB"),
        S->alpha,
        S->intensity,
        S->names,
        S->stok))
}

`SC' `MAIN'::_Colors_get(`Bool' rgbforce, `RM' RGB, `RC' alpha, 
    `RC' intensity, `SC' names, `RC' stok)
{
    `Int' i
    `SC'  C
     
     i = rows(RGB)
     C = J(i, 1, "")
     for (; i; i--) {
         if (rgbforce)             C[i] = invtokens(strofreal(RGB[i,]))
         else if (stok[i]==`TRUE') C[i] = names[i]
         else                      C[i] = invtokens(strofreal(RGB[i,]))
         if (alpha[i]<.)           C[i] = C[i] + "%" + strofreal(alpha[i]*100)
         if (intensity[i]<.)       C[i] = C[i] + "*" + strofreal(intensity[i])
     }
     return(C)
}

void `MAIN'::colors_set(`SS' c, `SS' wchar) Colors_set(_tokens(c, wchar))

void `MAIN'::Colors_set(`SV' C)
{
    `Int' i
    
    i = parse_split(C)
    if (i==0) i = parse_convert()
    if (i) {
        display("{err}color specification '" + C[i] + "' is invalid")
        exit(3498)
    }
}

`Int' `MAIN'::_colors_set(`SS' c, `SS' wchar) return(_Colors_set(_tokens(c, wchar)))

`Int' `MAIN'::_Colors_set(`SV' C)
{
    `Int' i
    
    i = parse_split(C)
    if (i==0) i = parse_convert()
    return(i)
}

`Int' `MAIN'::parse_split(`SV' C)
{
    rgb_set(J(length(C), 3, .))
    return(_parse_split(C, S->alpha, S->intensity, S->info))
}

`Int' `MAIN'::_parse_split(`SV' C, `RC' alpha, `RC' intensity, `SC' info)
{
    `Int' n, i
    `SS'  tok
    `SR'  pchars
    `T'   t
    
    pchars = ("%","*")
    n = length(C)
    t = tokeninit("", pchars, "")
    for (i=1; i<=n; i++) {
        tokenset(t, C[i])
        tok = strtrim(tokenget(t))
        if (anyof(pchars, tok)) {
            info[i] = "_NULL_" // %... or *... without color
        }
        else {
            if (tok=="_NULL_") return(i)
            if (substr(tok,1,2)=="..") {
                if (i<n)  return(1) // ".." and "..." only allowed at end
                if (n==1) return(1)
            }
            else if (substr(tok,1,1)=="=") {
                if (i==1) return(1) // "=" not allowed as first element
                C[i] = C[i-1] // copy previous color
                i--
                continue
            }
            info[i] = tok
            if ((tok = tokenget(t))=="") continue
        }
        if (tok=="%") {
            alpha[i] = strtoreal(tokenget(t))/100
            if (alpha[i]<0 | alpha[i]>1) return(i)
            if ((tok = tokenget(t))=="") continue
            if (tok=="*") {
                intensity[i] = strtoreal(tokenget(t))
                if (intensity[i]<0 | intensity[i]>255) return(i)
            }
        }
        else if (tok=="*") {
            intensity[i] = strtoreal(tokenget(t))
            if (intensity[i]<0 | intensity[i]>255) return(i)
            if ((tok = tokenget(t))=="") continue
            if (tok=="%") {
                alpha[i] = strtoreal(tokenget(t))/100
                if (alpha[i]<0 | alpha[i]>1) return(i)
            }
        }
        else return(i)
        if (tokenrest(t)=="") continue
        return(i)
    }
    // done
    return(0)
}

`Int' `MAIN'::parse_convert()
{
    return(_parse_convert(S->n, S->RGB, S->alpha, S->names, S->info, S->stok))
}

`Int' `MAIN'::_parse_convert(`Int' r, `RM' RGB, `RC' alpha, `SC' names,
    `SC' info, `RC' stok)
{
    `Int'  i, l
    `RR'   TMP
    `SR'   tok
    `SS'   t
    `SC'   type
    `IntC' p

    type = J(r, 1, "")
    for (i=1; i<=r; i++) {
        tok = info[i]
        if (tok=="_NULL_") { // %... or *... without color
            info[i] = ""
            stok[i] = `TRUE'
            RGB[i,] = J(1,3,0)
            continue
        }
        if (substr(tok,1,1)=="#") { // HEX color
            RGB[i,] = _HEX_to_RGB(tok)/255
            if (missing(RGB[i,])) return(i)
            continue
        }
        tok = tokens(tok)
        l = length(tok)
        if (l==0) return(i)
        if (l==2) return(i)
        if (l==1) { // named color
            if (parse_named(tok, i, RGB, info, stok)) return(i)
            names[i] = tok
            continue
        }
        if (l==3) { // RGB [0-255]
            TMP = strtoreal(tok)
            if (_clip(round(TMP),0,255)!=TMP) return(i)
            RGB[i,] = TMP/255
            if (missing(RGB[i,])) return(i)
            info[i] = ""
            continue
        }
        if (l==4) { // check whether CMYK
            if (strtoreal(tok[1])<.) { 
                TMP = strtoreal(tok)
                if (all(TMP:<=1)) { // CMYK [0-1]
                    if (any(TMP:<0)) return(i)
                    RGB[i,] = _CMYK1_to_RGB1(TMP)
                }
                else {              // CMYK [0-255]
                    if (_clip(round(TMP),0,255)!=TMP) return(i)
                    RGB[i,] = _CMYK1_to_RGB1(TMP/255) 
                }
                if (missing(RGB[i,])) return(i)
                continue
            }
        }
        t = strlower(tok[1]) // check whether CMYK
        if (t==substr("cmyk1", 1, max((2, strlen(t))))) {
            if (l!=5) return(i)
            TMP = strtoreal(tok[|2 \ .|])
            if (t=="cmyk1") RGB[i,] = _CMYK1_to_RGB1(TMP)     // CMYK [0-1]
            else            RGB[i,] = _CMYK1_to_RGB1(TMP/255) // CMYK [0-255]
            if (missing(RGB[i,])) return(i)
            continue
        }
        if (l==5) { // check whether RGBA/RGBA1
            if (t=="rgba" | t=="rgba1") {
                TMP = strtoreal(tok[5])
                if (TMP<0 | TMP>1) return(i)
                if (alpha[i]<.) {
                    display("{err}opacity not allowed with RGBA")
                    exit(3498)
                }
                alpha[i] = TMP
                if (t=="rgba") RGB[i,] = strtoreal(tok[|2 \ 4|])/255
                else           RGB[i,] = strtoreal(tok[|2 \ 4|])
                continue
            }
        }
        type[i] = invtokens(tok[|1 \ l-3|])   // get color space info
        if (type[i]=="") return(i)
        RGB[i,] = strtoreal(tok[|l-3+1 \ .|]) // get value (last 3 elements)
        if (missing(RGB[i,])) return(i)
    }
    // convert remaining colors
    for (i=r; i; i--) {
        t = type[i]
        if (t=="") continue
        if (convert_parse(tok, t, 0)) return(i)
        p = ::select(1::r, type:==t)
        RGB[p,]  = convert(RGB[p,], t, "RGB1")
        type[p,] = J(length(p), 1, "")
    }
    // done
    return(0)
}

`Bool' `MAIN'::parse_named(`SS' s, `Int' i, `RM' RGB, `SC' info, `RC' stok)
{
    `SR' c
    `RR' rgb
    
    // get color from dictionary
    c = parse_namedcolor(s) // returns "" if not found
    if (c[1]!="") { // skip system colors that have not yet been imported
        rgb = _HEX_to_RGB(c[1])
        if (missing(rgb)) return(1) // invalid HEX code
        if (c[2]!="") {
            info[i] = ""
            stok[i] = `TRUE'
        }
        else info[i] = c[1]
        RGB[i,] = rgb/255
        return(0)
    }
    // get color from color-<name>.style; this includes Stata's system colors
    c = parse_stcolorstyle(s) // returns "" if not found 
    if (c!="") {
        rgb = strtoreal(tokens(c))
        if (length(rgb)!=3) return(1) // invalid RGB code
        info[i] = ""
        stok[i] = `TRUE'
        RGB[i,] = rgb/255
        namedcolors->put(s, (_RGB_to_HEX(rgb),"1")) // "1" => system color
        return(0)
    }
    // color not found
    return(1)
}

`SR' `MAIN'::parse_namedcolor(`SS' s0)
{
    `SS' s
    `SR' c
    
    if (namedcolors==NULL) namedcolorindex()
    c = namedcolors->get(s0) // only finds exact match, including case
    if (c=="") {
        s = findkey(s0, namedcolors->keys())
        if (s!="") {
            s0 = s
            c = namedcolors->get(s0)
        }
    }
    return(c)
}

`SS' `MAIN'::parse_stcolorstyle(`SS' s) // read RGB from color-<name>.style
{   
    `SS' fn, dir, basename, line
    `RS' fh
    `SM' EOF
    
    pragma unset dir
    pragma unset basename

    // look for color-<name>.style along adopath
    fn = findfile("color-"+s+".style")
    if (fn=="") return("")

    // findfile() is not case sensitive, but -graph- is; must check case
    pathsplit(fn, dir, basename) 
    if (length(dir(dir, "files", basename))==0) return("") // no match

    // read RGB from file
    fh  = fopen(fn, "r")
    EOF = J(0, 0, "")
    while ((line=fget(fh))!=EOF) {
        line = strtrim(stritrim(line))
        if (substr(line, 1, 8)=="set rgb ") {
            line = tokens(substr(line, 9, .))
            if (length(line)!=1) continue // invalid
            fclose(fh)
            return(line)
        }
    }
    fclose(fh)
    return("") // no valid color definition found
}

`T' `MAIN'::names(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data
        return(names_get())
    }
    // set
    S = &data1
    copydata()
    names_set(o1, o2)
    replacedata()
}

`T' `MAIN'::names_added(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data1
        copyadded()
        return(names_get())
    }
    // set
    S = &data1
    copyadded()
    names_set(o1, o2)
    updatedata()
}

`T' `MAIN'::Names(| `SV' o1)
{
    // get
    if (args()==0) {
        S = &data
        return(Names_get())
    }
    // set
    S = &data1
    copydata()
    Names_set(o1)
    replacedata()
}

`T' `MAIN'::Names_added(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data1
        copyadded()
        return(Names_get())
    }
    // set
    S = &data1
    copyadded()
    Names_set(o1, o2)
    updatedata()
}

`T' `MAIN'::info(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data
        return(info_get())
    }
    // set
    S = &data1
    copydata()
    info_set(o1, o2)
    replacedata()
}

`T' `MAIN'::info_added(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data1
        copyadded()
        return(info_get())
    }
    // set
    S = &data1
    copyadded()
    info_set(o1, o2)
    updatedata()
}

`T' `MAIN'::Info(| `SV' o1)
{
    // get
    if (args()==0) {
        S = &data
        return(Info_get())
    }
    // set
    S = &data1
    copydata()
    Info_set(o1)
    replacedata()
}

`T' `MAIN'::Info_added(| `SS' o1, `SS' o2)
{
    // get
    if (args()==0) {
        S = &data1
        copyadded()
        return(Info_get())
    }
    // set
    S = &data1
    copyadded()
    Info_set(o1, o2)
    updatedata()
}

`SS' `MAIN'::names_get()
{
    `Int' i
    `SC'  C
     
     C = S->names
     if (allof(C, "")) return("")
     for (i = rows(C); i; i--) {
         if      (C[i]=="")          C[i] = `""""'
         else if (strpos(C[i], " ")) C[i] = `"""' + C[i] + `"""'
     }
     return(invtokens(C'))
}

`SC' `MAIN'::Names_get() return(S->names)

void `MAIN'::names_set(`SS' c, `SS' wchar) Names_set(_tokens(c, wchar))

void `MAIN'::Names_set(`SV' C) _Names_set(C, S->n, S->names)

void `MAIN'::_Names_set(`SV' C, `Int' n, `SC' names)
{
    `Int' i
    
    for (i = min((length(C), n)); i; i--) names[i] = C[i]
}

`SS' `MAIN'::info_get()
{
    `Int' i
    `SC'  C
     
     C = S->info
     if (allof(C, "")) return("")
     for (i = rows(C); i; i--) {
         if      (C[i]=="")          C[i] = `""""'
         else if (strpos(C[i], " ")) C[i] = `"""' + C[i] + `"""'
     }
     return(invtokens(C'))
}

`SC' `MAIN'::Info_get() return(S->info)

void `MAIN'::info_set(`SS' c, `SS' wchar) Info_set(_tokens(c, wchar))

void `MAIN'::Info_set(`SV' C) _Info_set(C, S->n, S->info)

void `MAIN'::_Info_set(`SV' C, `Int' n, `SC' info)
{
    `Int' i
    
    for (i = min((length(C), n)); i; i--) info[i] = C[i]
}

end

* {smcl}
* {marker set}{bf:Set or retrieve colors} {hline}
* {asis}

mata:

void `MAIN'::set(`T' C, | `SS' space)
{
    S = &data1
    _set(C, space)
    replacedata()
}

void `MAIN'::add(`T' C, | `SS' space)
{
    S = &data1
    _set(C, space)
    appenddata()
}

void `MAIN'::reset(`T' C, | `SS' space, `IntV' p)
{
    S = &data1
    copydata()
    _reset(C, space, p)
    replacedata()
}

void `MAIN'::reset_added(`T' C, | `SS' space, `IntV' p)
{
    S = &data1
    copyadded()
    _reset(C, space, p)
    updatedata()
}

`TM' `MAIN'::get(| `SS' space)
{
    S = &data
    return(_get(space))
}

`TM' `MAIN'::get_added(| `SS' space)
{
    S = &data1
    copyadded()
    return(_get(space))
}

void `MAIN'::_set(`T' C, `SS' space)             __set(C, space, 0)

void `MAIN'::_reset(`T' C, `SS' space, `IntV' p) __set(C, space, 1, p)

void `MAIN'::__set(`T' C, `SS' space, `Bool' reset, | `IntV' p0)
{
    `SR'   s
    `RC'   a
    `IntV' p
    
    // preprocess p
    if (reset) {
        p = p0
        if (p==.) p = J(1,0,.)
        else {
            p = (sign(p):!=-1):*p :+ (sign(p):==-1):*((S->n):+1:+p)
            if (any(p:<1 :| p:>(S->n))) {
                display("{err}p contains invalid indices")
                exit(3300)
            }
        }
    }
    // set colors
    s = strtrim(strlower(space))
    if (s=="rgba" | s=="rgba1") {
        if (s=="rgba") s = "RGB"
        else           s = "RGB1"
        assert_cols(C, 4)
        a = C[,4]
        if (any((a:>1) :| (a:<0))) {
            display("{err}alpha must be in [0,1]")
            exit(3300)
        }
        if (reset) rgb_reset(convert(C[,(1,2,3)], s, "RGB1"), p)
        else       rgb_set(convert(C[,(1,2,3)], s, "RGB1"))
        if (reset) {
            if (length(p)) S->alpha[p,] = C[,4]
            else           S->alpha = C[,4]
        }
        else S->alpha = C[,4]
        return
    }
    if (reset) rgb_reset(convert(C, space, "RGB1"), p)
    else       rgb_set(convert(C, space, "RGB1"))
    // generate info
    if (rows(C)==0) return
    (void) convert_parse(s, space, 0) // will replace s
    if      (s[1]=="RGB")   return
    if      (s[1]=="RGB1")  return
    if      (s[1]=="HEX")   info_reset("", C, "", p)
    else if (s[1]=="CMYK")  info_reset("", C, "%9.0f", p)
    else if (s[1]=="CMYK1") info_reset("", C, "%9.3g", p)
    else if (anyof(("lRGB", "XYZ1", "xyY", "xyY1", "HSV", "HSL"), s[1]))
                            info_reset(s[1], C, "%9.3g", p)
    else if (s[1]=="CAM02") info_reset(s[2]!="" ? s[2] : "CAM02", C, "%9.0f", p)
    else                    info_reset(s[1], C, "%9.0f", p)
}

void `MAIN'::rgb_set(`RM' rgb)
{
    `Int' n
    
    n = rows(rgb)
    assert_cols(rgb, 3)
    S->n         = n
    S->RGB       = rgb
    S->alpha     = J(n, 1, .)
    S->intensity = J(n, 1, .)
    S->stok      = J(n, 1, `FALSE')
    S->names     = J(n, 1, "")
    S->info      = J(n, 1, "")
    S->isip      = `FALSE'
}

void `MAIN'::rgb_reset(`RM' rgb, | `IntV' p)
{
    if (length(p)==0) {
        assert_size(rgb, S->n, 3)
        S->RGB   = rgb
        S->names = J(rows(rgb), 1, "")
        S->info  = J(rows(rgb), 1, "")
        S->stok  = J(rows(rgb), 1, `FALSE')
        return
    }
    assert_size(rgb, length(p), 3)
    S->RGB[p,]   = rgb
    S->names[p,] = J(length(p), 1, "")
    S->info[p,]  = J(length(p), 1, "")
    S->stok[p,]  = J(length(p), 1, `FALSE')
}

void `MAIN'::info_reset(`SS' c, `T' C, | `SS' fmt, `IntV' p)
{
    if (length(p)==0) S->info     = _info_reset(c, C, fmt)
    else              S->info[p,] = _info_reset(c, C, fmt)
}

`SC' `MAIN'::_info_reset(`SS' c, `T' C, `SS' fmt)
{
    `Int' i
    `SC'  info
    
    if (isstring(C)) {
        info = J(length(C), 1, (c!="" ? c + " " : ""))
        if (cols(C)!=1) info = info + C'
        else            info = info + C
        return(info)
    }
    info = J(rows(C), 1, (c!="" ? c + " " : ""))
    for (i=1; i<=cols(C); i++) {
        if (i>1) info = info :+ " "
        info = info + strofreal(C[,i], fmt)
    }
    return(info)
}

`TM' `MAIN'::_get(| `SS' space)
{
    `SS' s
    
    s = strtrim(strlower(space))
    if (s=="rgba")  return((convert(S->RGB, "RGB1", "RGB"), editmissing(S->alpha, 1)))
    if (s=="rgba1") return((S->RGB, editmissing(S->alpha, 1)))
    return(convert(S->RGB, "RGB1", space))
}

end

* {smcl}
* {marker opacity}{bf:Set or retrieve opacity and intensity} {hline}
* {asis}

foreach f in opacity alpha intensity {
    mata `T' `MAIN'::`f'(| `RV' O, `RS' noreplace)
    {
        // get
        if (args()==0) {
            S = &data
            return(`f'_get())
        }
        // set
        if (args()<2) noreplace = `FALSE'
        S = &data1
        copydata()
        `f'_set(O, noreplace)
        replacedata()
    }
    mata void `MAIN'::add_`f'(`RV' O, | `RS' noreplace)
    {
        if (args()<2) noreplace = `FALSE'
        S = &data1
        copydata()
        `f'_set(O, noreplace)
        appenddata()
    }
    mata `T' `MAIN'::`f'_added(| `RV' O, `RS' noreplace)
    {
        // get
        if (args()==0) {
            S = &data1
            copyadded()
            return(`f'_get())
        }
        // set
        if (args()<2) noreplace = `FALSE'
        S = &data1
        copyadded()
        `f'_set(O, noreplace)
        updatedata()
    }
    mata void `MAIN'::add_`f'_added(`RV' O, | `RS' noreplace)
    {
        if (args()<2) noreplace = `FALSE'
        S = &data1
        copyadded()
        `f'_set(O, noreplace)
        appenddata()
    }
}
mata:

`RC' `MAIN'::opacity_get() return(S->alpha * 100)

void `MAIN'::opacity_set(`RV' O, `RS' noreplace)
{
    if (length(O)==0) return
    if (any( ((O:<.):&(O:>100)) :| (O:<0) )) {
        display("{err}opacity must be in [0,100]")
        exit(3300)
    }
    _alpha(O/100, noreplace, S->n, S->alpha)
}

`RC' `MAIN'::alpha_get() return(S->alpha)

void `MAIN'::alpha_set(`RV' O, `RS' noreplace)
{
    if (length(O)==0) return
    if (any( ((O:<.):&(O:>1)) :| (O:<0) )) {
        display("{err}alpha must be in [0,1]")
        exit(3300)
    }
    _alpha(O, noreplace, S->n, S->alpha)
}

void `MAIN'::_alpha(`RV' A0, `RS' noreplace, `RS' n, `RC' alpha)
{
    `Int' i
    `RC'  A
    
    if (cols(A0)>1) A = A0'
    else            A = A0
    if      (rows(A) < n) A = colrecycle(A, n)
    else if (rows(A) > n) _recycle(rows(A))
    if (noreplace) {
        for (i=n; i; i--) {
            if (alpha[i]<.) A[i] = alpha[i]
        }
    }
    alpha = A
}

`RC' `MAIN'::intensity_get() return(S->intensity)

void `MAIN'::intensity_set(| `RV' O, `RS' noreplace)
{
    if (length(O)==0) return
    if (any( ((O:<.):&(O:>255)) :| (O:<0) )) {
        display("{err}intensity multiplier must be in [0,255]")
        exit(3300)
    }
    _intensity(O, noreplace, S->n, S->intensity)
}

void `MAIN'::_intensity(`RV' I0, `RS' noreplace, `RS' n, `RC' intensity)
{
    `Int' i
    `RC'  I
    
    if (cols(I0)>1) I = I0'
    else            I = I0
    if      (rows(I) < n) I = colrecycle(I, n)
    else if (rows(I) > n) _recycle(rows(I))
    if (noreplace) {
        for (i=n; i; i--) {
            if (intensity[i]<.) I[i] = intensity[i]
        }
    }
    intensity = I
}

end

* {smcl}
* {marker modify}{bf:Color modification} {hline}
* {asis}

foreach f in recycle select drop order reverse shift ipolate mix intensify ///
    Intensify saturate luminate gray cvd {
    mata void `MAIN'::`f'(| `T' o1, `T' o2, `T' o3, `T' o4, `T' o5, `T' o6)
    {
        S = &data1
        copydata()
        if      (args()==0) _`f'()
        else if (args()==1) _`f'(o1)
        else if (args()==2) _`f'(o1, o2)
        else if (args()==3) _`f'(o1, o2, o3)
        else if (args()==4) _`f'(o1, o2, o3, o4)
        else if (args()==5) _`f'(o1, o2, o3, o4, o5)
        else                _`f'(o1, o2, o3, o4, o5, o6)
        replacedata()
    }
    mata void `MAIN'::add_`f'(| `T' o1, `T' o2, `T' o3, `T' o4, `T' o5, `T' o6)
    {
        S = &data1
        copydata()
        if      (args()==0) _`f'()
        else if (args()==1) _`f'(o1)
        else if (args()==2) _`f'(o1, o2)
        else if (args()==3) _`f'(o1, o2, o3)
        else if (args()==4) _`f'(o1, o2, o3, o4)
        else if (args()==5) _`f'(o1, o2, o3, o4, o5)
        else                _`f'(o1, o2, o3, o4, o5, o6)
        appenddata()
    }
    mata void `MAIN'::`f'_added(| `T' o1, `T' o2, `T' o3, `T' o4, `T' o5, `T' o6)
    {
        S = &data1
        copyadded()
        if      (args()==0) _`f'()
        else if (args()==1) _`f'(o1)
        else if (args()==2) _`f'(o1, o2)
        else if (args()==3) _`f'(o1, o2, o3)
        else if (args()==4) _`f'(o1, o2, o3, o4)
        else if (args()==5) _`f'(o1, o2, o3, o4, o5)
        else                _`f'(o1, o2, o3, o4, o5, o6)
        updatedata()
    }
    mata void `MAIN'::add_`f'_added(| `T' o1, `T' o2, `T' o3, `T' o4, `T' o5, `T' o6)
    {
        S = &data1
        copyadded()
        if      (args()==0) _`f'()
        else if (args()==1) _`f'(o1)
        else if (args()==2) _`f'(o1, o2)
        else if (args()==3) _`f'(o1, o2, o3)
        else if (args()==4) _`f'(o1, o2, o3, o4)
        else if (args()==5) _`f'(o1, o2, o3, o4, o5)
        else                _`f'(o1, o2, o3, o4, o5, o6)
        appenddata()
    }
}

* {smcl}
* {marker order}{bf:- recycle, select, drop, order, shift} {hline}
* {asis}

mata:

void `MAIN'::_recycle(`Int' n)
{
    // skip recycle if n is missing, number of existing colors is zero, or n is
    // equal to number of existing colors 
    if (n>=.) return
    if (S->n==0) return
    if (trunc(n)==(S->n)) return
    // remove colors if n<1
    if (n<1) {
        rgb_set(J(0, 3, .))
        return
    }
    // recycle
    S->RGB       = colrecycle(S->RGB, n)
    S->alpha     = colrecycle(S->alpha, n)
    S->intensity = colrecycle(S->intensity, n)
    S->names     = colrecycle(S->names, n)
    S->info      = colrecycle(S->info, n)
    S->stok      = colrecycle(S->stok, n)
    S->n         = rows(S->RGB)
}

void `MAIN'::_select(`IntV' p0)
{
    `Int'  n
    `IntC' p
    
    n = S->n
    p = p0
    if (cols(p)!=1) _transpose(p)
    p = (sign(p):!=-1):*p :+ (sign(p):==-1):*(n:+1:+p)
    p = ::select(p, p:>=1 :& p:<=n) // may return 0x0
    __select(p)
}

void `MAIN'::__select(`IntM' p)
{
    `Int' n
    
    n = length(p)
    if (n==0) {
        rgb_set(J(0, 3, .))
        return
    }
    S->n         = n
    S->RGB       = S->RGB[p,]
    S->alpha     = S->alpha[p,]
    S->intensity = S->intensity[p,]
    S->names     = S->names[p,]
    S->info      = S->info[p,]
    S->stok      = S->stok[p,]
}

void `MAIN'::_drop(`IntV' p0)
{
    `Int'  n
    `IntC' p, k
    
    n = S->n
    p = p0
    if (cols(p)!=1) _transpose(p)
    p = (sign(p):!=-1):*p :+ (sign(p):==-1):*(n:+1:+p)
    p = ::select(p, p:>=1 :& p:<=n)        // may return 0x0
    if (length(p)==0) return               // nothing to drop
    k = J(n,1,1); k[p,] = J(length(p),1,0) // tag elements to be kept
    p = selectindex(k)
    __select(p)
}

void `MAIN'::_order(`IntV' p0)
{
    `Int'  n
    `IntC' p, rest
    
    n = S->n
    p = p0
    if (cols(p)!=1) _transpose(p)
    p = (sign(p):!=-1):*p :+ (sign(p):==-1):*(n:+1:+p)
    p = ::select(p, p:>=1 :& p:<=n)
    if (length(p)==0) return
    rest = 1::n
    rest[p,] = J(length(p), 1, .)
    rest = ::select(rest, rest:<.)
    if (length(rest)) p = p \ rest
    __select(p)
}

void `MAIN'::_reverse()
{
    `Int'  n

    n = S->n
    if (n<=1) return
    __select(n::1)
}

void `MAIN'::_shift(`Int' k)
{
    `Int'  n
    `IntC' p
    
    if (k>=.) return // do nothing
    if (k==0) return // do nothing
    n = S->n
    p = (1::n) :- trunc(abs(k)>=1 ? k : k*n)
    p = mod(p:-1, n) :+ 1
    __select(p)
}

end

* {smcl}
* {marker ipolate}{bf:- interpolate, mix} {hline}
* {asis}

mata:

void `MAIN'::_ipolate(`Int' n, | `SS' space0,
    `RV' range, `RS' power, `RV' pos, `Bool' pad)
{
    `Int'  jh
    `SS'   space, mask
    `RC'   A, I
    `RM'   C

    if (args()<6) pad = `FALSE'
    // parse space
    space = findkey(gettok(space0, mask=""), SPACES, "Jab")
    if (space=="") {
        display("{err}space '" + space0 + "' not allowed")
        exit(3498)
    }
    // skip interpolation if n is missing or if number of colors is zero
    if (n>=.) return
    if (S->n==0) return
    // remove colors if n<1
    if (n<1) {
        rgb_set(J(0, 3, .))
        return
    }
    // convert RGB1 to interpolation space
    C = _ipolate_get(space, mask, jh=0)
    if (mask!="") space = space + " " + mask
    // get opacity and intensity
    if (any(S->alpha:<.))     A = editmissing(S->alpha, 1)
    else                      A = J(S->n, 0, .)
    if (any(S->intensity:<.)) I = editmissing(S->intensity, 1)
    else                      I = J(S->n, 0, .)
    // interpolate
    if (anyof(("cyclic","circular"), S->pclass))
         C = colipolate_c((C,A,I), n) // ignoring range, power, pos, pad
    else C = colipolate((C,A,I), n, range, power, pos, pad)
    if (length(I)) {; I = C[,cols(C)]; C = C[,1..cols(C)-1]; }
    if (length(A)) {; A = C[,cols(C)]; C = C[,1..cols(C)-1]; }
    // convert back to RGB1
    if (jh) C[,jh] = mod(C[,jh] :+ .5, 360) :- .5
    _set(C, space)
    S->isip = `TRUE'
    // reset opacity and intensity if necessary
    if (length(A)) {
        if (pad) S->alpha = clip(A, 0, 1)
        else     S->alpha = A
    }
    if (length(I)) {
        if (pad) S->intensity = clip(I, 0, 255)
        else     S->intensity = I
    }
}

`RM' `MAIN'::_ipolate_get(`SS' space, `SS' mask, `Int' j)
{
    `RM'   C
    
    C = _get(space + (mask!="" ? " " + mask : ""))
    if (space=="CAM02") {
        if (mask=="") j = 3 // default mask is JCh
        else          j = strpos(mask, "h")
    }
    else j = strpos(strlower(space), "h")
    if (j) C[,j] = _ipolate_halign(C[,j])
    return(C)
}

`RC' `MAIN'::_ipolate_halign(`RC' C)
{   // realigns hues such that for each consecutive pair of colors the
    // shorter distance on the color wheel is used
    `Int' i, j
    `RS'  a, b, c
    
    for(i=(rows(C)-1); i; i--) {
        c = C[i+1]
        if (c>=.) continue
        j = trunc(c/360)
        b = mod(C[i], 360)
        if (b<mod(C[i+1], 360)) {
            a = j*360 + b
            b = (j+1)*360 + b
        }
        else {
            a = (j-1)*360 + b
            b = j*360 + b
        }
        if (abs(a-c)<=abs(b-c)) C[i] = a
        else                    C[i] = b
    }
    return(C)
}

void `MAIN'::_mix(| `SS' space0, `RV' w0)
{
    `Int'  jh
    `SS'   space, mask
    `RC'   w, A, I
    `RM'   C

    // parse space
    space = findkey(gettok(space0, mask=""), SPACES, "Jab")
    if (space=="") {
        display("{err}space '" + space0 + "' not allowed")
        exit(3498)
    }
    // skip mixing if number of colors is zero
    if (S->n==0) return
    // convert RGB1 to interpolation space
    C = _mix_get(space, mask, jh=0)
    if (mask!="") space = space + " " + mask
    // weights
    if (length(w0)==0) w = 1
    else if (w0==.)    w = 1
    else {
        if (cols(w0)>1) w = w0'
        else            w = w0
        if      (rows(w)<(S->n)) w = colrecycle(w, S->n)
        else if (rows(w)>(S->n)) w = w[|1 \ S->n|]
    }
    // get opacity and intensity
    if (any(S->alpha:<.))     A = editmissing(S->alpha, 1)
    else                      A = J(S->n, 0, .)
    if (any(S->intensity:<.)) I = editmissing(S->intensity, 1)
    else                      I = J(S->n, 0, .)
    // average
    C = mean((C,A,I), w)
    if (length(I)) {; I = C[cols(C)]; C = C[,1..cols(C)-1]; }
    if (length(A)) {; A = C[cols(C)]; C = C[,1..cols(C)-1]; }
    // convert back to RGB1
    if (jh) {
        C[jh] = mod(atan2(C[jh], C[cols(C)])*180/pi() + .5, 360) - .5
        C = C[|1 \ cols(C)-1|]
    }
    _set(C, space)
    // reset opacity if necessary
    if (length(A)) S->alpha     = A
    if (length(I)) S->intensity = I
}

`RM' `MAIN'::_mix_get(`SS' space, `SS' mask, `Int' j)
{
    `RM'   C
    
    C = _get(space + (mask!="" ? " " + mask : ""))
    if (space=="CAM02") {
        if (mask=="") j = 3 // default mask is JCh
        else          j = strpos(mask, "h")
    }
    else j = strpos(strlower(space), "h")
    if (j) {
        C[,j] = C[,j] * (pi() / 180)
        C     = C, sin(C[,j])
        C[,j] = cos(C[,j])
    }
    return(C)
}

end

* {smcl}
* {marker intens}{bf:- intensify, saturate, luminate} {hline}
* {asis}

mata:

// intensify(): equivalent to intensity adjustment as implemented in official
// Stata (increase/decrease R, G, and B such that their ratio is maintained)
// 0 <= p < 1 makes color lighter
// 1 <  p <= 255 makes color darker
// output is always within [0,255] and rounded

void `MAIN'::_intensify(`RV' p0)
{
    `Int'   i
    `IntC'  id
    `RC'    p
    `RM'    C
    
    if (length(p0)==0) return
    if (any( ((p0:<.):&(p0:>255)) :| (p0:<0) )) {
        display("{err}intensity multiplier must be in [0,255]")
        exit(3300)
    }
    if (cols(p0)>1) p = p0'
    else            p = p0
    if ((S->n)==0) return
    if      (rows(p)<(S->n)) p = colrecycle(p, S->n)
    else if (rows(p)>(S->n)) _recycle(rows(p))
    if (missing(p)) {
        id = ::select(1::(S->n), p:<.)
        if (length(id)==0) return
        p = p[id]
    }
    C = _get("RGB")
    if (length(id)) C = C[id,]
    for (i=rows(C); i; i--) C[i,] = __intensify(C[i,], p[i])
    _reset(C, "RGB", id)
}

`RR' `MAIN'::__intensify(`RR' C0, `RS' p)
{   // C0 is assumed to be rounded and clipped to [0,255]
    `RS' m
    `RR' C

    if (p==1) return(C0)
    m = (p<0 ? 0 : (p>255 ? 255 : p))
    if (m<1) return(round(C0*m :+ (255*(1-m))))
    m = 1/m
    C = round(C0*m)
    if (any(C0 :& C:==0)) C = round(C0 / min(::select((255,C0), (255,C0):>0)))
    return(C)
}

// Intensify(): intensify() by intensity() and reset intensity()
void `MAIN'::_Intensify()
{
    _intensify(S->intensity)
    S->intensity = J(S->n,1,.)
}

// saturate(): change saturation/chroma
// similar to color.saturate()/color.desaturate() in chroma.js
// source: https://gka.github.io/chroma.js/

void `MAIN'::_saturate(`RV' p0, | `SS' method0, `Bool' level)
{
    `Int'  i
    `IntC' id
    `RC'   p
    `SS'   method
    `RM'   C
    
    if (args()<3) level = `FALSE'
    method = findkey(method0, ("LCh", "HCL", "JCh", "JMh")', "LCh")
    if (method=="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    if (method=="JCh") method = "CAM02 JCh"
    if (length(p0)==0) return
    if (cols(p0)>1) p = p0'
    else            p = p0
    if ((S->n)==0) return
    if      (rows(p)<(S->n)) p = colrecycle(p, S->n)
    else if (rows(p)>(S->n)) _recycle(rows(p))
    if (missing(p)) {
        id = ::select(1::(S->n), p:<.)
        if (length(id)==0) return
        p = p[id]
    }
    C = _get(method)
    if (length(id)) C = C[id,]
    for (i=rows(C); i; i--) {
        if (level) C[i,2] = max((p[i],0))
        else       C[i,2] = max((C[i,2] + p[i], 0))
    }
    _reset(C, method, id)
}

// luminate(): change luminance/brightness
// similar to color.brighten()/color.darken() in chroma.js
// source: https://gka.github.io/chroma.js/

void `MAIN'::_luminate(`RV' p0, | `SS' method0, `Bool' level)
{
    `Int'  i, j
    `IntC' id
    `RC'   p
    `SS'   method
    `RM'   C
    
    if (args()<3) level = `FALSE'
    method = findkey(method0, ("Lab", "LCh", "Luv", "HCL", "JCh", "JMh",
        "Jab")', "JMh")
    if (method=="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    if (method=="JCh") method = "CAM02 JCh"
    if (length(p0)==0) return
    if (cols(p0)>1) p = p0'
    else            p = p0
    if ((S->n)==0) return
    if      (rows(p)<(S->n)) p = colrecycle(p, S->n)
    else if (rows(p)>(S->n)) _recycle(rows(p))
    if (missing(p)) {
        id = ::select(1::(S->n), p:<.)
        if (length(id)==0) return
        p = p[id]
    }
    C = _get(method)
    if (length(id)) C = C[id,]
    if (method=="HCL") j = 3
    else               j = 1
    for (i=rows(C); i; i--) {
        if (level) C[i,j] = max((p[i],0))
        else       C[i,j] = max((C[i,j] + p[i], 0))
    }
    _reset(C, method, id)
}

end

* {smcl}
* {marker gray}{bf:- grayscale conversion} {hline}
* {asis}

mata:

// gray(): reduce luminance towards zero
// note: the method proposed at https://en.wikipedia.org/wiki/Grayscale
// (set all RGB channels to Y of XYZ; and apply gamma) is equivalent to
// method "HCL" or "LCh" with p = 1

void `MAIN'::_gray(| `RV' p0, `SS' method0)
{
    `IntC' id
    `RC'   p
    `SS'   method
    `RM'   C
    
    if (args()==0) p0 = 1
    method = findkey(method0, ("LCh", "HCL", "JCh", "JMh")', "LCh")
    if (method=="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    if (method=="JCh") method = "CAM02 JCh"
    if (length(p0)==0) return
    if (any(p0:<0) | any(p0:>1 :& p0:<.)) {
        display("{err}{it:p} must be within 0 and 1")
        exit(3300)
    }
    if (cols(p0)>1) p = p0'
    else            p = p0
    if ((S->n)==0) return
    if      (rows(p)<(S->n)) p = colrecycle(p, S->n)
    else if (rows(p)>(S->n)) _recycle(rows(p))
    if (missing(p)) {
        id = ::select(1::(S->n), p:<.)
        if (length(id)==0) return
        p = p[id]
    }
    C = _get(method)
    if (length(id)) C = C[id,]
    C[,2] = C[,2] :* (1 :- p)
    _reset(C, method, id)
}

`RM' `MAIN'::GRAY(`RM' C, `SS' space, `RS' p0, `SS' method0)
{
    `RS' p
    `SS' method
    `RM' G
    
    p = (p0<. ? p0 : 1)
    if (p<0 | p>1) {
        display("{err}proportion must be within 0 and 1")
        exit(3300)
    }
    method = findkey(method0, ("LCh", "HCL", "JCh", "JMh")', "LCh")
    if (method=="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    if (method=="JCh") method = "CAM02 JCh"
    if (p==0) return(C) // no conversion needed
    G = convert(C, space, method)
    if (p!=1) G[,2] = G[,2] * (1-p)
    else      G[,2] = J(rows(G), 1, 0)
    return(convert(G, method, space))
}

end

* {smcl}
* {marker cvd}{bf:Color vision deficiency simulation} {hline}
* {asis}

mata:

// cvd(): simulate color vision deficiency 
// Source:
//   Machado, G.M., M.M. Oliveira, L.A.F. Fernandes (2009). A
//   Physiologically-based Model for Simulation of Color Vision Deficiency. IEEE
//   Transactions on Visualization and Computer Graphics 15(6): 1291-1298. 
//   {browse "https://doi.org/10.1109/TVCG.2009.113"}
// Transformation matrices:
//   {browse "http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html"}

void `MAIN'::_cvd(| `RV' p0, `SS' method0)
{
    `Int'  i, m
    `IntC' id
    `RC'   p
    `RS'   pi
    `RM'   C, M
    `SS'   method
    
    if (args()==0) p0 = 1
    method = strlower(method0)
    if      (smatch(method, "deuteranomaly"))  m = 1 // includes ""
    else if (smatch(method, "protanomaly"))    m = 2
    else if (smatch(method, "tritanomaly"))    m = 3
    else {
        display("{err}type '" + method0 + "' not allowed")
        exit(3498)
    }
    if (length(p0)==0) return
    if (any(p0:<0) | any(p0:>1 :& p0:<.)) {
        display("{err}{it:p} must be within 0 and 1")
        exit(3300)
    }
    if (cols(p0)>1) p = p0'
    else            p = p0
    if ((S->n)==0) return
    if      (rows(p)<(S->n)) p = colrecycle(p, S->n)
    else if (rows(p)>(S->n)) _recycle(rows(p))
    if (missing(p)) {
        id = ::select(1::(S->n), p:<.)
        if (length(id)==0) return
        p = p[id]
    }
    C = _get("lRGB")
    if (length(id)) C = C[id,]
    pi = .
    for (i=rows(C); i; i--) {
        if (p[i]!=pi) { // update M only if p changed
            pi = p[i]
            M = _cvd_M(pi, m)'
        }
        C[i,] = C[i,] * M
    }
    _reset(C, "lRGB", id)
}

`RM' `MAIN'::CVD(`RM' C, `SS' space, | `RS' p, `SS' method)
{
    `Int' i
    `RM'  M, CVD
    
    if (p==0) return(C) // no conversion needed
    M = cvd_M(p, method)'
    CVD = convert(C, space, "lRGB")
    for (i=rows(CVD); i; i--) CVD[i,] = CVD[i,] * M
    return(convert(CVD, "lRGB", space))
}

`RM' `MAIN'::cvd_M(| `RS' p0, `SS' method0)
{
    `Int' m
    `RS'  p
    `SS'  method
    
    p = (p0<. ? p0 : 1)
    if (p<0 | p>1) {
        display("{err}severity must be within 0 and 1")
        exit(3300)
    }
    method = strlower(method0)
    if      (smatch(method, "deuteranomaly")) m = 1 // includes ""
    else if (smatch(method, "protanomaly"))   m = 2
    else if (smatch(method, "tritanomaly"))   m = 3
    else {
        display("{err}type '" + method0 + "' not allowed")
        exit(3498)
    }
    return(_cvd_M(p, m))
}

`RM' `MAIN'::_cvd_M(`RS' p, `Int' m)
{
    `Int' a, b
    `RS'  f
    
    f = p/.1
    a = floor(f); b = ceil(f)
    if (a==b) {
        if (m==1) return(cvd_M_d(a))
        if (m==2) return(cvd_M_p(a))
        if (m==3) return(cvd_M_t(a))
    }
    f = f-a
    if (m==1) return( (1-f)*cvd_M_d(a) :+ f*cvd_M_d(b) )
    if (m==2) return( (1-f)*cvd_M_p(a) :+ f*cvd_M_p(b) )
    if (m==3) return( (1-f)*cvd_M_t(a) :+ f*cvd_M_t(b) )
}

`RM' `MAIN'::cvd_M_d(`Int' i)
{
    if (i==0)  return(( 1.000000,  0.000000, -0.000000) \
                      ( 0.000000,  1.000000,  0.000000) \
                      (-0.000000, -0.000000,  1.000000) )
    if (i==1)  return(( 0.866435,  0.177704, -0.044139) \
                      ( 0.049567,  0.939063,  0.011370) \
                      (-0.003453,  0.007233,  0.996220) )
    if (i==2)  return(( 0.760729,  0.319078, -0.079807) \
                      ( 0.090568,  0.889315,  0.020117) \
                      (-0.006027,  0.013325,  0.992702) )
    if (i==3)  return(( 0.675425,  0.433850, -0.109275) \
                      ( 0.125303,  0.847755,  0.026942) \
                      (-0.007950,  0.018572,  0.989378) )
    if (i==4)  return(( 0.605511,  0.528560, -0.134071) \
                      ( 0.155318,  0.812366,  0.032316) \
                      (-0.009376,  0.023176,  0.986200) )
    if (i==5)  return(( 0.547494,  0.607765, -0.155259) \
                      ( 0.181692,  0.781742,  0.036566) \
                      (-0.010410,  0.027275,  0.983136) )
    if (i==6)  return(( 0.498864,  0.674741, -0.173604) \
                      ( 0.205199,  0.754872,  0.039929) \
                      (-0.011131,  0.030969,  0.980162) )
    if (i==7)  return(( 0.457771,  0.731899, -0.189670) \
                      ( 0.226409,  0.731012,  0.042579) \
                      (-0.011595,  0.034333,  0.977261) )
    if (i==8)  return(( 0.422823,  0.781057, -0.203881) \
                      ( 0.245752,  0.709602,  0.044646) \
                      (-0.011843,  0.037423,  0.974421) )
    if (i==9)  return(( 0.392952,  0.823610, -0.216562) \
                      ( 0.263559,  0.690210,  0.046232) \
                      (-0.011910,  0.040281,  0.971630) )
    if (i==10) return(( 0.367322,  0.860646, -0.227968) \
                      ( 0.280085,  0.672501,  0.047413) \
                      (-0.011820,  0.042940,  0.968881) )
}

`RM' `MAIN'::cvd_M_p(`Int' i)
{
    if (i==0)  return(( 1.000000,  0.000000, -0.000000) \
                      ( 0.000000,  1.000000,  0.000000) \
                      (-0.000000, -0.000000,  1.000000) )
    if (i==1)  return(( 0.856167,  0.182038, -0.038205) \
                      ( 0.029342,  0.955115,  0.015544) \
                      (-0.002880, -0.001563,  1.004443) )
    if (i==2)  return(( 0.734766,  0.334872, -0.069637) \
                      ( 0.051840,  0.919198,  0.028963) \
                      (-0.004928, -0.004209,  1.009137) )
    if (i==3)  return(( 0.630323,  0.465641, -0.095964) \
                      ( 0.069181,  0.890046,  0.040773) \
                      (-0.006308, -0.007724,  1.014032) )
    if (i==4)  return(( 0.539009,  0.579343, -0.118352) \
                      ( 0.082546,  0.866121,  0.051332) \
                      (-0.007136, -0.011959,  1.019095) )
    if (i==5)  return(( 0.458064,  0.679578, -0.137642) \
                      ( 0.092785,  0.846313,  0.060902) \
                      (-0.007494, -0.016807,  1.024301) )
    if (i==6)  return(( 0.385450,  0.769005, -0.154455) \
                      ( 0.100526,  0.829802,  0.069673) \
                      (-0.007442, -0.022190,  1.029632) )
    if (i==7)  return(( 0.319627,  0.849633, -0.169261) \
                      ( 0.106241,  0.815969,  0.077790) \
                      (-0.007025, -0.028051,  1.035076) )
    if (i==8)  return(( 0.259411,  0.923008, -0.182420) \
                      ( 0.110296,  0.804340,  0.085364) \
                      (-0.006276, -0.034346,  1.040622) )
    if (i==9)  return(( 0.203876,  0.990338, -0.194214) \
                      ( 0.112975,  0.794542,  0.092483) \
                      (-0.005222, -0.041043,  1.046265) )
    if (i==10) return(( 0.152286,  1.052583, -0.204868) \
                      ( 0.114503,  0.786281,  0.099216) \
                      (-0.003882, -0.048116,  1.051998) )
}

`RM' `MAIN'::cvd_M_t(`Int' i)
{
    if (i==0)  return(( 1.000000,  0.000000, -0.000000) \
                      ( 0.000000,  1.000000,  0.000000) \
                      (-0.000000, -0.000000,  1.000000) )
    if (i==1)  return(( 0.926670,  0.092514, -0.019184) \
                      ( 0.021191,  0.964503,  0.014306) \
                      ( 0.008437,  0.054813,  0.936750) )
    if (i==2)  return(( 0.895720,  0.133330, -0.029050) \
                      ( 0.029997,  0.945400,  0.024603) \
                      ( 0.013027,  0.104707,  0.882266) )
    if (i==3)  return(( 0.905871,  0.127791, -0.033662) \
                      ( 0.026856,  0.941251,  0.031893) \
                      ( 0.013410,  0.148296,  0.838294) )
    if (i==4)  return(( 0.948035,  0.089490, -0.037526) \
                      ( 0.014364,  0.946792,  0.038844) \
                      ( 0.010853,  0.193991,  0.795156) )
    if (i==5)  return(( 1.017277,  0.027029, -0.044306) \
                      (-0.006113,  0.958479,  0.047634) \
                      ( 0.006379,  0.248708,  0.744913) )
    if (i==6)  return(( 1.104996, -0.046633, -0.058363) \
                      (-0.032137,  0.971635,  0.060503) \
                      ( 0.001336,  0.317922,  0.680742) )
    if (i==7)  return(( 1.193214, -0.109812, -0.083402) \
                      (-0.058496,  0.979410,  0.079086) \
                      (-0.002346,  0.403492,  0.598854) )
    if (i==8)  return(( 1.257728, -0.139648, -0.118081) \
                      (-0.078003,  0.975409,  0.102594) \
                      (-0.003316,  0.501214,  0.502102) )
    if (i==9)  return(( 1.278864, -0.125333, -0.153531) \
                      (-0.084748,  0.957674,  0.127074) \
                      (-0.000989,  0.601151,  0.399838) )
    if (i==10) return(( 1.255528, -0.076749, -0.178779) \
                      (-0.078411,  0.930809,  0.147602) \
                      ( 0.004733,  0.691367,  0.303900) )
}

end

* {smcl}
* {marker diff}{bf:Color differences and contrast ratios} {hline}
* {asis}

mata:

// contrast(): compute color contrast
// Contrast Ratio according to Web Content Accessibility Guidelines (WCAG) 2.0
// source: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef

`RC' `MAIN'::contrast(| `IntM' P)
{
    S = &data
    if (args()==0) return(_contrast())
    return(_contrast(P))
}

`RC' `MAIN'::contrast_added(| `IntM' P)
{
    S = &data1
    copyadded()
    if (args()==0) return(_contrast())
    return(_contrast(P))
}

`RC' `MAIN'::_contrast(| `IntM' P0)
{
    `Int'  i, n, p1, p2
    `RS'   l1, l2
    `IntM' P
    `RC'   L, C
    
    if ((n = S->n)<1) return(J(0,1,.))
    if (args()<1 | P0==.) {
        if (n<2) return(J(0,1,.))
        P = ((1::(n-1)), (2::n))
    }
    else P = P0
    assert_cols(P, 2)
    L = _get("XYZ")[,2] :+ 5
    C = J(i = rows(P), 1, .)
    for (; i; i--) {
        p1 = P[i,1]; p2 = P[i,2]
        if (p1<0) p1 = n + 1 + p1
        if (p2<0) p2 = n + 1 + p2
        if (p1<1 | p1>n) continue
        if (p2<1 | p2>n) continue
        l1 = L[p1]; l2 = L[p2]
        C[i] = (l1>l2 ? l1/l2 : l2/l1)
    }
    return(C)
}

// delta(): compute color differences
// sources:
// - https://en.wikipedia.org/wiki/Color_difference
// - http://www.brucelindbloom.com/
// - Luo, M.R., C. Li (2013). CIECAM02 and its recent developments. P 19-58 in: 
//   C. Fernandez-Maloigne (ed). Advanced color image processing and analysis. 
//   New York: Springer. https://doi.org/10.1007/978-1-4419-6190-7_2

`RC' `MAIN'::delta(| `IntM' P, `SS' method, `Bool' noclip)
{
    S = &data
    if (args()==0) return(_delta())
    if (args()==1) return(_delta(P))
    if (args()==2) return(_delta(P, method))
    return(_delta(P, method, noclip))
}

`RC' `MAIN'::delta_added(| `IntM' P, `SS' method, `Bool' noclip)
{
    S = &data1
    copyadded()
    if (args()==0) return(_delta())
    if (args()==1) return(_delta(P))
    if (args()==2) return(_delta(P, method))
    return(_delta(P, method, noclip))
}

`RC' `MAIN'::_delta(| `IntM' P0, `SS' method0, `Bool' noclip)
{
    `SS'   method, coefs, space
    `Int'  n
    `IntM' P
    `RM'   C
    pragma unset coefs

    if (args()<3) noclip = `FALSE'
    // parse method 
    method = findkey(gettok(method0, coefs), ("E76", "E94", "E2000",
        "RGB", "RGB1", "lRGB", "XYZ", "XYZ1", "xyY1", "Lab", "LCh", "Luv",
        "HCL", "JCh", "JMh", "Jab")', ("Jab"))
    if (method=="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    if      (method=="Jab") space = "Jab" + (coefs!="" ? " " + coefs : "")
    else if (method=="JMh") space = "JMh" + (coefs!="" ? " " + coefs : "")
    else if (method=="JCh") space = "CAM02 JCh"
    else if (coefs!="") {
        display("{err}method '" + method0 + "' not allowed")
        exit(3498)
    }
    else {
        if (anyof(("E76","E94","E2000"), method)) space = "Lab"
        else                                      space = method
    }
    // determine positions
    if ((n = S->n)<1) return(J(0,1,.))
    if (args()<1 | P0==.) {
        if (n<2) return(J(0,1,.))
        P = ((1::(n-1)), (2::n))
    }
    else P = P0
    assert_cols(P, 2)
    // get colors
    if (noclip==`TRUE') C = _get(space)
    else                C = convert(clip(_get("lRGB"), 0, 1), "lRGB", space)
    // compute differences
    if (method=="Jab")        return(delta_jab(C, P, coefs))
    else if (method=="E94")   return(delta_E94(C, P))
    else if (method=="E2000") return(delta_E2000(C, P))
    return(delta_euclid(C, P))
}

`RC' `MAIN'::delta_jab(`RM' C, `IntM' P, `SS' coefs)
{
    `Int'  i, n, p1, p2
    `RS'   KL
    `RR'   a, b
    `RC'   D
    
    if (coefs=="") KL = ucscoefs()[1]
    else           KL = ucscoefs_get(coefs)[1]
    n = rows(C)
    D = J(i = rows(P), 1, .)
    for (; i; i--) {
        p1 = P[i,1]; p2 = P[i,2]
        if (p1<0) p1 = n + 1 + p1
        if (p2<0) p2 = n + 1 + p2
        if (p1<1 | p1>n) continue
        if (p2<1 | p2>n) continue
        a = C[p1,]; b = C[p2,]
        D[i] = sqrt(((a[1]-b[1])/KL)^2 + (a[2]-b[2])^2 + (a[3]-b[3])^2)
    }
    return(D)
}

`RC' `MAIN'::delta_E94(`RM' Lab, `IntM' P)
{   // see http://www.brucelindbloom.com/index.html?Eqn_DeltaE_CIE94.html
    // (but using symmetric variant as in Hunt 2004:670)
    `Int'  i, n, p1, p2
    `Int'  dL, dC, dH2, KL, KC, KH, K1, K2, SL, SC, SH, C1, C2
    `RR'   Lab1, Lab2
    `RC'   D

    KL = 1    // textile: 2
    KC = 1
    KH = 1
    K1 = .045 // textile: .048
    K2 = .015 // textile: .014
    SL = 1
    n = rows(Lab)
    D = J(i = rows(P), 1, .)
    for (; i; i--) {
        p1 = P[i,1]; p2 = P[i,2]
        if (p1<0) p1 = n + 1 + p1
        if (p2<0) p2 = n + 1 + p2
        if (p1<1 | p1>n) continue
        if (p2<1 | p2>n) continue
        Lab1 = Lab[p1,]; Lab2 = Lab[p2,]
        C1  = sqrt(sum(Lab1[(2,3)]:^2))
        C2  = sqrt(sum(Lab2[(2,3)]:^2))
        SC  = 1 + K1 * sqrt(C1*C2) // asymmetic: 1 + K1 * C1
        SH  = 1 + K2 * sqrt(C1*C2) // asymmetic: 1 + K2 * C1
        dL  = Lab1[1] - Lab2[1]
        dC  = C1 - C2
        dH2 = sum((Lab1[(2,3)]-Lab2[(2,3)]):^2) - dC^2
        dL  = dL  / (KL*SL)
        dC  = dC  / (KC*SC)
        dH2 = dH2 / (KH*SH)^2
        D[i] = sqrt(dL^2 + dC^2 + dH2)
    }
    return(D)
}

`RC' `MAIN'::delta_E2000(`RM' Lab, `IntM' P)
{   // see http://www.brucelindbloom.com/Eqn_DeltaE_CIE2000.html
    // (need to work in radians: radian = degree * pi() / 180)
    `Int'  i, n, p1, p2
    `RS'   dL, dC, dH, Lbar, Cbar, Hbar, G, a1, a2, C1, C2, h1, h2, T, dh, RT
    `RS'   KL, KC, KH
    `RR'   Lab1, Lab2
    `RC'   D

    KL = KC = KH = 1
    n = rows(Lab)
    D = J(i = rows(P), 1, .)
    for (; i; i--) {
        p1 = P[i,1]; p2 = P[i,2]
        if (p1<0) p1 = n + 1 + p1
        if (p2<0) p2 = n + 1 + p2
        if (p1<1 | p1>n) continue
        if (p2<1 | p2>n) continue
        Lab1 = Lab[p1,]; Lab2 = Lab[p2,]
        Lbar = (Lab1[1] + Lab2[1]) / 2
        Cbar = (sqrt(sum(Lab1[(2,3)]:^2)) + sqrt(sum(Lab2[(2,3)]:^2))) / 2
        G    = (1 - sqrt(Cbar^7 / (Cbar^7 + 25^7))) / 2
        a1   = Lab1[2] * (1 + G)
        a2   = Lab2[2] * (1 + G)
        C1   = sqrt(a1^2 + Lab1[3]^2)
        C2   = sqrt(a2^2 + Lab2[3]^2)
        Cbar = (C1 + C2) / 2
        h1   = mod(atan2(a1, Lab1[3]), 2*pi()) // Mata's atan2() is reverse
        h2   = mod(atan2(a2, Lab2[3]), 2*pi()) // Mata's atan2() is reverse
        Hbar = (h1 + h2)/2 + (abs(h1-h2)>pi() ? pi() : 0)
        T    = 1 - 0.17*cos(Hbar - pi()/6)    + 0.24*cos(2*Hbar) + 
                   0.32*cos(3*Hbar + pi()/30) - 0.20*cos(4*Hbar - 63*pi()/180)
        dh   = h2 - h1
        dh   = dh - (abs(dh)>pi() ? sign(dh)*2*pi() : 0)
        dL   = Lab2[1] - Lab1[1]
        dC   = C2 - C1
        dH   = 2 * sqrt(C1*C2) * sin(dh/2)
        dL = dL / (KL * (1 + 0.015*(Lbar-50)^2/sqrt(20 + (Lbar-50)^2)))
        dC = dC / (KC * (1 + 0.045*Cbar))
        dH = dH / (KH * (1 + 0.015*Cbar*T))
        RT = -2*sqrt(Cbar^7/(Cbar^7 + 25^7)) * 
             sin(exp(-((Hbar*180/pi() - 275)/25)^2)*pi()/3)
        D[i] = sqrt(dL^2 + dC^2 + dH^2 + RT*dC*dH)
    }
    return(D)
}

`RC' `MAIN'::delta_euclid(`RM' C, `IntM' P)
{
    `Int'  i, n, p1, p2
    `RR'   a, b
    `RC'   D
    
    n = rows(C)
    D = J(i = rows(P), 1, .)
    for (; i; i--) {
        p1 = P[i,1]; p2 = P[i,2]
        if (p1<0) p1 = n + 1 + p1
        if (p2<0) p2 = n + 1 + p2
        if (p1<1 | p1>n) continue
        if (p2<1 | p2>n) continue
        a = C[p1,]; b = C[p2,]
        D[i] = sqrt(((a[1]-b[1]))^2 + (a[2]-b[2])^2 + (a[3]-b[3])^2)
    }
    return(D)
}

end

* {smcl}
* {marker convert}{bf:Translate colors between spaces (without storing colors)} {hline}
* {asis}

mata:

`TM' `MAIN'::convert(`TM' C0, `SS' from, | `SS' to, `RS' p, `SS' method)
{
    `SR' FROM, TO
    `SM' PATH
    `TM' C
    pragma unset FROM
    pragma unset TO

    if (convert_parse(TO, to, 1)) {
        display("{err}'" + to + "' not allowed")
        exit(3498)
    }
    if (TO[1]=="GRAY") return(GRAY(C0, from, p, method))
    if (TO[1]=="CVD")  return(CVD(C0, from, p, method))
    if (convert_parse(FROM, from, 0)) {
        display("{err}'" + from + "' not allowed")
        exit(3498)
    }
    if (FROM[1]==TO[1]) {
        if (FROM[1]=="CAM02") {
            if (FROM==TO) return(C0)
            CAM02_setup()
            return(CAM02_to_CAM02(C0, FROM[2], TO[2]))
        }
        if (FROM[1]=="JMh") {
            if (FROM==TO) return(C0)
            return(CAM02_to_JMh(JMh_to_CAM02(C0, FROM[2], "JMh"), "JMh", TO[2]))
        }
        if (FROM[1]=="Jab") {
            if (FROM==TO) return(C0)
            return(CAM02_to_Jab(Jab_to_CAM02(C0, FROM[2], "JMh"), "JMh", TO[2]))
        }
        return(C0)
    }
    PATH = convert_getpath(TO, FROM)
    C = C0
    return(convert_run(C, PATH))
}

`Bool' `MAIN'::convert_parse(`SR' S, `SS' s0, `RS' to)
{
    `SS' s, mask
    pragma unset mask
    
    s = findkey(gettok(s0, mask), SPACES \ SPACES2, "RGB")
    if (s=="") {
        if (to) s = findkey(gettok(s0), ("GRAY", "CVD")')
        if (s=="") return(`TRUE')
    }
    if (mask!="") {
        if (anyof(("CAM02", "JMh", "Jab"), s)==0) return(`TRUE')
    }
    S = (s, mask)
    return(`FALSE')
}

`SM' `MAIN'::convert_getpath(`SR' TO, `SM' path0)
{
    `Int' i
    `SR'  path
    `SM'  edgelist

    i = rows(path0)
    edgelist = ::select(EDGELIST, EDGELIST[,1]:==path0[i,1])
    if (i>1) edgelist = ::select(edgelist, edgelist[,2]:!=path0[i-1,1])
    for (i=rows(edgelist); i; i--) {
        if (edgelist[i,2]==TO[1]) return(path0 \ TO)
        path = convert_getpath(TO, path0 \ (edgelist[i,2], ""))
        if (rows(path)) return(path)
    }
    return(J(0,2,""))
}

`TM' `MAIN'::convert_run(`TM' C, `SM' PATH)
{
    `SS' from, to, err
    
    err = "inconsistent conversion path; this should never happen"
    from = PATH[1,1]; to = PATH[2,1]
    if      (from=="HEX") {
        if      (to=="RGB")     C = HEX_to_RGB(C)
        else                    _error(err)
    }
    else if (from=="RGB") {
        if      (to=="HEX")     C = RGB_to_HEX(C)
        else if (to=="RGB1")    C = RGB_to_RGB1(C)
        else                    _error(err)
    }
    else if (from=="CMYK") {
        if      (to=="CMYK1")   C = CMYK_to_CMYK1(C)
        else                    _error(err)
    }      
    else if (from=="CMYK1") {
        if      (to=="CMYK")    C = CMYK1_to_CMYK(C)
        else if (to=="RGB1")    C = CMYK1_to_RGB1(C)
        else                    _error(err)
    }
    else if (from=="HSV") {
        if      (to=="RGB1")    C = HSV_to_RGB1(C)
        else                    _error(err)
    }
    else if (from=="HSL") {
        if      (to=="RGB1")    C = HSL_to_RGB1(C)
        else                    _error(err)
    }
    else if (from=="RGB1") {
        if      (to=="RGB")     C = RGB1_to_RGB(C)
        else if (to=="CMYK1")   C = RGB1_to_CMYK1(C)
        else if (to=="HSV")     C = RGB1_to_HSV(C)
        else if (to=="HSL")     C = RGB1_to_HSL(C)
        else if (to=="lRGB")    C = RGB1_to_lRGB(C)
        else                    _error(err)
    }
    else if (from=="lRGB") {
        if      (to=="RGB1")    C = lRGB_to_RGB1(C)
        else if (to=="XYZ")     C = lRGB_to_XYZ(C)
        else                    _error(err)
    }
    else if (from=="XYZ") {
        if      (to=="lRGB")    C = XYZ_to_lRGB(C)
        else if (to=="XYZ1")    C = XYZ_to_XYZ1(C)
        else if (to=="xyY")     C = XYZ_to_xyY(C)
        else if (to=="Lab")     C = XYZ_to_Lab(C)
        else if (to=="Luv")     C = XYZ_to_Luv(C)
        else if (to=="CAM02")   C = XYZ_to_CAM02(C, PATH[2,2])
        else                    _error(err)
    }
    else if (from=="XYZ1") {
        if      (to=="XYZ")     C = XYZ1_to_XYZ(C)
        else                    _error(err)
    }
    else if (from=="xyY") {
        if      (to=="XYZ")     C = xyY_to_XYZ(C)
        else if (to=="xyY1")    C = xyY_to_xyY1(C)
        else                    _error(err)
    }
    else if (from=="xyY1") {
        if      (to=="xyY")     C = xyY1_to_xyY(C)
        else                    _error(err)
    }
    else if (from=="Lab") {
        if      (to=="XYZ")     C = Lab_to_XYZ(C)
        else if (to=="LCh")     C = Lab_to_LCh(C)
        else                    _error(err)
    }
    else if (from=="LCh") {
        if      (to=="Lab")     C = LCh_to_Lab(C)
        else                    _error(err)
    }
    else if (from=="Luv") {
        if      (to=="XYZ")     C = Luv_to_XYZ(C)
        else if (to=="HCL")     C = Luv_to_HCL(C)
        else                    _error(err)
    }
    else if (from=="HCL") {
        if      (to=="Luv")     C = HCL_to_Luv(C)
        else                    _error(err)
    }
    else if (from=="CAM02") {
        if      (to=="XYZ")     C = CAM02_to_XYZ(C, PATH[1,2])
        else if (to=="JMh")     C = CAM02_to_JMh(C, PATH[1,2], PATH[2,2])
        else if (to=="Jab")     C = CAM02_to_Jab(C, PATH[1,2], PATH[2,2])
        else                    _error(err)
    }
    else if (from=="JMh") {
        if      (to=="CAM02")   C = JMh_to_CAM02(C, PATH[1,2], PATH[2,2])
        else                    _error(err)
    }
    else if (from=="Jab") {
        if      (to=="CAM02")   C = Jab_to_CAM02(C, PATH[1,2], PATH[2,2])
        else                    _error(err)
    }
    else                        _error(err)
    if (rows(PATH)==2) return(C)
    return(convert_run(C, PATH[|2,1 \ .,.|]))
}

end

* {smcl}
* {marker translate}{bf:Elementary translators} {hline}
* {asis}

mata:

// Transformation between HEX string and RGB {0,...,255} 

`RM' `MAIN'::HEX_to_RGB(`SV' HEX)
{
    `Int' i
    `RM'  RGB
    
    i = length(HEX)
    RGB = J(i, 3, .)
    for (; i; i--) RGB[i,] = _HEX_to_RGB(strtrim(HEX[i]))
    return(RGB)
}

`RR' `MAIN'::_HEX_to_RGB(`SS' HEX)
{
    `SS'  c
    `Int' l
    
    c = strtrim(substr(HEX,2,.)) // get rid of #; allows blanks after #
    l = strlen(c)
    if (l==3) c = substr(c,1,1)*2 + substr(c,2,1)*2 + substr(c,3,1)*2
    else if (l!=6) return(J(1,3,.))
    return((frombase(16, substr(c,1,2)), 
            frombase(16, substr(c,3,2)),
            frombase(16, substr(c,5,2))))
}

`SC' `MAIN'::RGB_to_HEX(`RM' RGB)
{
    `Int' i
    `SC'  HEX
    
    assert_cols(RGB, 3)
    i = rows(RGB)
    HEX = J(i, 1, "")
    for (; i; i--) HEX[i] = _RGB_to_HEX(RGB[i,])
    return(HEX)
}

`SS' `MAIN'::_RGB_to_HEX(`RV' RGB)
{
    `Int' i
    `SS'  HEX, s
    pragma unset HEX
    
    if (max(RGB)>255) return("")
    if (min(RGB)<0)   return("")
    for (i=3; i; i--) {
        s = inbase(16, RGB[i])
        if (strlen(s)==1) s = "0" + s
        //if (strlen(s)!=2) return("")
        HEX = s + HEX
    }
    return("#" + HEX)
}

// Transformation between CMYK [0,1] and CMYK {0,...,255}

`RM' `MAIN'::CMYK1_to_CMYK(`RM' CMYK1) return(clip(round(CMYK1*255), 0, 255))

`RM' `MAIN'::CMYK_to_CMYK1(`RM' CMYK) return(CMYK/255)

// Transformation between CMYK [0,1] and RGB [0,1]
// source: .setcmyk from color.class and rgb2cmyk in palette.ado (official Stata)

`RM' `MAIN'::RGB1_to_CMYK1(`RM' RGB1)
{
    `Int' i
    `RM'  CMYK1
    
    assert_cols(RGB1, 3)
    i = rows(RGB1)
    CMYK1 = J(i, 4, .)
    for (; i; i--) CMYK1[i,] = _RGB1_to_CMYK1(RGB1[i,])
    return(CMYK1)
}

`RR' `MAIN'::_RGB1_to_CMYK1(`RV' RGB1)
{
    `RS' c, m, y, k
    
    c = 1 - RGB1[1]; m = 1 - RGB1[2]; y = 1 - RGB1[3]
    k = min((c, m, y))
    return((c-k, m-k, y-k, k))
}

`RM' `MAIN'::CMYK1_to_RGB1(`RM' CMYK1)
{
    `Int' i
    `RM'  RGB1
    
    assert_cols(CMYK1, 4)
    i = rows(CMYK1)
    RGB1 = J(i, 3, .)
    for (; i; i--) RGB1[i,] = _CMYK1_to_RGB1(CMYK1[i,])
    return(RGB1)
}

`RR' `MAIN'::_CMYK1_to_RGB1(`RV' CMYK1)
{
    `RS' c, m, y, k
    
    c = CMYK1[1]; m = CMYK1[2]; y = CMYK1[3]; k = CMYK1[4]
    return((((c+k)<1 ? 1 - (c+k) : 0),
            ((m+k)<1 ? 1 - (m+k) : 0),
            ((y+k)<1 ? 1 - (y+k) : 0)))
}

// Transformation between RGB [0,1] and RGB {0,...,255}

`RM' `MAIN'::RGB_to_RGB1(`RM' RGB) return(RGB/255)

`RM' `MAIN'::RGB1_to_RGB(`RM' RGB1) return(clip(round(RGB1*255), 0, 255))

// Transformation between RGB [0,1] and linear RGB [0,1]
// sources:
// https://en.wikipedia.org/wiki/SRGB
// http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html
// http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf
// https://en.wikipedia.org/wiki/Gamma_correction

`RM' `MAIN'::RGB1_to_lRGB(`RM' RGB1)
{
    `Int' i, j, c
    `RS'  C, g, offset, transition, slope
    `RM'  lRGB
    
    if (length(rgb_gamma)==1) {
        if (rgb_gamma!=1) return(sign(RGB1) :* abs(RGB1):^rgb_gamma)
        return(RGB1)
    }
    i = rows(RGB1); c = cols(RGB1)
    lRGB = J(i, c, .)
    g          = rgb_gamma[1]
    offset     = rgb_gamma[2]
    transition = rgb_gamma[3]
    slope      = rgb_gamma[4]
    transition = transition * slope
    for (; i; i--) {
        for (j=c; j; j--) {
            C = RGB1[i,j]
            lRGB[i,j] = (abs(C)<=transition ? C/slope :
                sign(C) * ((abs(C) + offset)/(1 + offset))^g)
        }
    }
    return(lRGB)
}

`RM' `MAIN'::lRGB_to_RGB1(`RM' lRGB)
{
    `Int' i, j, c
    `RS'  C, g, offset, transition, slope
    `RM'  RGB1
    
    if (length(rgb_gamma)==1) {
        if (rgb_gamma!=1) return(sign(lRGB) :* abs(lRGB):^(1/rgb_gamma))
        return(lRGB)
    }
    i = rows(lRGB); c = cols(lRGB)
    RGB1 = J(i, c, .)
    g          = 1/rgb_gamma[1]
    offset     = rgb_gamma[2]
    transition = rgb_gamma[3]
    slope      = rgb_gamma[4] 
    for (; i; i--) {
        for (j=c; j; j--) {
            C = lRGB[i,j]
            RGB1[i,j] = (abs(C)<=transition ? slope*C :
                sign(C) * ((1 + offset)*abs(C)^g - offset))
        }
    }
    return(RGB1)
}

// Transformation between CIE XYZ 100 and linear RGB [0,1]
// source: https://en.wikipedia.org/wiki/CIE_1931_color_space

`RM' `MAIN'::lRGB_to_XYZ(`RM' lRGB)
{
    `Int' i
    `RM'  XYZ, M
    
    assert_cols(lRGB, 3)
    M = rgb_M'
    i = rows(lRGB)
    XYZ = J(i, 3, .)
    for (; i; i--) XYZ[i,] = lRGB[i,] * M
    if (rgb_white!=white) XYZ = XYZ_to_XYZ(XYZ, rgb_white, white)
    return(XYZ)
}

`RM' `MAIN'::XYZ_to_lRGB(`RM' XYZ)
{
    `Int' i
    `RM'  lRGB, M
    
    assert_cols(XYZ, 3)
    lRGB = XYZ
    if (white!=rgb_white) lRGB = XYZ_to_XYZ(lRGB, white, rgb_white)
    M = rgb_invM'
    for (i=rows(lRGB); i; i--) lRGB[i,] = lRGB[i,] * M
    return(lRGB)
}

`RM' `MAIN'::XYZ_to_XYZ1(`RM' XYZ) return(XYZ/100)
`RM' `MAIN'::XYZ1_to_XYZ(`RM' XYZ1) return(XYZ1*100)

// Transformation between CIE XYZ and CIE xyY
// source: https://en.wikipedia.org/wiki/CIE_1931_color_space

`RM' `MAIN'::XYZ_to_xyY(`RM' XYZ)
{
    `Int' i
    `RS'  sum
    `RM'  xyY
    
    assert_cols(XYZ, 3)
    i = rows(XYZ)
    xyY = J(i, 3, .)
    for (; i; i--) {
        sum = XYZ[i,1] + XYZ[i,2] + XYZ[i,3]
        xyY[i,1] = (sum==0 ? 0 : XYZ[i,1]/sum)
        xyY[i,2] = (sum==0 ? 0 : XYZ[i,2]/sum)
        xyY[i,3] =               XYZ[i,2]
    }
    return(xyY)
}

`RM' `MAIN'::xyY_to_XYZ(`RM' xyY)
{
    `Int' i
    `RS'  f
    `RM'  XYZ
    
    assert_cols(xyY, 3)
    i = rows(xyY)
    XYZ = J(i, 3, .)
    for (; i; i--) {
        f = (xyY[i,2] ? xyY[i,3]/xyY[i,2] : sign(xyY[i,3]) * maxdouble())
        XYZ[i,1] = xyY[i,1] * f
        XYZ[i,2] = xyY[i,3]
        XYZ[i,3] = (1 - xyY[i,1] - xyY[i,2]) * f
    }
    return(XYZ)
}

`RM' `MAIN'::xyY_to_xyY1(`RM' xyY)
{
    assert_cols(xyY, 3)
    xyY[,3] = xyY[,3]/100
    return(xyY)
}
`RM' `MAIN'::xyY1_to_xyY(`RM' xyY1)
{
    assert_cols(xyY1, 3)
    xyY1[,3] = xyY1[,3]*100
    return(xyY1)
}

// Transformation between CIE L*a*b* and CIE XYZ
// source: https://en.wikipedia.org/wiki/CIELAB_color_space

`RM' `MAIN'::XYZ_to_Lab(`RM' XYZ)
{
    `Int' i
    `RM' Lab
    
    assert_cols(XYZ, 3)
    Lab = XYZ :/ white
    for (i=rows(Lab); i; i--) Lab[i,] = _XYZ_to_Lab(Lab[i,])
    return(Lab)
}

`RR' `MAIN'::_XYZ_to_Lab(`RR' XYZ)
{
    return((
        /* L* */ 116 *  _XYZ_to_Lab_f(XYZ[2]) - 16,
        /* a* */ 500 * (_XYZ_to_Lab_f(XYZ[1]) - _XYZ_to_Lab_f(XYZ[2])),
        /* b* */ 200 * (_XYZ_to_Lab_f(XYZ[2]) - _XYZ_to_Lab_f(XYZ[3]))
        ))
}

`RS' `MAIN'::_XYZ_to_Lab_f(`RS' t)
{
    `RS' delta
    
    delta = 6/29
    return( t > delta^3 ? t^(1/3) : t / (3*delta^2) + 4/29 )
}

`RM' `MAIN'::Lab_to_XYZ(`RM' Lab)
{
    `Int' i
    `RM' XYZ

    assert_cols(Lab, 3)
    i = rows(Lab)
    XYZ = J(i, 3, .)
    for (; i; i--) XYZ[i,] = _Lab_to_XYZ(Lab[i,])
    XYZ = XYZ :* white
    return(XYZ)
}

`RR' `MAIN'::_Lab_to_XYZ(`RR' Lab)
{
    return((
        /* X */ _Lab_to_XYZ_f((Lab[1] + 16)/116 + Lab[2]/500),
        /* Y */ _Lab_to_XYZ_f((Lab[1] + 16)/116),
        /* Z */ _Lab_to_XYZ_f((Lab[1] + 16)/116 - Lab[3]/200)
        ))
}

`RS' `MAIN'::_Lab_to_XYZ_f(`RS' t)
{
    `RS' delta
    
    delta = 6/29
    return( t > delta ? t^3 : 3*delta^2 * (t - 4/29) )
}

// Transformation between CIE L*a*b* and CIE LCh
// source: https://en.wikipedia.org/wiki/CIELAB_color_space

`RM' `MAIN'::Lab_to_LCh(`RM' Lab)
{
    assert_cols(Lab, 3)
    if (rows(Lab)==0) return(Lab)
    return((
        /* L */ Lab[,1],
        /* C */ sqrt(Lab[,2]:^2 :+ Lab[,3]:^2),
        /* h */ mod(atan2(Lab[,2], Lab[,3]) * 180 / pi() :+ .5, 360) :- .5
        ))  // note: Mata's atan2() is reverse
}

`RM' `MAIN'::LCh_to_Lab(`RM' LCh)
{
    `RC' h

    assert_cols(LCh, 3)
    h = LCh[,3] * pi() / 180
    return((LCh[,1], LCh[,2] :* cos(h), LCh[,2] :* sin(h)))
}

// Transformation between CIE L*u*v* and RGB [0,1]
// source: https://en.wikipedia.org/wiki/CIELUV
// source: https://en.wikipedia.org/wiki/HCL_color_space

`RM' `MAIN'::XYZ_to_Luv(`RM' XYZ)
{
    `Int' i
    `RM'  Luv
    
    assert_cols(XYZ, 3)
    i = rows(XYZ)
    Luv = J(i, 3, .)
    for (; i; i--) Luv[i,] = _XYZ_to_Luv(XYZ[i,], white)
    return(Luv)
}

`RR' `MAIN'::_XYZ_to_Luv(`RR' XYZ, `RR' xyz)
{
    `RS' L
    
    L = 116 * _XYZ_to_Lab_f(XYZ[2]/xyz[2]) - 16
    return((
        /* L* */ L,
        /* u* */ 13*L * (_XYZ_to_Luv_u(XYZ) - _XYZ_to_Luv_u(xyz)),
        /* v* */ 13*L * (_XYZ_to_Luv_v(XYZ) - _XYZ_to_Luv_v(xyz))
        ))
}

`RS' `MAIN'::_XYZ_to_Luv_u(`RR' XYZ)
{
    `RS' sum
    
    sum = XYZ[1] + 15*XYZ[2] + 3*XYZ[3]
    return( sum==0 ? 0 : 4*XYZ[1]/sum )
}

`RS' `MAIN'::_XYZ_to_Luv_v(`RR' XYZ)
{
    `RS' sum
    
    sum = XYZ[1] + 15*XYZ[2] + 3*XYZ[3]
    return( sum==0 ? 0 : 9*XYZ[2]/sum )
}

`RM' `MAIN'::Luv_to_XYZ(`RM' Luv)
{
    `Int' i
    `RM'  XYZ
    
    assert_cols(Luv, 3)
    i = rows(Luv)
    XYZ = J(i, 3, .)
    for (; i; i--) XYZ[i,] = _Luv_to_XYZ(Luv[i,], white)
    return(XYZ)
}

`RR' `MAIN'::_Luv_to_XYZ(`RR' Luv, `RR' xyz)
{
    `RS' u, v, Y
    
    Y = xyz[2] * _Lab_to_XYZ_f((Luv[1]+16)/116)
    u = (Luv[1]==0 ? 0 : Luv[2] / (13*Luv[1])) + _XYZ_to_Luv_u(xyz)
    v = (Luv[1]==0 ? 0 : Luv[3] / (13*Luv[1])) + _XYZ_to_Luv_v(xyz)
    return((
        /* X */ Y * (9*u) / (4*v),
        /* Y */ Y,
        /* Z */ Y * (12-3*u-20*v) / (4*v)
        ))
}

// Transformation between CIE L*u*v* and HCL
// source: https://en.wikipedia.org/wiki/HCL_color_space

`RM' `MAIN'::Luv_to_HCL(`RM' Luv)
{
    assert_cols(Luv, 3)
    if (rows(Luv)==0) return(Luv)
    return((
        /* H */ mod(atan2(Luv[,2], Luv[,3]) * 180 / pi() :+ .5, 360) :- .5,
        /* C */ sqrt(Luv[,2]:^2 :+ Luv[,3]:^2),
        /* L */ Luv[,1]
        ))  // note: Mata's atan2() is reverse
}

`RM' `MAIN'::HCL_to_Luv(`RM' HCL)
{
    `RC' h
    
    assert_cols(HCL, 3)
    h = HCL[,1] * pi() / 180
    return((HCL[,3], HCL[,2] :* cos(h), HCL[,2] :* sin(h)))
}

// Transformation between CIE XYZ and CIE CAM02
// Source: 
//   Luo, M.R., C. Li (2013). CIECAM02 and its recent developments. P 19-58 in: 
//   C. Fernandez-Maloigne (ed). Advanced color image processing and analysis. 
//   New York: Springer. https://doi.org/10.1007/978-1-4419-6190-7_2

`RM' `MAIN'::XYZ_to_CAM02(`RM' XYZ, `SS' mask)
{
    `Int'  i, r
    `RM'   JCh
    
    assert_cols(XYZ, 3)
    r = rows(XYZ)
    JCh = J(r, 3, .)
    CAM02_setup()
    for (i=r; i; i--) JCh[i,] = _XYZ_to_CAM02(XYZ[i,]')
    return(CAM02_to_CAM02(JCh, "JCh", mask))
}

`RR' `MAIN'::_XYZ_to_CAM02(`RC' XYZ)
{
    `RS'  a, b, h2, A, et, t
    `RS'  J, C, h
    `RC'  LMS, sign, tmp

    LMS  = C02.HPE * C02.iCAT02 * (C02.D :* (C02.CAT02 * XYZ))
    sign = sign(LMS)
    tmp  = ((sign :* C02.FL :* LMS)/100):^0.42
    LMS  = sign :* 400 :* (tmp:/(tmp :+ 27.13)) :+ 0.1
    a    = LMS[1] - 12*LMS[2]/11 + LMS[3]/11
    b    = (LMS[1] + LMS[2] - 2*LMS[3]) / 9
    h    = mod(atan2(a, b) * 180 / pi() + .5, 360) - .5
    h2   = (h < C02.HUE[1,1] ? h + 360 : h)
    A    = (2*LMS[1] + LMS[2] + LMS[3]/20 - 0.305) * C02.Nbb
    if (A<0) J = 0
    else     J = 100 * (A / C02.Aw)^(C02.c * C02.z)
    et   = 0.25 * (cos((h2 * pi())/180 + 2) + 3.8)
    t    = ((50000/13 * C02.Nc * C02.Ncb) * et * sqrt(a^2 + b^2)) / 
           (LMS[1] + LMS[2] + (21/20) * LMS[3])
    if (t<0) C = 0
    else     C = t^0.9 * sqrt(J/100) * (1.64 - 0.29^C02.n)^0.73
    return((J, C, h))
}

`RM' `MAIN'::CAM02_to_XYZ(`RM' CAM02, `SS' mask)
{
    `Int'  i
    `RM'   XYZ
    
    CAM02_setup()
    XYZ = CAM02_to_CAM02(CAM02, mask, "JCh")
    for (i=rows(XYZ); i; i--) XYZ[i,] = _CAM02_to_XYZ(XYZ[i,])
    return(XYZ)
}

`RR' `MAIN'::_CAM02_to_XYZ(`RR' JCh)
{
    `RS'  a, b, A, et, t, p1, p2, p3
    `RS'  J, C, h
    `RC'  XYZ

    // Compute a and b
    J = JCh[1]; C = JCh[2]; h = JCh[3] * pi() / 180
    if (J<0) J = 0 // J is not allowed to be smaller than 0
    if (C<0) C = 0 // C is not allowed to be smaller than 0
    t  = (C / (sqrt(J/100) * (1.64 - 0.29^C02.n)^0.73))^(1/0.9)
    A  = C02.Aw * (J/100)^(1 / (C02.c * C02.z))
    p2 = A / C02.Nbb + 0.305
    if (t==0) a = b = 0
    else {
        p3 = 21/20
        et = 0.25 * (cos(h + 2) + 3.8)
        p1 = (50000/13 * C02.Nc * C02.Ncb) * et * (1/t)
        if (J==0) p1 = 0  // in this case: 1/t = 1/infinity = 0
        if (abs(sin(h))>=abs(cos(h))) {
            b = (p2 * (2 + p3) * (460/1403)) / ((p1 / sin(h)) 
                + (2 + p3) * (220/1403) * (cos(h) / sin(h)) 
                - (27/1403) + p3 * (6300/1403))
            a = b * (cos(h)/sin(h))
        }
        else {
            a = (p2 * (2 + p3) * (460/1403)) / ((p1 / cos(h))
                + (2 + p3) * (220/1403) 
                - ((27/1403) - p3 * (6300/1403)) * (sin(h)/cos(h)))
            b = a * (sin(h)/cos(h))
        }
    }
    
    // Compute XYZ from J, a, and b
    if (missing((J,a,b))) return(J(1,3,.))  // not enough information
    XYZ = ((460*p2 + 451*a + 288*b) \ 
           (460*p2 - 891*a - 261*b) \ 
           (460*p2 - 220*a - 6300*b)) / 1403 :- 0.1
    XYZ = (100/C02.FL) * sign(XYZ) :* 
          ((27.13 * abs(XYZ)) :/ (400 :- abs(XYZ))):^(1/0.42)
          // missing if XYZ>=400
    XYZ = C02.iCAT02 * ((C02.CAT02 * C02.iHPE * XYZ) :/ C02.D)
    return(XYZ')
}

`RM' `MAIN'::CAM02_to_CAM02(`RM' IN, `SS' fmask, `SS' tmask)
{
    `RS' i, j
    `SS' c
    `RC' J, Q, C, M, s, H, h
    `RM' OUT
    
    // setup
    if (fmask=="") fmask = "JCh"
    if (tmask=="") tmask = "JCh"
    j = strlen(fmask)
    assert_cols(IN, j)
    if (fmask==tmask) return(IN) // nothing to do

    // parse input
    for (; j; j--) {
        c = substr(fmask, j, 1)
        if      (c=="J") J = IN[,j]
        else if (c=="Q") Q = IN[,j]
        else if (c=="C") C = IN[,j]
        else if (c=="M") M = IN[,j]
        else if (c=="s") s = IN[,j]
        else if (c=="H") H = IN[,j]
        else if (c=="h") h = IN[,j]
        else {
            display("{err}character '" + c + "' not allowed in mask")
            exit(3498)
        }
    }
    
    // generate output
    j = strlen(tmask)
    i = rows(IN)
    OUT = J(i, j, .)
    if (i==0) return(OUT)
    for (; j; j--) {
        c = substr(tmask, j, 1)
        if (c=="J") {
            if (rows(J)==0) {
                if (rows(Q)) {
                    J = 6.25 * ((C02.c*Q) / ((C02.Aw+4)*C02.FL^0.25)):^2
                }
                else {
                    display("{err}input must contain one of J and Q")
                    exit(3498)
                }
            }
            OUT[,j] = J
        }
        else if (c=="Q") {
            if (rows(Q)==0) CAM02_to_CAM02_Q(J, Q)
            OUT[,j] = Q
        }
        else if (c=="C") {
            if (rows(C)==0) {
                if (rows(M)==0) {
                    if (rows(s)) {
                        if (rows(Q)==0) CAM02_to_CAM02_Q(J, Q)
                        M = (s/100):^2 :* Q
                    }
                    else {
                        display("{err}input must contain one of C, M, and s")
                        exit(3498)
                    }
                }
                C = M / C02.FL^0.25
            }
            OUT[,j] = C
        }
        else if (c=="M") {
            if (rows(M)==0) {
                if (rows(C)) {
                    M = C * C02.FL^0.25
                }
                else if (rows(s)) {
                    if (rows(Q)==0) CAM02_to_CAM02_Q(J, Q)
                    M = (s/100):^2 :* Q
                }
                else {
                    display("{err}input must contain one of C, M, and s")
                    exit(3498)
                }
            }
            OUT[,j] = M
        }
        else if (c=="s") {
            if (rows(s)==0) {
                if (rows(Q)==0) CAM02_to_CAM02_Q(J, Q)
                if (rows(M)==0) {
                    if (rows(C)) M = C * C02.FL^0.25
                    else {
                        display("{err}input must contain one of C, M, and s")
                        exit(3498)
                    }
                }
                s = 100 * sqrt(M:/Q)
            }
            OUT[,j] = s
        }
        else if (c=="H") {
            if (rows(H)==0) {
                if (rows(h)) {
                    i = rows(h)
                    H = J(i, 1, .)
                    for (; i; i--) {
                        H[i]  = CAM02_H(h[i]<C02.HUE[1,1] ? h[i] + 360 : h[i], C02.HUE)
                    }
                }
                else {
                    display("{err}input must contain one of h and H")
                    exit(3498)
                }
            }
            OUT[,j] = H
        }
        else if (c=="h") {
            if (rows(h)==0) {
                if (rows(H)) {
                    i = rows(H)
                    h = J(i, 1, .)
                    for (; i; i--) {
                        h[i] = CAM02_invH(H[i], C02.HUE)
                        if (h[i]>360) h[i] = h[i] - 360
                    }
                }
                else {
                    display("{err}input must contain one of h and H")
                    exit(3498)
                }
            }
            OUT[,j] = h
        }
        else {
            display("{err}character '" + c + "' not allowed in mask")
            exit(3498)
        }
    }
    return(OUT)
}

void `MAIN'::CAM02_to_CAM02_Q(`RM' J, `RM' Q)
{
    if (rows(J)) {
        Q = ((4/C02.c) * (C02.Aw + 4) * C02.FL^0.25) * sqrt(J/100)
    }
    else {
        display("{err}input must contain one of J and Q")
        exit(3498)
    }
}

void `MAIN'::CAM02_setup()
{
    `RS' d, k
    `RC' LMS, sign, tmp
    
    // already set?
    if (C02.set==1) return
    
    // matrixes
    C02.CAT02  = tmatrix("CAT02")
    C02.iCAT02 = luinv(C02.CAT02)
    C02.HPE    = tmatrix("HPE")
    C02.iHPE   = luinv(C02.HPE)
    C02.HUE    = (20.14 , 0.8, 0  ) \  // i=1 Red    (h_i, e_i, H_i)
                 (90    , 0.7, 100) \  // i=2 Yellow
                 (164.25, 1.0, 200) \  // i=3 Green
                 (237.53, 1.2, 300) \  // i=4 Blue
                 (380.14, 0.8, 400)    // i=5 Red
    
    // some constants
    k       = 1 / (5*C02.LA + 1)
    C02.FL  = 0.2 * k^4 * (5*C02.LA) + 0.1 * (1 - k^4)^2 * (5*C02.LA)^(1/3)
    C02.n   = C02.Yb/white[2] //__clip(C.Yb/white[2], 0, 1)  ??
    C02.z   = 1.48 + sqrt(C02.n)
    C02.Ncb = C02.Nbb = 0.725 * (1/C02.n)^0.2

    // Compute D and Aw from white point
    LMS    = C02.CAT02 * white'
    d      = __clip(C02.F * (1 - (1/3.6) * exp((-C02.LA - 42)/92)), 0, 1)
    C02.D  = d * (white[2]:/LMS) :+ (1 - d)
    LMS    = C02.HPE * C02.iCAT02 * (C02.D :* LMS)
    sign   = sign(LMS)
    tmp    = ((sign :* C02.FL :* LMS)/100):^0.42
    LMS    = sign :* 400 :* (tmp:/(tmp :+ 27.13)) :+ 0.1
    C02.Aw = (2*LMS[1] + LMS[2] + LMS[3]/20 - 0.305) * C02.Nbb
    
    // update flag
    C02.set = 1
}

`RS' `MAIN'::CAM02_H(`RS' h, `RM' H)
{
    `Int' i
    
    i = 1
    if (H[2,1]<=h) i++
    if (H[3,1]<=h) i++
    if (H[4,1]<=h) i++
    return(H[i,3] + (100 * (h-H[i,1])/H[i,2]) / 
        ((h-H[i,1])/H[i,2] + (H[i+1,1] - h)/H[i+1,2]))
}

`RS' `MAIN'::CAM02_invH(`RS' h, `RM' H)
{
    `Int' i
    
    i = 1
    if (H[2,3]<=h) i++
    if (H[3,3]<=h) i++
    if (H[4,3]<=h) i++
    return(((h - H[i,3]) * (H[i+1,2]*H[i,1] - H[i,2]*H[i+1,1]) - 100*H[i,1]*H[i+1,2]) /
           ((h - H[i,3]) * (H[i+1,2]-H[i,2]) - 100*H[i+1,2]))
}

// Transformation between CIE CAM02 and J'a'b'
// Source:
//   Luo, M.R., C. Li (2013). CIECAM02 and its recent developments. P 19-58 in: 
//   C. Fernandez-Maloigne (ed). Advanced color image processing and analysis. 
//   New York: Springer. https://doi.org/10.1007/978-1-4419-6190-7_2

// Step 1: CIE CAM02 JMh <-> J'M'h
`RM' `MAIN'::CAM02_to_JMh(`RM' CAM02, `SS' mask, `SS' ucscoefs)
{
    `RS' c1, c2
    `RR' KLc1c2
    `RM' JMh
    
    if (ucscoefs=="") KLc1c2 = ucscoefs()
    else              KLc1c2 = ucscoefs_get(ucscoefs)
    c1 = KLc1c2[2]; c2 = KLc1c2[3]
    CAM02_setup()
    JMh = CAM02_to_CAM02(CAM02, mask, "JMh")
    JMh[,1] = (1 + 100 * c1) * JMh[,1] :/ (1 :+ c1 * JMh[,1])
    JMh[,2] = (1 / c2) * ln(1 :+ c2 * JMh[,2])
    return(JMh)
}

`RM' `MAIN'::JMh_to_CAM02(`RM' JMh0, `SS' ucscoefs, `SS' mask)
{
    `RS' c1, c2
    `RR' KLc1c2
    `RM' JMh
    
    assert_cols(JMh0, 3)
    if (ucscoefs=="") KLc1c2 = ucscoefs()
    else              KLc1c2 = ucscoefs_get(ucscoefs)
    c1 = KLc1c2[2]; c2 = KLc1c2[3]
    JMh = JMh0
    JMh[,1] = JMh[,1] :/ ((1 + 100 * c1) :- c1 * JMh[,1])
        // invalid if J > (1+100*c1)/c1
    JMh[,2] = (exp(c2 * JMh[,2]) :- 1) / c2
    CAM02_setup()
    return(CAM02_to_CAM02(JMh, "JMh", mask))
}

// Step 2: J'M'h <-> J'a'b'
`RM' `MAIN'::CAM02_to_Jab(`RM' CAM02, `SS' mask, `SS' ucscoefs)
{
    `RM' JMh
    `RC' h
    
    JMh = CAM02_to_JMh(CAM02, mask, ucscoefs)
    h = JMh[,3] * pi() / 180
    return((JMh[,1], JMh[,2] :* cos(h), JMh[,2] :* sin(h)))
}

`RM' `MAIN'::Jab_to_CAM02(`RM' Jab, `SS' ucscoefs, `SS' mask)
{
    assert_cols(Jab, 3)
    if (rows(Jab)==0) return(JMh_to_CAM02(Jab, ucscoefs, mask))
    return(JMh_to_CAM02((
        /* J */ Jab[,1],
        /* C */ sqrt(Jab[,2]:^2 :+ Jab[,3]:^2),
        /* h */ mod(atan2(Jab[,2], Jab[,3]) * 180 / pi() :+ .5, 360) :- .5
        ), ucscoefs, mask))  // note: Mata's atan2() is reverse
}

// Transformation between HSV and RGB [0,1]
// source: https://en.wikipedia.org/wiki/HSL_and_HSV

`RM' `MAIN'::RGB1_to_HSV(`RM' RGB1)
{
    `Int' i
    `RM'  HSV

    assert_cols(RGB1, 3)
    i = rows(RGB1)
    HSV = J(i, 3, .)
    for (; i; i--) HSV[i,] = _RGB1_to_HSV(RGB1[i,])
    return(HSV)
}

`RR' `MAIN'::_RGB1_to_HSV(`RV' RGB1)
{
    `RS' r, g, b, M, m, C, H, S, V
    
    r = RGB1[1]; g = RGB1[2]; b = RGB1[3]
    M = max(RGB1); m = min(RGB1)
    C = M - m
    if (C==0)      H = 0 // H undefined, set to 0 by convention
    else if (M==r) H = mod((g-b)/C,  6) * 60
    else if (M==g) H =    ((b-r)/C + 2) * 60
    else if (M==b) H =    ((r-g)/C + 4) * 60
    V = M
    S = (V==0 ? 0 : C/V)
    return((H, S, V))
}

`RM' `MAIN'::HSV_to_RGB1(`RM' HSV)
{
    `Int' i
    `RM'  RGB1
    
    assert_cols(HSV, 3)
    i = rows(HSV)
    RGB1 = J(i, 3, .)
    for (; i; i--) RGB1[i,] = _HSV_to_RGB1(HSV[i,])
    return(RGB1)
}

`RR' `MAIN'::_HSV_to_RGB1(`RV' HSV)
{
    `RS'   h
    `Int'  i
    `IntR' p
    
    h = HSV[1] / 60
    i = mod(floor(h), 6) // wrap around if H outside [0,360)
         if (i==0) p = 1, 2, 3
    else if (i==1) p = 2, 1, 3
    else if (i==2) p = 3, 1, 2
    else if (i==3) p = 3, 2, 1
    else if (i==4) p = 2, 3, 1
    else if (i==5) p = 1, 3, 2
    return((
        HSV[3], 
        HSV[3] * (1 - HSV[2] * abs(mod(h,2) - 1)), 
        HSV[3] * (1 - HSV[2])
        )[p])
}

// Transformation between HSL and RGB [0,1]
// source: https://en.wikipedia.org/wiki/HSL_and_HSV

`RM' `MAIN'::RGB1_to_HSL(`RM' RGB1)
{
    `Int' i
    `RM'  HSL

    assert_cols(RGB1, 3)
    i = rows(RGB1)
    HSL = J(i, 3, .)
    for (; i; i--) HSL[i,] = _RGB1_to_HSL(RGB1[i,])
    return(HSL)
}

`RR' `MAIN'::_RGB1_to_HSL(`RV' RGB1)
{
    `RS' r, g, b, M, m, C, H, S, L
    
    r = RGB1[1]; g = RGB1[2]; b = RGB1[3]
    M = max(RGB1); m = min(RGB1)
    C = M - m
    if (C==0)      H = 0 // H undefined, set to 0 by convention
    else if (M==r) H = mod((g-b)/C,  6) * 60
    else if (M==g) H =    ((b-r)/C + 2) * 60
    else if (M==b) H =    ((r-g)/C + 4) * 60
    L = (M + m)/2
    S = (L==1 ? 0 : C / (1 - abs(M + m - 1)))
    return((H, S, L))
}

`RM' `MAIN'::HSL_to_RGB1(`RM' HSL)
{
    `Int' i
    `RM'  RGB1
    
    assert_cols(HSL, 3)
    i = rows(HSL)
    RGB1 = J(i, 3, .)
    for (; i; i--) RGB1[i,] = _HSL_to_RGB1(HSL[i,])
    return(RGB1)
}

`RR' `MAIN'::_HSL_to_RGB1(`RV' HSL)
{
    `RS'   h, C, X, m
    `Int'  i
    `IntR' p
    
    h = HSL[1] / 60
    i = mod(floor(h), 6) // wrap around if H outside [0,360)
         if (i==0) p = 1, 2, 3
    else if (i==1) p = 2, 1, 3
    else if (i==2) p = 3, 1, 2
    else if (i==3) p = 3, 2, 1
    else if (i==4) p = 2, 3, 1
    else if (i==5) p = 1, 3, 2
    C = (1 - abs(2*HSL[3] - 1)) * HSL[2]
    X = C * (1 - abs(mod(h,2) - 1))
    m = HSL[3] - C/2
    return((C+m, X+m, m)[p])
}

end

* {smcl}
* {marker web}{bf:Named web colors} {hline}
* {asis}

mata:

`SM' `MAIN'::namedcolors(| `SS' pattern, `Bool' strict)
{
    `Int' i
    `SC'  keys
    `SM'  C
    
    if (namedcolors==NULL) namedcolorindex()
    keys = namedcolors->keys()
    if (pattern!="") {
        if (args()<2) strict = 0
        if (strict) keys = ::select(keys, strmatch(keys, pattern))
        else keys = ::select(keys, strmatch(strlower(keys), strlower(pattern)))
    }
    _sort(keys, 1)
    i = rows(keys)
    C = J(i,2,"")
    for (i=rows(keys); i; i--) C[i,] = namedcolors->get(keys[i])
    return(::select((keys,C[,1]), C[,2]:=="")) // exclude system colors
}

void `MAIN'::namedcolorindex()
{
    `Bool' brk
    `Int'  j
    `SR'   kw
    
    // setup
    namedcolors = findexternal("ColrSpace_namedcolorindex")
    if (namedcolors!=NULL) {
        if (classname(*namedcolors)=="`DICT'") return
        namedcolors = NULL
        rmexternal("ColrSpace_namedcolorindex")
    }
    brk = setbreakintr(0)
    namedcolors = crexternal("ColrSpace_namedcolorindex")
    *namedcolors = `DICT'()
    namedcolors->notfound("")
    // default library
    if (_namedcolorindex("namedcolors")) {
        namedcolors = NULL
        rmexternal("ColrSpace_namedcolorindex")
        display("{err}colrspace_library_namedcolors.sthlp not found")
        exit(error(601))
    }
    // personal library
    (void) _namedcolorindex("namedcolors_personal")
    // placeholders for (documented) system colors
    kw = ("black","gs0","gs1","gs2","gs3","gs4","gs5","gs6","gs7","gs8","gs9",
        "gs10","gs11","gs12","gs13","gs14","gs15","gs16","white","blue",
        "bluishgray","brown","cranberry","cyan","dimgray","dkgreen","dknavy",
        "dkorange","eggshell","emerald","forest_green","gold","gray","green",
        "khaki","lavender","lime","ltblue","ltbluishgray","ltkhaki","magenta",
        "maroon","midblue","midgreen","mint","navy","olive","olive_teal",
        "orange","orange_red","pink","purple","red","sand","sandb","sienna",
        "stone","teal","yellow","ebg","ebblue","edkblue","eltblue","eltgreen",
        "emidblue","erose")
    if (stataversion()>=1800) {
        kw = kw, ("stc1","stc2","stc3","stc4","stc5",
            "stc6","stc7","stc8","stc9","stc10","stc11","stc12","stc13",
            "stc14","stc15","stblue","stgreen","stred","styellow")
    }
    else { // make additional Stata 18 colors available in Stata 17 or below
        namedcolors->put(    "stc1", ("#1a85ff",""))
        namedcolors->put(    "stc2", ("#d41159",""))
        namedcolors->put(    "stc3", ("#00bf7f",""))
        namedcolors->put(    "stc4", ("#ffd400",""))
        namedcolors->put(    "stc5", ("#4f2c99",""))
        namedcolors->put(    "stc6", ("#ff6333",""))
        namedcolors->put(    "stc7", ("#4db7ff",""))
        namedcolors->put(    "stc8", ("#7c0015",""))
        namedcolors->put(    "stc9", ("#0fefaf",""))
        namedcolors->put(   "stc10", ("#faa307",""))
        namedcolors->put(   "stc11", ("#758bfd",""))
        namedcolors->put(   "stc12", ("#fed9b7",""))
        namedcolors->put(   "stc13", ("#08234c",""))
        namedcolors->put(   "stc14", ("#f88dad",""))
        namedcolors->put(   "stc15", ("#0f5156",""))
        namedcolors->put(  "stblue", ("#1a85ff",""))
        namedcolors->put( "stgreen", ("#00bf7f",""))
        namedcolors->put(   "stred", ("#d41159",""))
        namedcolors->put("styellow", ("#ffd400",""))
    }
    for (j=length(kw); j; j--) namedcolors->put(kw[j], ("","1"))
    // additional keywords allowed by colorstyle (or colorstylelist)
    kw = ("none", "fg", "foreground", "bg", "background", ".", "..", "...")
    for (j=length(kw); j; j--) namedcolors->put(kw[j], ("#000000","1"))
    // done
    (void) setbreakintr(brk)
}

`Bool' `MAIN'::_namedcolorindex(`SS' lib)
{
    `Int' l
    `SS'  fn, line, c, nm, hex
    `RS'  fh
    `SM'  EOF
    
    fn = findfile("colrspace_library_"+lib+".sthlp")
    if (fn=="") return(1)
    hex = "#"
    fh  = fopen(fn, "r")
    EOF = J(0, 0, "")
    while ((line=strltrim(fget(fh)))!=EOF) {
        if (substr(line,1,1)!=hex) continue // not a color definition
        l = strpos(line," ")
        if (l==0) continue // color name is missing
        c = substr(line,1,l-1)
        nm = strtrim(substr(line,l,.))
        if (nm=="") continue // color name is missing
        namedcolors->put(nm, (c,"")) // second el "" => not a system color 
    }
    fclose(fh)
    return(0)
}

end

* {smcl}
* {marker palettes}{bf:Palettes} {hline}
* {asis}

mata:

`SS' `MAIN'::pexists(| `SS' pal0, `SS' libname)
{
    `Int' t
    `SS'  pal
    `SR'  SRC
    
    if (palettes==NULL) paletteindex()
    pal = pal0
    t = parse_palette(pal)
    if (t) {
        SRC = ("palettes", "namedcolors", "lsmaps", "generators", "rgbmaps")
        if (t==99)     libname = "internal/alias"
        else if (t<10) libname = SRC[t]
        else           libname = SRC[t-10] + "_personal"
        return(pal)
    }
    return("")
}

`SM' `MAIN'::palettes(| `SS' pattern, `Bool' strict)
{
    `Int' i
    `SC'  keys
    `SC'  src
    `SR'  SRC
    `TS'  t
    
    if (palettes==NULL) paletteindex()
    keys = palettes->keys()
    if (pattern!="") {
        if (args()<2) strict = 0
        if (strict) keys = ::select(keys, strmatch(keys, pattern))
        else keys = ::select(keys, strmatch(strlower(keys), strlower(pattern)))
    }
    i = rows(keys)
    if (i==0) return(J(0,2,""))
    _sort(keys, 1)
    src = J(i, 1, "")
    SRC = ("palettes", "namedcolors", "lsmaps", "generators", "rgbmaps")
    for (;i;i--) {
        t = _palette_tget(keys[i])
        if (isstring(t)) continue
        if (t==99)     src[i] = "internal/alias"
        else if (t<10) src[i] = SRC[t]
        else           src[i] = SRC[t-10] + "_personal"
    }
    keys = ::select(keys, src:!="")
    src  = ::select(src, src:!="")
    return((keys,src))
}

`Int' `MAIN'::parse_palette(`SS' p0) // p0 may be replaced
{
    `SS' p
    `TS' t
    
    t = _palette_tget(p0) // only finds exact match, including case
    if (isstring(t)) {
        p0 = t
        t = _palette_tget(p0)
    }
    if (t==0) {
        p = findkey(p0, palettes->keys(), stataversion()<1800 ? "s2" : "st")
        if (p!="") {
            p0 = p
            t = _palette_tget(p0)
            if (isstring(t)) {
                p0 = t
                t = _palette_tget(p0)
            }
        }
    }
    return(t)
}

void `MAIN'::paletteindex()
{
    `Bool' brk
    
    palettes = findexternal("ColrSpace_paletteindex")
    if (palettes!=NULL) {
        if (classname(*palettes)=="`DICT'") return
        palettes = NULL
        rmexternal("ColrSpace_paletteindex")
    }
    brk = setbreakintr(0)
    palettes = crexternal("ColrSpace_paletteindex")
    *palettes = `DICT'()
    palettes->notfound((&0, NULL))
    _paletteindex()
    (void) setbreakintr(brk)
}

void `MAIN'::_paletteindex() // create palette index
{
    `SS'  lib, fn, line, nm, tn
    `SR'  libraries
    `RS'  fh
    `SM'  EOF
    `TS'  t
    
    //   t = 1         palette from palette library
    //   t = 2         color group from namedcolors library
    //   t = 3         colormap from lsmaps library
    //   t = 4         color generator from generators library
    //   t = 5         colormap from rgbmaps library
    //   t = 11        palette from personal palette library
    //   t = 12        color group from personal namedcolors library
    //   t = 13        colormap from personal lsmaps library
    //   t = 14        color generator from personal generators library
    //   t = 15        colormap from personal rgbmaps library
    //   t = 99        internal palette/alias
    //   t = <string>  pure alias
    
    EOF = J(0, 0, "")
    tn = "n:"
    libraries = ("palettes", "namedcolors", "lsmaps", "generators", "rgbmaps")
    for (t=1; t<=5; t++) {
        lib = "colrspace_library_" + libraries[t] + ".sthlp"
        fn = findfile(lib)
        if (fn=="") {
            display("{err}"+lib+" not found")
            exit(error(601))
        }
        fh = fopen(fn, "r")
        while ((line=strltrim(fget(fh)))!=EOF) {
            if (substr(line,1,2)!=tn) continue // not a palette name
            nm = strtrim(substr(line,3,.))
            if (nm=="") continue // palette name is empty
            _palette_tput(nm, t)
            if (t==2) {
                if (substr(nm,1,4)=="HTML") { // create alias
                    nm = "webcolors" + substr(nm,5,.)
                    _palette_tput(nm, t)
                }
            }
        }
        fclose(fh)
        // personal library
        lib = "colrspace_library_" + libraries[t] + "_personal.sthlp"
        fn = findfile(lib)
        if (fn=="") continue
        fh = fopen(fn, "r")
        while ((line=strltrim(fget(fh)))!=EOF) {
            if (substr(line,1,2)!=tn) continue // not a palette name
            nm = strtrim(substr(line,3,.))
            if (nm=="") continue // palette name is empty
            _palette_tput(nm, t+10)
        }
        fclose(fh)
    }
    t = 99
    _palette_tput("okabe"                      , t)
    _palette_tput("tab10"                      , t)
    _palette_tput("tab20"                      , t)
    _palette_tput("tab20b"                     , t)
    _palette_tput("tab20c"                     , t)
    _palette_tput("spmap blues"                , t)
    _palette_tput("spmap greens"               , t)
    _palette_tput("spmap greys"                , t)
    _palette_tput("spmap reds"                 , t)
    _palette_tput("spmap rainbow"              , t)
    _palette_tput("HTML"                       , t)
    _palette_tput("HTML redorange"             , t)
    _palette_tput("webcolors"                  , t)
    _palette_tput("webcolors redorange"        , t)
    _palette_tput("twilight shifted"           , t)
    _palette_tput("sb"        , "sb deep")
    _palette_tput("sb6"       , "sb deep6")
    _palette_tput("pals"      , "pals kelly")
    _palette_tput("tol"       , "tol muted")
    _palette_tput("carto"     , "carto bold")
    _palette_tput("ptol"      , "ptol qualitative")
    _palette_tput("lin"       , "lin carcolor")
    _palette_tput("sfso"      , "sfso blue")
    _palette_tput("sfso cmyk" , "sfso blue cmyk")
    _palette_tput("w3"        , "w3 default")
    _palette_tput("matplotlib", "matplotlib jet")
    _palette_tput("CET"       , "CET L20")
    _palette_tput("scico"     , "scico batlow")
    _palette_tput("HCL"       , "HCL qualitative")
    _palette_tput("LCh"       , "LCh qualitative")
    _palette_tput("JMh"       , "JMh qualitative")
    _palette_tput("HSV"       , "HSV qualitative")
    _palette_tput("HSL"       , "HSL qualitative")
    _paletteindex_mkalias("scico8", "scico")
}

void `MAIN'::_paletteindex_mkalias(`SS' s1, `ss' s0)
{   // add s1-to-s0-alias for each scheme that exists in s0 but not in s1
    `Int' i
    `SC'  K, k
    `T'   A

    A = asarray_create()
    K = palettes->keys()
    k = substr(::select(K, strmatch(K, s1 + " *")), strlen(s1)+2, .)
    for (i=length(k);i;i--) asarray(A, k[i], 1)
    k = substr(::select(K, strmatch(K, s0 + " *")), strlen(s0)+2, .)
    for (i=length(k);i;i--) {
        if (asarray(A, k[i])!=J(0,0,.)) continue // palette exists
        _palette_tput(s1+" "+k[i], s0+" "+k[i])  // create alias
    }
}

void `MAIN'::_palette_tput(`SS' key, `TS' t0)
{
    `TS' t
    
    if (isfleeting(t0)) {
        palettes->put(key, (&t0, NULL))
        return
    }
    t = t0 // need to make copy
    palettes->put(key, (&t, NULL))
}

`TS' `MAIN'::_palette_tget(`SS' key)
{
    return(*(palettes->get(key)[1]))
}

void `MAIN'::palette(| `SS' pal, `RS' n, `RV' o1, `RV' o2, `RV' o3, `RV' o4)
{
    `Int' rc
    
    S = &data1
    if      (args()<=2) rc = Palette(pal, n)
    else if (args()==3) rc = Palette(pal, n, o1)
    else if (args()==4) rc = Palette(pal, n, o1, o2)
    else if (args()==5) rc = Palette(pal, n, o1, o2, o3)
    else                rc = Palette(pal, n, o1, o2, o3, o4)
    if (rc==0) {
        display("{err}palette '" + pal + "' not found")
        exit(3499)
    }
    replacedata()
}

void `MAIN'::add_palette(| `SS' pal, `RS' n, `RV' o1, `RV' o2, `RV' o3, `RV' o4)
{
    `Int' rc
    
    S = &data1
    if      (args()<=2) rc = Palette(pal, n)
    else if (args()==3) rc = Palette(pal, n, o1)
    else if (args()==4) rc = Palette(pal, n, o1, o2)
    else if (args()==5) rc = Palette(pal, n, o1, o2, o3)
    else                rc = Palette(pal, n, o1, o2, o3, o4)
    if (rc==0) {
        display("{err}palette '" + pal + "' not found")
        exit(3499)
    }
    appenddata()
}

`Bool' `MAIN'::Palette(`SS' pal0, `RS' n0, | `RV' o1, `RV' o2, `RV' o3, `RV' o4)
{
    `Int' t, n
    `SS'  pal
    
    // find palette
    if (palettes==NULL) paletteindex()
    pal = pal0
    t = parse_palette(pal)
    if (t==0) return(0)
    
    // read colors
    n = (n0<. ? (n0<0 ? 0 : trunc(n0)) : 15)
    if      (t==99) Palette_internal(pal, n, o1)   // o1: range
    else if (t==1)  Palette_palettes(pal, n)
    else if (t==2)  Palette_namedcolors(pal)
    else if (t==3)  Palette_lsmaps(pal, n, o1) // o1: range
    else if (t==4)  Palette_generators(pal, n, o1, o2, o3, o4) // o#; h, c, l, p
    else if (t==5)  Palette_rgbmaps(pal, n, o1, 0) // o1: range
    else if (t==11) Palette_palettes(pal, n, "_personal")
    else if (t==12) Palette_namedcolors(pal, "_personal")
    else if (t==13) Palette_lsmaps(pal, n, o1, "_personal")
    else if (t==14) Palette_generators(pal, n, o1, o2, o3, o4, "_personal")
    else if (t==15) Palette_rgbmaps(pal, n, o1, 0, "_personal") // o1: range
    else return(0)
    S->name = pal
    
    // interpolation/recycling
    if (n0<.) { // only if n has been specified by user
        if (n!=(S->n)) { // only if number of returned colors is unequal
            // the number of requested colors; this is only possible for 
            // palettes that have the -noexpand- (o1) argument
            if (anyof(("qualitative","categorical"), S->pclass)) {
                if (n<(S->n)) _recycle(n) // select first n colors
                else if (length(o1)) { // noexpand argument specified
                    if (o1==0) _recycle(n)
                }
                else _recycle(n)
            }
            else if (length(o1)) { // noexpand argument specified
                if (o1==0) _ipolate(n)
                else if (n<(S->n)) _recycle(n) // select first n colors
            }
            else _ipolate(n)
        }
    }
    
    // done
    return(t)
}

`SC' `MAIN'::Palette_read(`SS' pal, `SS' lib)
{
    `PV' T
    
    T = palettes->get(pal)
    if (T[2]==NULL) {
        T[2] = &(_Palette_read(pal, lib))
        palettes->put(pal, T)
    }
    return(*(T[2]))
}

`SC' `MAIN'::_Palette_read(`SS' pal, `SS' lib)
{
    `Int' pos, r, i
    `SS'  fn, line, tn
    `RS'  fh
    `SM'  EOF
    `SC'  f
    
    fn = findfile("colrspace_library_"+lib+".sthlp")
    if (fn=="") {
        display("{err}colrspace_library_"+lib+".sthlp not found")
        exit(error(601))
    }
    EOF = J(0, 0, "")
    tn = "n:"
    fh = fopen(fn, "r")
    while ((line=strltrim(fget(fh)))!=EOF) {
        if (substr(line,1,2)!=tn) continue // not a palette name
        if (strtrim(substr(line,3,.))!=pal) continue // different palette
        // read palette
        pos = ftell(fh)
        r = 0
        while ((line=strltrim(fget(fh)))!=EOF) {
            if (substr(line,1,2)==tn) break // next palette
            r++
        }
        fseek(fh, pos, -1)
        f = J(r,1,"")
        for (i=1;i<=r;i++) f[i] = strltrim(fget(fh))
        break
    }
    fclose(fh)
    if (r) f = ::select(f, substr(f,1,1):!="*") // remove comment lines
    return(f)
}

void `MAIN'::Palette_palettes(`SS' pal, `RS' n, | `SS' personal)
{   // get palette from palettes library
    `Int'  i, j, r, l
    `SS'   s, t, tc, td, ts, tP, tPn, tN, tI
    `SS'   N, I
    `SC'   f, P
    `IntC' nc, p
    
    // read palette from library
    f = Palette_read(pal, "palettes"+personal)
    r = rows(f)
    
    // collect palette information 
    // - count number of color sets
    tP = "P:"; tPn = "P"
    j = 0
    for (i=1;i<=r;i++) {
        s = f[i]
        if      (substr(s,1,2)==tP) j++
        else if (substr(s,1,1)==tPn) j++
    }
    P = J(j,1,""); nc = J(j,1,.)
    // - parse palette
    tc = "c:"; td = "d:"; ts = "s:"; tN = "N:"; tI = "I:"
    j = 0
    for (i=1;i<=r;i++) {
        s = f[i]
        t = substr(s,1,2)
        if      (t==tc) S->pclass = strtrim(substr(s,3,.))
        else if (t==td) S->note   = strtrim(substr(s,3,.))
        else if (t==ts) S->source = strtrim(substr(s,3,.))
        else if (t==tN) N         = strtrim(substr(s,3,.))
        else if (t==tI) I         = strtrim(substr(s,3,.))
        else if (t==tP) P[++j]    = strtrim(substr(s,3,.))
        else if (substr(s,1,1)==tPn) {
            l = strpos(s,":")
            if (l==0) continue // invalid P#:
            l = strtoreal(substr(s,2,l-2))
            if (l>=.) continue // invalid P#:
            nc[++j] = l
            P[j]  = strtrim(substr(s,strpos(s,":")+1,.))
        }
    }
    if (j==0) { // no color set found
        P = J(0,1,""); nc = J(0,1,.)
    } 
    else if (j<rows(nc)) {
        P = P[|1 \ j|]; nc = nc[|1 \ j|]
    }
    
    // process colors
    if      (j==0) colors_set("", ",")  // no colors in palette
    else if (j==1) colors_set(P, ",")   // single color set
    else {
        p = ::order(nc,1)
        for (i=1;i<=j;i++) {
            if (n<nc[p[i]]) {
                colors_set(P[p[i]], ",")
                break
            }
            if (n==nc[p[i]]) {
                colors_set(P[p[i]], ",")
                break
            }
            if (i==j) {
                colors_set(P[p[i]], ",")
            }
        }
    }
    if (N!="") names_set(N, ",")
    if (I!="") info_set(I, ",")
}

void `MAIN'::Palette_namedcolors(`SS' pal0, | `SS' personal)
{   // get palette from namedcolors library
    `Int'  l, i, j, r
    `SS'   pal, t, tc, td, ts, hex
    `SC'   f
    `SM'   P
    
    // read palette from library
    pal = pal0
    if (personal=="") {
        if (substr(pal0,1,9)=="webcolors") pal = "HTML" + substr(pal,10,.)
    }
    f = Palette_read(pal, "namedcolors"+personal)
    r = rows(f)
    
    // process palette
    tc = "c:"; td = "d:"; ts = "s:"; hex = "#"
    P = J(r, 2, "")
    j = 0
    for (i=1;i<=r;i++) {
        t = substr(f[i],1,1)
        if (t==hex) {
            l = strpos(f[i]," ")
            if (l==0) continue // color name is missing
            t = strtrim(substr(f[i],l,.))
            if (t=="") continue // color name is missing
            if (substr(t,-1,1)=="*") { // remove * suffix in name
                t = substr(t,1,strlen(t)-1)
            }
            P[++j,] = (substr(f[i],1,l-1), t)
            continue
        }
        t = substr(f[i],1,2)
        if      (t==tc) S->pclass = strtrim(substr(f[i],3,.))
        else if (t==td) S->note   = strtrim(substr(f[i],3,.))
        else if (t==ts) S->source = strtrim(substr(f[i],3,.))
    }
    if (j==0) P = J(0, 2, "") // no colors found
    else      P = P[|1,1 \ j,.|]
    _set(P[,1], "hex")
    Names_set(P[,2])
}

void `MAIN'::Palette_lsmaps(`SS' pal, `RS' n, `RV' range, | `SS' personal)
{   // get palette from lsmaps library
    `Int'  i, j, r
    `SS'   t, tc, td, ts, tr
    `SC'   f, R, G, B
    `IntR' rr
    
    // read palette from library
    f = Palette_read(pal, "lsmaps"+personal)
    r = rows(f)
    
    // process palette
    tc = "c:"; td = "d:"; ts = "s:"; tr = "r:"
    for (i=1;i<=r;i++) {
        t = substr(f[i],1,2)
        if      (t==tc) S->pclass = strtrim(substr(f[i],3,.))
        else if (t==td) S->note   = strtrim(substr(f[i],3,.))
        else if (t==ts) S->source = strtrim(substr(f[i],3,.))
        else if (t==tr) {
            rr = strtoreal(tokens(substr(f[i],3,.)))
            R = J(rr[1],3,"")
            for (j=1;j<=rr[1];j++) R[j,] = tokens(f[++i])
            G = J(rr[2],3,"")
            for (j=1;j<=rr[2];j++) G[j,] = tokens(f[++i])
            B = J(rr[3],3,"")
            for (j=1;j<=rr[3];j++) B[j,] = tokens(f[++i])
            rgb_set(lsmap(strtoreal(R), strtoreal(G), strtoreal(B), n, range))
            return
        }
    }
}

void `MAIN'::Palette_rgbmaps(`SS' pal, `RS' n, `RV' range, `Bool' shift, | `SS' personal)
{   // get palette from rgbmaps library
    `Int'  i, j, r, rr
    `SS'   t, tc, td, ts, tr
    `SC'   f
    `SM'   RGB
    
    // read palette from library
    f = Palette_read(pal, "rgbmaps"+personal)
    r = rows(f)
    
    // process palette
    tc = "c:"; td = "d:"; ts = "s:"; tr = "r:"
    for (i=1;i<=r;i++) {
        t = substr(f[i],1,2)
        if      (t==tc) S->pclass = strtrim(substr(f[i],3,.))
        else if (t==td) S->note   = strtrim(substr(f[i],3,.))
        else if (t==ts) S->source = strtrim(substr(f[i],3,.))
        else if (t==tr) {
            rr = strtoreal(tokens(substr(f[i],3,.)))
            RGB = J(rr,3,"")
            for (j=1;j<=rr;j++) RGB[j,] = tokens(f[++i])
            if (shift) {
                RGB = RGB[|rows(RGB)/2+1,1 \ .,.|] \ RGB[|1,1 \ rows(RGB)/2,.|]
                RGB = RGB[rows(RGB)::1,]
            }
            if (anyof(("cyclic","circular"), S->pclass))
                 rgb_set(colipolate_c(strtoreal(RGB), n))
            else rgb_set(colipolate(strtoreal(RGB), n, range))
            return
        }
    }
}

void `MAIN'::Palette_generators(`SS' pal, `RS' n, `RV' h, `RV' c, `RV' l, `RV' p, | `SS' personal)
{   // get palette from color generator
    `Int'  r, i
    `SS'   t, tc, td, ts, tP
    `SC'   f
    `RR'   P
    
    // read palette from library
    f = Palette_read(pal, "generators"+personal)
    r = rows(f)
    
    // process palette
    tc = "c:"; td = "d:"; ts = "s:"; tP = "P:"
    for (i=1;i<=r;i++) {
        t = substr(f[i],1,2)
        if      (t==tc) S->pclass = strtrim(substr(f[i],3,.))
        else if (t==td) S->note   = strtrim(substr(f[i],3,.))
        else if (t==ts) S->source = strtrim(substr(f[i],3,.))
        else if (t==tP) P = strtoreal(tokens(substr(f[i],3,.)))
    }
    
    // overwrite defaults by user-specified parameters
    if (cols(P)<8) P = P, J(1,8-cols(P),.)
    if (length(h)) {
        if (h[1]<.) P[1] = h[1]
        if (length(h)>1) {
            if (h[2]<.) P[2] = h[2]
        }
    }
    if (length(c)) {
        if (c[1]<.) P[3] = c[1]
        if (length(c)>1) {
            if (c[2]<.) P[4] = c[2]
        }
    }
    if (length(l)) {
        if (l[1]<.) P[5] = l[1]
        if (length(l)>1) {
            if (l[2]<.) P[6] = l[2]
        }
    }
    if (length(p)) {
        if (p[1]<.) P[7] = p[1]
        if (length(p)>1) {
            if (p[2]<.) P[8] = p[2]
        }
    }
    
    // apply generator
    generate(pal, n, P[(1,2)], P[(3,4)], P[(5,6)], P[(7,8)])
}

void `MAIN'::Palette_internal(`SS' pal, `RS' n, | `RV' range)
{   // get internal palette or alias
    if (pal=="okabe") {
        Palette_palettes("cblind", n)
        // remove gray and update note
        _select((1,3..S->n))
        S->note = substr(S->note,1,strpos(S->note,",")-1)
    }
    else if (pal=="tab10")               Palette_palettes("d3 10", n)
    else if (pal=="tab20")               Palette_palettes("d3 20", n)
    else if (pal=="tab20b")              Palette_palettes("d3 20b", n)
    else if (pal=="tab20c")              Palette_palettes("d3 20c", n)
    else if (pal=="spmap blues")         Palette_spmap("blues", n)
    else if (pal=="spmap greens")        Palette_spmap("greens", n)
    else if (pal=="spmap greys")         Palette_spmap("greys", n)
    else if (pal=="spmap reds")          Palette_spmap("reds", n)
    else if (pal=="spmap rainbow")       Palette_spmap("rainbow", n)
    else if (pal=="HTML")                Palette_htmlcolors()
    else if (pal=="HTML redorange")      Palette_htmlcolors(("red", "orange"))
    else if (pal=="webcolors")           Palette_htmlcolors()
    else if (pal=="webcolors redorange") Palette_htmlcolors(("red", "orange"))
    else if (pal=="twilight shifted")    Palette_rgbmaps("twilight", n, range, 1)
}

void `MAIN'::Palette_htmlcolors(| `SV' keys)
{   // get all HTML colors
    `Int'  i, k
    `IntC' p
    `RM'   C
    `SC'   N, I
    
    C = J(0,3,.); N = J(0,1,""); I = J(0,1,"")
    if (args()) {
        k = length(keys)
        for (i=1;i<=k;i++) {
            Palette_namedcolors("HTML " + keys[i])
            C = C \ S->RGB
            N = N \ S->names
            I = I \ S->info
        }
    }
    else {
        keys = palettes->keys()
        keys = ::select(keys, substr(keys,1,4):=="HTML")
        k = length(keys)
        for (i=1;i<=k;i++) {
            if (_palette_tget(keys[i])!=2) continue // palette is not from namedcolors library
            Palette_namedcolors(keys[i])
            C = C \ S->RGB
            N = N \ S->names
            I = I \ S->info
        }
        p = ::order(N,1)  // sort colors by name
        C = C[p,]; N = N[p,]; I = I[p,]
        // remove doubles
        p = selectindex(N :!= (N[|2\.|] \ N[1]))
        C = C[p,]; N = N[p,]; I = I[p,]
    }
    rgb_set(C)
    S->names = N
    S->info  = I
    S->pclass = "qualitative"
    S->note   = "HTML Colors from from www.w3schools.com"
    S->source = "https://www.w3schools.com/colors/colors_names.asp"
}

void `MAIN'::Palette_spmap(`SS' pal,`RS' n0)
{
    `Int' n
    `RM'  C
    `RC'  p
    
    S->pclass = "sequential"
    S->source = "spmap_color.ado from https://ideas.repec.org/c/boc/bocode/s456812.html"
    n = __clip(n0, 2, 99)
    if (pal=="blues") {
        S->note = "light blue to blue color scheme from the spmap package by Pisati (2007)"
        p = ((1::n):-1) / (n-1)
        C = J(n,1,208), (.2 :+ .8*p), (1 :- .6*p)
        _set(C, "HSV")
    }
    else if (pal=="greens") {
        S->note = "light green to green color scheme from the spmap package by Pisati (2007)"
        p = ((1::n):-1) / (n-1)
        C = (122 :+ 20*p), (.2 :+ .8*p), (1 :- .7*p)
        _set(C, "HSV")
    }
    else if (pal=="greys") {
        S->note = "light gray to black color scheme from the spmap package by Pisati (2007)"
        C = J(n,2,0), (.88 :- .88*((1::n):-1)/(n-1))
        _set(C, "HSV")
    }
    else if (pal=="reds") {
        S->note = "light red to red color scheme from the spmap package by Pisati (2007)"
        p = ((1::n):-1) / (n-1)
        C = (20 :- 20*p), (.2 :+ .8*p), (1 :- rowmax((J(n, 1, 0), 1.2*(p:-.5))))
        _set(C, "HSV")
    }
    else if (pal=="rainbow") {
        S->note = "rainbow color scheme from the spmap package by Pisati (2007)"
        C = (240 :- 240*((1::n):-1)/(n-1)), J(n,2,1)
        _set(C, "HSV")
    }
    else C = J(0,3,.)
    _set(C, "HSV")
}

end

* {smcl}
* {marker gen}{bf:Color generators} {hline}
* {asis}

mata:

void `MAIN'::generate(`SS' pal, `Int' n, `RV' h, `RV' c, `RV' l, `RV' p)
{
    `SS'  space
    
    space = findkey(gettok(pal), ("HUE", "HCL", "LCh", "JMh", "HSV", "HSL")')
    if      (space=="HUE")              generate_HUE(n, h, c[1], l[1], p[1])
    else if (S->pclass=="qualitative")  generate_qual(space, n, h, c[1], l[1])
    else if (S->pclass=="sequential")   generate_seq(space, n, h, c, l, p)
    else if (S->pclass=="diverging")    generate_div(space, n, h, c[1], l, p)
    else if (space=="HSV" & S->pclass=="heat0")    generate_HSV_heat0(n, h, c, l[1])
    else if (space=="HSV" & S->pclass=="terrain0") generate_HSV_terrain0(n, h, c, l)
    else rgb_set(J(0,3,.)) // unknown generator class; do not create any colors
}

void `MAIN'::generate_HUE(`Int' n, `RV' h, `RS' c, `RS' l, `Bool' reverse)
{
    // adopted from pal_hue() from the -scales- package by Hadley Wickham in R
    // see https://github.com/hadley/scales
    `Int' i
    `RS'  h1, h2, dir
    `RM'  C
    
    h1 = h[1]; h2 = h[2]
    if (mod(h2-h1,360) < 1) h2 = h2 - 360/n
    dir = (reverse & reverse<. ? -1 : 1)
    C = J(n, 1, (., c, l))
    for (i=1; i<=n; i++) {
         C[i,1] = mod((h1 + (n<=1 ? 0 : (i-1) * (h2-h1) / (n-1))) * dir, 360) 
    }
    _set(C, "HCL")
}

void `MAIN'::generate_qual(`SS' space, `Int' n, `RV' h, `RS' c, `RS' l)
{
    `Int' i
    `RM'  C
    `RS'  h1, h2
    
    h1 = h[1]; h2 = h[2]
    if (h2>=.) h2 = h1 + 360*(n-1)/n
    C = J(n, 3, .)
    for (i=1; i<=n; i++) {
        C[i,] = (mod(n==1 ? h1 : h1 + (i-1)*(h2-h1)/(n-1), 360), c, l)
    }
    if (space=="LCh" | space=="JMh") C = C[,(3,2,1)]
    _set(C, space)
}

void `MAIN'::generate_seq(`SS' space, `Int' n, `RV' h, `RV' c, `RV' l, `RV' p)
{
    `Int' i, j
    `RM'  C
    `RS'  h1, h2, c1, c2, l1, l2, p1, p2
    
    h1 = h[1]; h2 = h[2]
    if (h2>=.) h2 = h1
    c1 = c[1]; c2 = c[2]
    if (c2>=.) c2 = c1
    l1 = l[1]; l2 = l[2]
    if (l2>=.) l2 = l1
    p1 = p[1]; p2 = p[2]
    if (p2>=.) p2 = p1
    C = J(n, 3, .)
    for (i=1; i<=n; i++) {
        j = (n==1 ? 1 : (n-i)/(n-1))
        C[i,] = (mod(h2 - j*(h2-h1), 360), c2 - j^p1*(c2-c1), l2 - j^p2*(l2-l1))
    }
    if (space=="LCh" | space=="JMh") C = C[,(3,2,1)]
    _set(C, space)
}

void `MAIN'::generate_div(`SS' space, `Int' n, `RV' h, `RS' c, `RV' l, `RV' p)
{
    `Int' i, j
    `RM'  C
    `RS'  h1, h2, l1, l2, p1, p2
    
    h1 = h[1]; h2 = h[2]
    if (h2>=.) h2 = h1
    l1 = l[1]; l2 = l[2]
    if (l2>=.) l2 = l1
    p1 = p[1]; p2 = p[2]
    if (p2>=.) p2 = p1
    C = J(n, 3, .)
    for (i=1; i<=n; i++) {
        j = (n==1 ? 1 : (n - 2*i + 1)/(n-1))
        C[i,] = (mod(j>0 ? h1 : h2, 360), c * abs(j)^p1, l2 - abs(j)^p2*(l2-l1))
    }
    if (space=="LCh" | space=="JMh") C = C[,(3,2,1)]
    _set(C, space)
}

void `MAIN'::generate_HSV_heat0(`Int' n, `RV' h, `RV' s, `RS' v)
{
    `Int' i, n1, n2
    `RS'  h1, h2, s1, s2
    `RM'  C
    
    n2 = trunc(n/4)
    n1 = n - n2
    h1 = h[1]; h2 = h[2]; s1 = s[1]; s2 = s[2]
    C = J(n, 3, .)
    for (i=1; i<=n1; i++) {
        C[i,] = (mod(h1 + (n1==1 ? 0 : (i-1)*(h2-h1)/(n1-1)), 360), s1, v)
    }
    for (; i<=n; i++) {
        C[i,] = (mod(h2, 360), s1 - (s1-s2)/(2*n2) + 
        (n2==1 ? 0 : (i-n1-1) * (s2 - s1 + (s1-s2)/n2) / (n2-1)), v)
    }
    _set(C, "HSV")
}

void `MAIN'::generate_HSV_terrain0(`Int' n, `RV' h, `RV' s, `RV' v)
{
    `Int' i, n1, n2, h1, h2, h3, s1, s2, v1, v2, v3
    `RM'  C

    n1   = trunc(n / 2)
    n2   = n - n1 + 1
    h1 = h[1]; h3 = h[2]; h2 = (h1 + h3) / 2
    s1 = s[1]; s2 = s[2]
    v1 = v[1]; v2 = v[2]; v3 = v2 + (1 - v2) / 2
    C = J(n, 3, .)
    for (i=1; i<=n1; i++) {
        C[i,] = (mod(h1 + (n1==1 ? 0 : (i-1)*(h2-h1)/(n1-1)), 360), 
                 s1, v1 + (n1==1 ? 0 : (i-1)*(v2-v1)/(n1-1)))
    }
    for (; i<=n; i++) {
        C[i,] = (mod(h2 + (i-n1)*(h3-h2)/(n2-1), 360),
                 s1 + (i-n1)*(s2-s1)/(n2-1), 
                 v2 + (i-n1)*(v3-v2)/(n2-1))
    }
    _set(C, "HSV")
}

end

exit
