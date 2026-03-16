*! version 1.2.8  11dec2025  Ben Jann

if c(stata_version)<14.2 {
    di as err "{bf:colorpalette} requires version 14.2 of Stata" _c
    if c(stata_version)>=9.2 {
        di as err "; use command {helpb colorpalette9}" _c
    }
    di as err ""
    error 9
}

capt findfile lcolrspace.mlib
if _rc {
    di as error "-colrspace- is required; type {stata ssc install colrspace}"
    error 499
}

program colorpalette
    version 14.2
    capt _on_colon_parse `0'
    if _rc==0 {
        local 0 `"`s(after)'"'
        local opts `"`s(before)'"'
        _parse comma lhs opts : opts
        if `"`lhs'"'!="" error 198
        Parse_palettes `0'
        forv i = 1/`r(n)' {
            local palettes `"`palettes'`"`r(p`i')'"' "'
        }
        Graph2 `palettes' `opts'
        exit
    }
    Parse_palettes `0'
    if r(n)==1 {
        Get_Global_Opts `0'
        Palette_Get `0'
    }
    else {
        local n = r(n) - 1
        forv i = 1/`n' {
            Global_Opts_Not_Allowed `r(p`i')'
            local palettes `"`palettes'`"`r(p`i')'"' "'
        }
        Get_Global_Opts `r(p`r(n)')'
        _parse comma p 0 : 0
        if `"`p'"'!="" { // last element has palette
            local palettes `"`palettes'`"`p'`0'"'"'
            local 0
        }
        Palette_Get2 `palettes' `0'
    }
    if `"`GLOBALS'`GLOBALS2'"'!="" {
        if "`GRAPH'"=="" local NOGRAPH nograph
        Macros global, `GLOBALS2'
    }
    if `"`LOCALS'`LOCALS2'"'!="" {
        if "`GRAPH'"=="" local NOGRAPH nograph
        Macros local, `LOCALS2'
        di _n as txt "locals:"
        foreach name of local mnames {
            gettoken p mvalues : mvalues
            c_local `name' `"`p'"'
            di as txt %22s "`name'" " : " as res `"`p'"'
        }
    }
    if `"`STYLEFILES'`STYLEFILES2'"'!="" {
        if "`GRAPH'"=="" local NOGRAPH nograph
        Parse_Stylefiles, `STYLEFILES2'
        Macros local, `LOCALS2'
        Stylefiles `"`mnames'"' `"`mvalues'"' `"`STYLEFILES2'"'
    }
    if "`NOGRAPH'"=="" {
        tempname hcurrent
        _return hold `hcurrent'
        _return restore `hcurrent', hold // make copy
        Graph, `GROPTS'
        _return restore `hcurrent'
    }
end

/*----------------------------------------------------------------------------*/
/* retrieve palette(s)                                                        */
/*----------------------------------------------------------------------------*/

program Parse_palettes, rclass
    local i 0
    while (`"`0'"'!="") {
        gettoken p 0 : 0, parse("/") quotes bind // "..." is separate token
        if `"`p'"'=="/" {
            return local p`++i' `"`palette'"'
            local palette
            local space
            continue
        }
        local palette `"`palette'`space'`p'"'
        local space " "
    }
    if `"`palette'"'!="" {
        return local p`++i' `"`palette'"'
    }
    return scalar n = max(1,`i')
end

program Get_Global_Opts
    _parse comma lhs 0 : 0
    syntax [, ///
        GLobals GLobals2(passthru) ///
        LOCals LOCals2(passthru) ///
        STYLEFiles STYLEFiles2(passthru) ///
        NOGRaph GRAPH ///
        GRopts(str asis) TItle(passthru) rows(passthru) ///
        names NOINFO NONUMbers * ]
    c_local GLOBALS     `globals'
    c_local GLOBALS2    `globals2'
    c_local LOCALS      `locals'
    c_local LOCALS2     `locals2'
    c_local STYLEFILES  `stylefiles'
    c_local STYLEFILES2 `stylefiles2'
    c_local NOGRAPH     `nograph'
    c_local GRAPH       `graph'
    c_local GROPTS      `gropts' `title' `rows' `names' `noinfo' `nonumbers'
    if `"`options'"'!="" local options `", `options'"'
    c_local 0 `"`lhs'`options'"'
end

program Global_Opts_Not_Allowed
    Get_Global_Opts `0'
    local 0 , `GLOBALS' `GLOBALS2' `LOCALS' `LOCALS2' /*
        */`STYLEFILES' `STYLEFILES2' `NOGRAPH' `GRAPH' `GROPTS'
    syntax [, _somew3irednamedopt ]
end

program Palette_Get2, rclass
    _parse comma palettes 0 : 0
    syntax [, name(str) * ]
    local space
    local i 0
    foreach p of local palettes {
        _parse comma pnm popts : p
        if `"`popts'"'=="" local popts ,
        Palette_Get `pnm' `popts' `options'
        local plist `"`plist'`space'`r(p)'"'
        local space " "
        forv j = 1/`r(n)' {
            local ++i
            return local p`i' `"`r(p`j')'"'
            return local p`i'name  `"`r(p`j'name)'"'
            return local p`i'info  `"`r(p`j'info)'"'
        }
    }
    return scalar n = `i'
    return local p `"`plist'"'
    if `"`name'"'!="" {
        return local pname `"`name'"'
    }
    else {
        return local pname "custom"
    }
    return local ptype "color"
end

program Palette_Get, rclass
    local opts N(numlist max=1 integer >=1) ///
        Select(numlist integer) drop(numlist integer) ///
        order(numlist integer) REVerse shift(numlist max=1) ///
        INtensity(numlist >=0 missingokay) ///
        OPacity(numlist int >=0 missingokay) ///
        IPolate(str) ///
        intensify(numlist >=0 missingokay) ///
        saturate(str) luminate(str) ///
        GScale GScale2(str) ///
        CBlind CBlind2(str) ///
        TORDer(str) ///
        IConvert IConvert2(str) ///
        FORCErgb NOEXPAND class(str) name(str) ///
        Return(str)
    syntax [anything(name=palette id="palette" everything equalok)] ///
        [, `opts' ///
        /// palette-specific options
        Hue(passthru) Chroma(passthru) SATuration(passthru) ///
        Luminance(passthru) VALue(passthru)  ///
        POWer(passthru) DIRection(passthru)  ///
        RAnge(passthru) * ]
    remove_repeatedopts `"`opts'"' `", `options'"'
    local passthruopts `hue' `chroma' `saturation' `luminance' `value' /*
        */`power' `direction' `range' `options'
    parse_psopts, `hue' `chroma' `saturation' `luminance' `value' /*
        */`power' `direction' `range'
    if `"`opacity'"'!="" {
        if c(stata_version)<15 {
            di as err "{bf:opacity()} requires Stata 15"
            exit 9
        }
    }
    if `"`select'"'!="" {
        if `"`drop'"'!="" {
            di as err "only one of select() and drop() allowed"
            exit 198
        }
        if `"`order'"'!="" {
            di as err "only one of select() and order() allowed"
            exit 198
        }
    }
    if `"`ipolate'"'!=""    parse_ipolate `ipolate'
    if `"`saturate'"'!=""   parse_saturate `saturate'
    if `"`luminate'"'!=""   parse_luminate `luminate'
    if `"`gscale2'"'!=""    local gscale gscale
    if "`gscale'"!=""       parse_gscale `gscale2'
    if `"`cblind2'"'!=""    local cblind cblind
    if "`cblind'"!=""       parse_cblind `cblind2'
    parse_torder `torder'
    if `"`iconvert2'"'!=""  local iconvert iconvert
    if "`iconvert'"!=""     parse_iconvert, `iconvert2'
    if `"`class'"'!=""      capture parse_class, `class'
    if `"`return'"'!=""     parse_return `return'
    // get colors
    local ptype 0
    if `"`palette'"'=="" { // use s2 if palette is empty
        if c(stata_version)<18 local palette s2
        else                   local palette st
        local ptype 2
    }
    else {  // check whether palette is mata(name)
        capt parse_mata, `palette' // returns local mataname
        if _rc==0 local ptype 3
    }
    if `ptype'==0 { // check whether palette is list of colors in (...)
        gettoken pal rest : palette, match(paren)
        if "`paren'"=="(" {
            if `"`rest'"'!="" local palette `"`pal' `rest'"'
            else              local palette `"`pal'"'
            local ptype 1
        }
    }
    if `ptype'==0 { // check whether palette has more than 3 words (ColrSpace
                    // currently has no palette names with more than 3 words)
        if `:list sizeof palette'>3 {
            local ptype 1
        }
    }
    mata: getpalette(strtoreal("`n'"), `ptype')
    // return palette
    return local cspace `"`r_cspace'"'
    return local c `"`r_c'"'
    local i 0
    foreach p of local plist {
        local ++i
        gettoken pnm pnames : pnames
        gettoken pi pinfo : pinfo
        return local p`i' `"`p'"'
        return local p`i'name `"`pnm'"'
        return local p`i'info `"`pi'"'
    }
    return local p       `"`plist'"'
    return local pclass  `"`class'"'
    return local psource `"`source'"'
    return local pnote   `"`note'"'
    if `"`name'"'!="" {
        local palette `"`name'"'
    }
    return local pname   `"`palette'"'
    return local ptype   "color"
    return scalar n      = `i'
end

program remove_repeatedopts
    args opts 0
    syntax [, `opts' * ]
    c_local options `"`options'"'
end

program _Palette_Get
    gettoken palette 0 : 0, parse(" ,")
    syntax [, n(numlist max=1 integer >0) * ]
    colorpalette_`palette', n(`n') `options'
    if `"`P'"'=="" { // palettes that define P#
        local min 1
        while (`"`P`min''"'=="") {
            local ++min
            if `min'>100 {
                c_local n 0
                exit // emergency exit
            }
        }
        local max `min'
        while (`"`P`max''"'!="") {
            local ++max
        }
        local --max
        if "`n'"=="" local n `max'
        local n = max(`min',min(`max',`n'))
        local P `"`P`n''"'
    }
    c_local plist  `"`P'"'
    c_local pnames `"`N'"'
    c_local pinfo  `"`I'"'
    if `"`class'"'!="" c_local class  `"`class'"'
    if `"`name'"'!=""  c_local palette `"`name'"'
    c_local note   `"`note'"'
    c_local source `"`source'"'
end

program parse_ipolate
    _parse comma n opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", n(`n') `opts'"'
    syntax [, n(numlist max=1 integer >=1) RAnge(numlist max=2) ///
        POWer(numlist max=1 >0) POSitions(numlist) PADded * ]
    if "`n'"=="" exit // no number specified
    c_local ipolate_n `n'
    c_local ipolate_range "`range'"
    c_local ipolate_power "`power'"
    c_local ipolate_positions "`positions'"
    c_local ipolate_pad "`padded'"
    c_local ipolate_space `"`options'"'
end

program parse_saturate
    _parse comma p opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", p(`p') `opts'"'
    syntax [, p(numlist missingokay) level * ]
    if "`p'"=="" exit // no numbers specified
    c_local saturate_p `"`p'"'
    c_local saturate_level "`level'"
    c_local saturate_method `"`options'"'
end

program parse_luminate
    _parse comma p opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", p(`p') `opts'"'
    syntax [, p(numlist missingokay) level * ]
    if "`p'"=="" exit // no numbers specified
    c_local luminate_p `"`p'"'
    c_local luminate_level "`level'"
    c_local luminate_method `"`options'"'
end

program parse_gscale
    _parse comma p opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", gscale(`p') `opts'"'
    syntax [, gscale(numlist >=0 <=1 missingokay) noIConvert * ]
    if "`gscale'"=="" local gscale 1
    c_local gscale_p `gscale'
    c_local gscale_noic "`iconvert'"
    c_local gscale_method `"`options'"'
end

program parse_cblind
    _parse comma p opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", cblind(`p') `opts'"'
    syntax [, cblind(numlist >=0 <=1 missingokay) noIConvert * ]
    if "`cblind'"=="" local cblind 1
    c_local cblind_p `cblind'
    c_local cblind_noic "`iconvert'"
    c_local cblind_method `"`options'"'
end

program parse_torder
    local KW IPolate INtensify SATurate LUMinate GScale CBlind
    local O0 = strlower("`KW'")
    local o0: copy local 0
    foreach o of local o0 {
        local 0 `", `o'"'
        syntax [, `KW' ]
        foreach O of local O0 {
            local ord `ord' ``O''
        }
    }
    local ord: list uniq ord
    local ord: list ord | O0
    c_local torder `ord'
end

program parse_iconvert
    syntax [, First Last IPolate INtensify SATurate LUMinate ]
    local iconvert `first' `last' `ipolate' `intensify' `saturate' `luminate'
    if `: list sizeof iconvert'>1 {
        di as err "invalid iconvert(): only one argument allowed"
        exit 198
    }
    if "`iconvert'"=="" local iconvert first
    c_local iconvert `iconvert'
end

program parse_class
    syntax [, Qualitative CATegorical Sequential Diverging CYClic CIRCular  ]
    local class `qualitative' `categorical' `sequential' `diverging' `cyclic' `circular'
    if `:list sizeof class'>1 exit 198
    c_local class `class'
end

program parse_return
    syntax anything(id="{it:cspace}") [, format(str) ]
    if `"`format'"'=="" local format %9.0g
    else confirm numeric format `format'
    c_local r_cspace `anything'
    c_local r_fmt `format'
end

program parse_mata
    syntax, Mata(name)
    c_local mataname `"`mata'"'
end

program parse_psopts // options for color generators and matplotlib colormaps
    syntax [, Hue(numlist max=2) ///
        Chroma(numlist max=2 >=0) SATuration(numlist max=2 >=0 <=1) ///
        Luminance(numlist max=2 >=0 <=100) VALue(numlist max=2 >=0 <=1) ///
        POWer(numlist max=2 >0) DIRection(numlist int max=1) /// 
        RAnge(numlist max=2 >=0 <=1) ]
    if "`saturation'"!="" {
        if "`chroma'"!="" &  {
            di as err "chroma() and saturation() not both allowed"
            exit 198
        }
        local chroma `saturation'
    }
    if "`value'"!="" {
        if "`luminance'"!="" &  {
            di as err "luminance() and value() not both allowed"
            exit 198
        }
        local luminance `value'
    }
    if "`direction'"!="" {
        if "`power'"!="" &  {
            di as err "power() and direction() not both allowed"
            exit 198
        }
        if !inlist(`direction',1,-1) {
            di as err "{bf:direction()} must be 1 or -1"
            exit 198
        }
        local power = (`direction'==-1)
    }
    c_local hue `hue'
    c_local chroma `chroma'
    c_local luminance `luminance'
    c_local power `power'
    c_local range `range'
end

program parse_optnotallowed
    syntax [, _somew3irednamedopt ]
end

/*----------------------------------------------------------------------------*/
/* return macros                                                              */
/*----------------------------------------------------------------------------*/

program Macros
    // syntax
    _parse comma macro 0 : 0
    if "`macro'"=="local" {
        local lmax 31
        local uscore "_"
    }
    else {
        local lmax 32
        local uscore
    }
    syntax [, `macro's2(str) ]   // get contents of option
    local 0 `"``macro's2'"'
    capt n syntax [anything] [, NONames Prefix(str) Suffix(str) ]
    if _rc==1 exit _rc
    if _rc {
        di as err "(error in option {bf:`macro's()})"
        exit _rc
    }
    if `"`prefix'"'!="" {
        local 0 `", prefix(`uscore'`prefix')"'
        capt n syntax [, prefix(name) ]
        if _rc==1 exit _rc
        if _rc {
            di as err "(error in option {bf:`macro's()})"
            exit _rc
        }
        if "`macro'"=="local" {
            local prefix = substr("`prefix'",2,.) // remove "_"
        }
        else if substr("`prefix'",1,1)=="_" {
            di as err "global macro name may not start with '_'"
            di as err "(error in option {bf:`macro's()})"
            exit 198
        }
    }
    while (`"`anything'"'!="") {
        gettoken name anything : anything, quotes
        if `"`anything'"'=="" { // last element
            if substr(`"`name'"',-1,1)=="*" {
                local name = substr(`"`name'"',1,strlen(`"`name'"')-1)
                capt confirm name `uscore'`name'
                if _rc==1 exit _rc
                if _rc {
                    di as err "'" `"`name'*"' "' not allowed in {bf:`macro's()}"
                    exit _rc
                }
                local prefix1 `name'
                if "`macro'"!="local" {
                    if substr("`prefix1'",1,1)=="_" {
                        di as err "global macro name may not start with '_'"
                        di as err "(error in option {bf:`macro's()})"
                        exit 198
                    }
                }
                continue, break
            }
        }
        if "`prefix'"!="" {
            local name `prefix'`name'
            local name = substr("`name'",1,`lmax')
        }
        capt n confirm name `uscore'`name'
        if _rc==1 exit _rc
        if _rc {
            di as err "(error in option {bf:`macro's()})"
            exit _rc
        }
        if "`macro'"!="local" {
            if substr("`name'",1,1)=="_" {
                di as err "global macro name may not start with '_'"
                di as err "(error in option {bf:`macro's()})"
                exit 198
            }
        }
        local names `names' `name'
    }
    if `"`prefix1'"'=="" local prefix1 `prefix'
    if "`macro'"!="local" & `"`prefix1'"'=="" local prefix1 "p"
    if `"`suffix'"'!="" {
        local 0 `", suffix(_`suffix')"'
        capt n syntax [, suffix(name) ]
        if _rc==1 exit _rc
        if _rc {
            di as err "(error in option {bf:`macro's()})"
            exit _rc
        }
        local suffix = substr(`"`suffix'"',2,.)
    }
    
    // return macros
    local n = r(n)
    local ls = strlen(`"`suffix'"')
    local prefix1 = substr("`prefix1'",1,`lmax'-floor(log10(`n')))
    forv i = 1/`n' {
        local p `"`r(p`i')'"'
        if `: list sizeof p'>1 {
            local p `""`p'""'
        }
        gettoken name names : names
        if "`name'`nonames'"=="" {
            local pname `"`r(p`i'name)'"'   // name available in palette
            if `"`pname'"'!="" {
                local name = ustrtoname(`"`uscore'`pname'"', 0)
                if "`macro'"=="local" {
                    local name = substr(`"`name'"',2,.)
                }
            }
            else {
                capt confirm name `p' // color code is a name
                if _rc==0 {
                    local name `prefix'`p'
                    local name = substr("`name'",1,`lmax')
                }
            }
        }
        if "`name'"=="" local name `prefix1'`i'
        if `ls' {
            local name = substr("`name'",1,`lmax'-`ls')
            local name `name'`suffix'
        }
        local mnames `mnames' `name'
        local mvalues `"`mvalues' `"`p'"'"'
    }
    if "`macro'"=="local" {
        c_local mnames `mnames'
        c_local mvalues `"`mvalues'"'
        exit
    }
    di _n as txt "globals:"
    foreach name of local mnames {
        gettoken p mvalues : mvalues
        global `name' `"`p'"'
        di as txt %22s "`name'" " : " as res `"${`name'}"'
    }
end

/*----------------------------------------------------------------------------*/
/* write style files                                                          */
/*----------------------------------------------------------------------------*/

program Parse_Stylefiles
    syntax [, stylefiles2(str) ]   // get contents of option
    local 0 `"`stylefiles2'"'
    capt n syntax [anything] [, PERsonal path(passthru) replace * ]
    if _rc==1 exit _rc
    if _rc {
        di as err "(error in option {bf:stylefiles()})"
        exit _rc
    }
    if "`personal'"!="" {
        if `"`path'"'!="" {
            di as err "{bf:personal} and {bf:path()} not both allowed"
            di as err "(error in option {bf:stylefiles()})"
            exit 198
        }
        local path "personal"
    }
    if `"`options'"'!="" {
        local options `", `options'"'
    }
    local locals2 `"`anything'`options'"'
    if `"`locals2'"'!="" {
        local locals2 locals2(`locals2')
    }
    c_local LOCALS2 `"`locals2'"'
    c_local STYLEFILES2 `"`path' `replace'"'
end

program Stylefiles
    // syntax
    args names values opts
    local 0 `", `opts'"'
    syntax [, PERsonal path(str) replace ]
    
    // determine path 
    if "`personal'"!="" {
        mata: st_local("path", pathjoin(pathsubsysdir("PERSONAL"),"style"))
    }
    else if `"`path'"'=="" {
        local path "style"
    }
    mata: colorpalette_mkdir(st_local("path"))
    
    // check existing files
    if "`replace'"=="" {
        foreach name of local names {
            local fn `"color-`name'.style"'
            mata: st_local("fn", pathjoin(st_local("path"), st_local("fn")))
            capt n confirm new file `"`fn'"'
            if _rc==1 exit _rc
            if _rc {
                di as err "specify {bf:stylefiles(, replace)} to overwrite existing files"
                exit _rc
            }
        }
    }
    
    // write style files
    local i 0
    tempname fh
    di _n as txt "color styles:"
    foreach name of local names {
        gettoken p values : values
        capt numlist `p', int min(3) max(3) range(>=0 <=255)
        if _rc {
            di as txt %22s "`name'" " : (color definition not RGB; skipped)"
            continue
        }
        local ++i
        local fn `"color-`name'.style"'
        mata: st_local("fn", pathjoin(st_local("path"), st_local("fn")))
        quietly file open `fh' using `"`fn'"', write replace
        file write `fh' `"set rgb `p'"'
        file close `fh'
        di as txt %22s "`name'" " : " as res `"`p'"'
    }
    if `i' {
        di _n as txt `"(style files written to directory `path')"'
    }
    else {
        di _n as txt `"(no style files written)"'
    }
end

/*----------------------------------------------------------------------------*/
/* graph of single palette                                                    */
/*----------------------------------------------------------------------------*/

program Graph
    syntax [, rows(int 5) TItle(passthru) names NOINFO NONUMbers * ]
    local n = r(n)
    local c = max(3,ceil(sqrt(`n'/12*3)))
    local cut = max(`rows',ceil(`n'/`c'))
    local rows = max(5, `cut')
    local c = max(3,ceil(`n'/`rows'))
    local size = (100-10)/(1.5*`rows')
    local lblgap = `size'/6
    local infogap = `size'/3.75
    local rgap = (100-5)/`c'
    local j 1
    local r 0
    forv i=1/`n' {
        if `i'>(`cut'*`j') {
            local ++j
            local r 0
        }
        local ++r
        local pi `"`r(p`i')'"'
        if `"`pi'"'=="" continue
        local jlab `j'
        local plots `plots' (scatteri `r' `j', mlw(vthin) mlc(black) ///
            msymbol(square) msize(`size') mfcolor(`"`pi'"'))
        local pnum `pnum' `r' `j' "`i'"
        local pinfo `"`r(p`i'name)'"'
        local plbl
        if "`names'"!="" {
            if `"`pinfo'"'!="" {
                local plbl `"`pinfo'"'
                local pinfo
            }
        }
        if `"`plbl'"'=="" local plbl `"`pi'"'
        local lbl `lbl' `r' `jlab' `"`plbl'"'
        if `"`pinfo'"'==`"`pi'"' local pinfo    // do not repeat names
        if `"`pinfo'"'=="" local pinfo `"`r(p`i'info)'"' // use info if no name
        if `"`pinfo'"'!="" {
            local info `info' `r' `jlab' `"`pinfo'"'
        }
    }
    if `"`plots'"'=="" {
        di as txt "(nothing to display)"
        exit
    }
    if `rows'>=30 {
        local pnumsize tiny
        local lblsize tiny
        local infosize half_tiny
    }
    else if `rows'>=15 {
        local pnumsize small
        local lblsize vsmall
        local infosize tiny
    }
    else if `rows'>=10 {
        local pnumsize small
        local lblsize small
        local infosize vsmall
    }
    else {
        local pnumsize medium 3.8194
        local lblsize medsmall
        local infosize small
    }
    if "`nonumbers'"=="" {
        local pnum (scatteri `pnum', ms(i) msize(`size') mlabpos(9) ///
            mlabgap(`lblgap') mlabsize(`pnumsize') mlabcolor(black))
    }
    else local pnum
    if `"`lbl'"'!="" {
        local lbl (scatteri `lbl', ms(i) msize(`size') mlabpos(3) ///
            mlabgap(`lblgap') mlabsize(`lblsize') mlabcolor(black))
    }
    if "`noinfo'"=="" {
        if `"`info'"'!="" {
            local info (scatteri `info', ms(i) msize(`size') mlabpos(4) ///
                mlabgap(`infogap') mlabsize(`infosize') mlabcolor(black))
        }
    }
    else local info
    local l = `size'/2 + cond("`nonumbers'"=="", 9, 5)
    local r = `size'/2 + `rgap'
    local b = `size'/2 + 5
    local t = `size'/2 + 4
    if `"`title'"'=="" {
        local title title(`"`r(pname)'"')
    }
    two `plots' `pnum' `lbl' `info' , `title' scheme(s2color) ///
        legend(off) ylabel(none) graphr(color(white)) ///
        xlabel(none) xscale(range(1 3) off) ///
        yscale(range(1 `rows') off reverse) ///
        plotr(margin(`l' `r' `b' `t')) graphr(margin(0 0 0 3)) ///
        `source' `options'
end

/*----------------------------------------------------------------------------*/
/* graph of multiple palettes                                                 */
/*----------------------------------------------------------------------------*/

program Graph2
    _parse comma palettes 0 : 0
    syntax [, TItle(passthru) LABels(str asis) PLabels(str asis) ///
        NONUMbers GRopts(str asis) LColor(str) LWidth(str) ///
        VERTical HORizontal span BARWidth(numlist max=1) * ]
    if "`barwidth'"=="" local barwidth 0.7
    if `"`labels'"'!="" local plabels `"`labels'"'
    local orientation `vertical' `horizontal'
    if "`orientation'"=="" local orientation horizontal
    if `"`lcolor'"'!="" {
        local lcolor lc(`lcolor' ..) 
        if c(stata_version)>=15 local lcolor `lcolor' lalign(center ..)
    }
    if `"`lwidth'"'!="" {
        local lwidth lw(`lwidth' ..) 
    }
    else local lwidth lw(vthin ..)
    local np: list sizeof palettes
    local r = 4 * `np'
    if (_N > `r') {
        preserve
        qui keep in 1/`r'   // remove extra observations to speed up
    }
    else if (_N < `r') {
        preserve
        qui set obs `r'
    }
    tempvar y
    qui generate `y' = ceil(_n/4) - `barwidth'/2 + /*
        */ inlist(mod(_n-1,4)+1,3,4)*`barwidth' in 1/`r'
    if "`span'"!="" {
        tempvar psize
        qui generate `psize' = .
    }
    local nxvars 0
    local i 0
    local plots
    local ylab
    foreach p of local palettes {
        local ++i
        _parse comma pnm popts : p
        if `"`pnm'"'=="." continue
        if `"`popts'"'=="" local popts ,
        Palette_Get `pnm' `popts' `options'
        local colors `"`r(p)'"'
        local n = r(n)
        gettoken plab plabels : plabels
        if `"`plab'"'=="" {
            local plab `"`r(pname)'"'
        }
        local ylab `ylab' `i' `"`plab'"'
        while (`nxvars'<`n') {
            local xx0 `xx`nxvars''
            if mod(`nxvars',20)==0 local xx0
            local ++nxvars
            tempvar x`nxvars'
            local xx`nxvars' `xx0' `x`nxvars''
        }
        local from = (`i' - 1) * 4 + 1
        local to = `i' * 4
        if "`span'"!="" {
            qui replace `psize' = `n' in `from'/`to'
        }
        local n0 0
        while (`n0'<`n') {
            local ctmp
            while (1) {
                local ++n0
                gettoken ci colors : colors, quotes
                local ctmp `"`ctmp'`ci' "'
                if `n0'==`n' continue, break
                if mod(`n0',20)==0 continue, break
            }
            local plots `plots' ///
                (scatter `xx`n0'' `y' in `from'/`to', color(`ctmp') ///
                `lcolor' `lwidth' fintensity(100 ..) ///
                recast(area) `orientation' nodropbase)
        }
    }
    if `"`plots'"'=="" {
        di as txt "(noting to display)"
        exit
    }
    forv i=1/`nxvars' {
        qui gen `x`i'' = `i' + inlist(mod(_n-1,4)+1,2,3) - .5
    }
    if "`span'"!="" {
        forv i=1/`nxvars' {
            qui replace `x`i'' = (`x`i'' - .5) / `psize'
        }
    }
    if "`nonumbers'"=="" {
        local xlab = ceil(`nxvars'/20)
        numlist "1(`xlab')`nxvars'"
        local xlab `r(numlist)'
    }
    else local xlab none
    if "`orientation'"=="horizontal" {
        if "`span'"!="" {
            local xscale xscale(off range(0 1))
            local xlabel
        }
        else {
            local xscale xscale(lstyle(none) range(1 `nxvars'))
            local xlabel xlabel(`xlab', notick)
        }
        local yscale yscale(lstyle(none) range(0.65 `np'.35) reverse)
        local ylabel ylabel(`ylab', nogrid notick angle(hor))
    }
    else {
        local xscale xscale(lstyle(none) range(0.65 `np'.35) alt)
        local xlabel xlabel(`ylab', notick)
        if "`span'"!="" {
            local yscale yscale(off range(0 1) reverse)
            local ylabel ylabel(, nogrid)
        }
        else {
            local yscale yscale(lstyle(none) range(1 `nxvars') reverse)
            local ylabel ylabel(`xlab', nogrid notick angle(hor))
        }
    }
    twoway `plots', `xscale' `xlabel' xti("") `yscale' `ylabel' yti("") ///
        legend(off) graphr(margin(l=2 t=2 b=1 r=2) color(white)) ///
        scheme(s2color) `title' `gropts'
end

/*----------------------------------------------------------------------------*/
/* mata                                                                       */
/*----------------------------------------------------------------------------*/

version 14
mata:
mata set matastrict on

void colorpalette_mkdir(path)
{
    real scalar      i
    string scalar    d
    string rowvector dlist
    pragma unset     d
    pragma unset     dlist
    
    if (direxists(path)) return
    if (path=="") return
    printf("{txt}directory %s does not exist\n", path)
    printf("{txt}press any key to create the directory, or Break to abort\n")
    more()
    while (1) {
        pathsplit(path, path, d)
        dlist = dlist, d
        if (path=="") break
        if (direxists(path)) break
    }
    for (i=cols(dlist); i>=1; i--) {
        path = pathjoin(path, dlist[i])
        mkdir(path)
    }
}

void getpalette(real scalar n, real scalar ptype)
{
    real scalar      ip_n, rc, ti, tn, ic
    string scalar    pal, libname, iconv
    string rowvector torder
    class ColrSpace scalar S
    pragma unset     S
    pointer scalar   p

    // Step 1: determine type of palette and parse additional options
    //    ptype 0 = <not defined>
    //          1 = color list
    //          2 = ColrSpace palette
    //          3 = mata() object (already set)
    //          4 = colorpalette_<palette>.ado
    pal = st_local("palette")
    if (ptype==0) {
        // check whether palette exists in ColrSpace
        checkpalette(S, ptype, pal)
    }
    if (ptype==0) {
        // by now, if palette has multiple words, it must be a color list
        if (length(tokens(pal))>1) ptype = 1
    }
    if (ptype==0) {
        // if only one word: check whether resulting program name is valid
        if (_stata("confirm name colorpalette_"+pal, 1)) ptype = 1
    }
    if (ptype==0) {
        // check whether palette program exists; if yes: run it
        if (_stata("local junk: properties colorpalette_"+pal, 1)) ptype = 1
        else {
            rc = _stata("_Palette_Get "+pal+", n(\`n') \`passthruopts'")
            if (rc) exit(rc)
            st_local("options", "")
            ptype = 4
        }
    }
    // make sure no extra options are left
    rc = _stata("parse_optnotallowed, "+st_local("options"))
    if (rc) exit(rc)

    // Step 2: collect palette/colors and apply options
    // get colors
    if (ptype==1) { 
        S.colors(pal) // space-separated list of color specifications
        st_local("palette", "custom") // assign default palette name
    }
    else if (ptype==2) { // ColrSpace palette
        (void) S.pexists(pal, libname="")
        if (substr(libname, 1, strlen("generators"))=="generators") {
            S.palette(pal, n, strtoreal(tokens(st_local("hue"))), 
                strtoreal(tokens(st_local("chroma"))), 
                strtoreal(tokens(st_local("luminance"))),
                strtoreal(tokens(st_local("power"))))
        }
        else if (substr(libname, 1, strlen("lsmaps"))=="lsmaps" |
                 substr(libname, 1, strlen("rgbmaps"))=="rgbmaps") {
            S.palette(pal, n, strtoreal(tokens(st_local("range"))))
        }
        else S.palette(pal, n, st_local("noexpand")!="")
        st_local("palette", S.name())
    }
    else if (ptype==3) {    // palette is Mata object
                            // mataname: name of object 
        p = findexternal(st_local("mataname"))
        if (p==NULL) {
            display("{err}mata object '" + st_local("mataname") + "' not found")
            exit(498)
        }
        if (classname(*p)!="ColrSpace") {
            display("{err}'" + st_local("mataname") + "' is not a ColrSpace() object")
            exit(498)
        }
        S = *p
        if (S.name()!="")   st_local("palette", S.name())
        else                st_local("palette", st_local("mataname"))
    }
    else if (ptype==4) { // colorpalette_<palette>.ado
                         // plist:   comma-separated list of colors
                         // pnames:  comma-separated list of color names
                         // pinfo:   comma-separated list of descriptions
                         // note:    palette note
                         // source:  palette source
        S.colors(st_local("plist"), ",")
        if (st_local("pnames")!="") S.names(st_local("pnames"), ",")
        if (st_local("pinfo")!="")  S.info(st_local("pinfo"), ",")
        S.note(st_local("note"))
        S.source(st_local("source"))
    }
    // class
    if (S.pclass()=="") S.pclass(st_local("class"))
    // option n() (only if not ColrSpace palette)
    if (n<. & ptype!=2) _recycle_or_ipolate(S, n, st_local("noexpand")!="")
    // option select()
    if (st_local("select")!="") S.select(strtoreal(tokens(st_local("select"))))
    // option drop()
    if (st_local("drop")!="") S.drop(strtoreal(tokens(st_local("drop"))))
    // option order()
    if (st_local("order")!="") S.order(strtoreal(tokens(st_local("order"))))
    // option reverse
    if (st_local("reverse")!="") S.reverse()
    // option shift()
    if (st_local("shift")!="") S.shift(strtoreal(st_local("shift")))
    // option opacity()
    if (st_local("opacity")!="") S.opacity(strtoreal(tokens(st_local("opacity"))), 1)
    // option intensity()
    if (st_local("intensity")!="") S.intensity(strtoreal(tokens(st_local("intensity"))), 1)
    // transformation options
    iconv = st_local("iconvert"); ic = 0
    if (iconv=="first") {; S.Intensify(); ic = 1; } // iconvert()
    torder = tokens(st_local("torder")); tn = length(torder)
    for (ti=1; ti<=tn; ti++) {
        if (torder[ti]=="gscale") { // gscale()
            if (st_local("gscale")=="") continue
            if (st_local("gscale_noic")=="" & ic==0) {; S.Intensify(); ic = 1; }
            S.gray(strtoreal(tokens(st_local("gscale_p"))),
                st_local("gscale_method"))
            continue
        }
        if (torder[ti]=="cblind") { // cblind()
            if (st_local("cblind")=="") continue
            if (st_local("cblind_noic")=="" & ic==0) {; S.Intensify(); ic = 1; }
            S.cvd(strtoreal(tokens(st_local("cblind_p"))),
                  st_local("cblind_method"))
            continue
        }
        if (iconv==torder[ti] & ic==0) {; S.Intensify(); ic = 1; } // iconvert()
        if (torder[ti]=="ipolate") { // ipolate()
            if ((ip_n = strtoreal(st_local("ipolate_n")))>=.) continue
            S.ipolate(ip_n, 
                st_local("ipolate_space"), 
                strtoreal(tokens(st_local("ipolate_range"))), 
                strtoreal(st_local("ipolate_power")),
                strtoreal(tokens(st_local("ipolate_positions"))),
                st_local("ipolate_pad")!="")
            continue
        }
        if (torder[ti]=="intensify") { // intensify()
            if (st_local("intensify")=="") continue
            S.intensify(strtoreal(tokens(st_local("intensify"))))
            continue
        }
        if (torder[ti]=="saturate") { // saturate()
            if (st_local("saturate_p")=="") continue
            S.saturate(strtoreal(tokens(st_local("saturate_p"))), 
                st_local("saturate_method"),
                st_local("saturate_level")!="")
            continue
        }
        if (torder[ti]=="luminate") { // luminate()
            if (st_local("luminate_p")=="") continue
            S.luminate(strtoreal(tokens(st_local("luminate_p"))), 
                st_local("luminate_method"),
                st_local("luminate_level")!="")
            continue
        }
    }
    if (iconv=="last" & ic==0) {; S.Intensify(); ic = 1; } // iconvert()
    // return colors
    st_local("plist", S.colors(st_local("forcergb")!=""))
    st_local("pnames", S.names())
    st_local("pinfo", S.info())
    st_local("note", S.note())
    st_local("source", S.source())
    st_local("class", S.pclass())
    st_local("r_c", _get_colors(S, st_local("r_cspace"), st_local("r_fmt")))
}

void checkpalette(class ColrSpace scalar S, real scalar ptype, string scalar pal0)
{
    real scalar      l
    string scalar    PAL0, pal, p1, OP, IN
    string rowvector PAL

    // characters # " may occur in color specifications, but are currently
    // not used in ColrSpace palette names; exit if such characters are found
    if (any(strpos(pal0, ("#",`"""')))) return
    // check for opacity and intensity specifications at end
    PAL0 = pal0
    if (l=strpos(PAL0,"%")) { // parse "...%#[*#]"
        OP   = substr(PAL0,l,.)
        PAL0 = substr(PAL0,1,l-1)
        if (substr(PAL0,-1,1)==" ") return // space not allowed
        if (l=strpos(OP,"*")) {
            IN = substr(OP,l,.)
            OP = substr(OP,1,l-1)
            if (substr(OP,-1,1)==" ") return // space not allowed
        }
    }
    if (IN=="") {
        if (l=strpos(PAL0,"*")) { // parse "...*#"
            IN   = substr(PAL0,l,.)
            PAL0 = substr(PAL0,1,l-1)
            if (substr(PAL0,-1,1)==" ") return // space not allowed
        }
    }
    if (strtrim(PAL0)=="") return // opacify/intensity only; treat as colorlist
    if (OP!="") {
        OP = substr(OP,2,.)
        if (substr(OP,1,1)==" ") return // space not allowed
        if (strtoreal(OP)>=.) return
    }
    if (IN!="") {
        IN = substr(IN,2,.)
        if (substr(IN,1,1)==" ") return // space not allowed
        if (strtoreal(IN)>=.) return
    }
    // case 1: palette exists in ColrSpace
    PAL = tokens(PAL0)
    if ((pal=S.pexists(PAL0))!="") {
        p1 = tokens(pal)[1]
        if (PAL[1]!=p1) {
            // if first word is not an exact match, check whether first word has 
            // an exact match in named colors; if yes, assume pal to be a list
            // of colors (such that, e.g., "blue" will be interpreted as color
            // "blue" and not as palette "Blues")
            if (S.cvalid(PAL[1])==PAL[1]) return
        }
        pal0 = pal  // return expanded palette name
        ptype = 2
        if (OP!="") st_local("opacity", OP)   // overwrite opacity()
        if (IN!="") st_local("intensity", IN) // overwrite intensity()
        return
    }
    // case 2: check whether first word matches a palette
    if ((pal=S.pexists(PAL[1]))!="") {
        p1 = tokens(pal)[1]
        if (PAL[1]!=p1) {
            // (see note above)
            if (S.cvalid(PAL[1])==PAL[1]) return
        }
        // modify palette such that first word is expanded
        PAL[1] = p1
        pal0 = invtokens(PAL)
        ptype = 2
        if (OP!="") st_local("opacity", OP)   // overwrite opacity()
        if (IN!="") st_local("intensity", IN) // overwrite intensity()
    }
}

void _recycle_or_ipolate(class ColrSpace scalar S, real scalar n,
    real scalar noexpand)
{
    real scalar      j, r
    string rowvector kw
    string colvector C
    
    C = S.Colors()
    r = rows(C)
    // special handling of ".." and "..." at end (noexpand will be ignored)
    if (anyof(("..","..."), C[r])) {
        if (r<2) return // do not expand if no colors
        if (n<r) { // select first n colors if too many colors
            S.select(1::n)
            return
        }
        // remove last element and repeat preceding color
        S.drop(-1)
        S.add_select(J(n-r+1, 1, -1))
        return
    }
    // exit if correct number of colors
    if (n==r) return
    // if noexpand: select first n colors if too many colors
    if (noexpand) {
        if (n<r) S.select(1::n)
        return
    }
    // recycle if only one color
    if (r<2) {
        S.recycle(n)
        return
    }
    // recycle if palette is categorical
    if (anyof(("qualitative","categorical"), S.pclass())) {
        S.recycle(n)
        return
    }
    // recycle if the palette contains elements that cannot be interpolated
    kw = ("none","fg","foreground","bg","background",".")
    for (j=length(kw); j; j--) {
        if (anyof(C, kw[j])) {
            S.recycle(n)
            return
        }
    }
    kw = ("%","*")
    for (j=length(kw); j; j--) {
        if (anyof(substr(C,1,1), kw[j])) {
            S.recycle(n)
            return
        }
    }
    // else interpolate
    S.ipolate(n)
}

string scalar _get_colors(class ColrSpace scalar S, string scalar cspace,
    string scalar fmt)
{
    real scalar  i
    transmorphic C
    
    if (cspace=="") return("")
    C = S.get(cspace)
    if (!isstring(C)) {
        C = strofreal(C, fmt)
        for (i=rows(C);i;i--) C[i,1] = invtokens(C[i,])
        C = `"""' :+ C[,1] :+ `"""'
    }
    return(invtokens(C'))
}

end

exit
