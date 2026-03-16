*! version 1.0.3  27dec2018  Ben Jann

program colorpalette9
    version 9.2
    capt _on_colon_parse `0'
    if _rc==0 {
        local 0 `"`s(before)'"'
        local rhs `"`s(after)'"'
        _parse comma lhs 0 : 0
        if `"`lhs'"'!="" error 198
        if `"`rhs'"'=="" local rhs s2
        local palettes
        local palette
        local space
        while (`"`rhs'"'!="") {
            gettoken p rhs : rhs, parse("/") quotes bind
            if `"`p'"'=="/" {
                local palettes `"`palettes'`"`palette'"' "'
                local palette
                local space
                continue
            }
            local palette `"`palette'`space'`p'"'
            local space " "
        }
        if `"`palette'"'!="" {
            local palettes `"`palettes'`"`palette'"'"'
        }
        Graph2 `palettes' `0'
        exit
    }
    gettoken p rhs : 0, parse("/") quotes bind
    if `"`rhs'"'!="" {
        local rhs: copy local 0
        local 0
        local palettes
        local palette
        local space
        while (`"`rhs'"'!="") {
            gettoken p rhs : rhs, parse("/") quotes bind
            if `"`p'"'=="/" {
                local palettes `"`palettes'`"`palette'"' "'
                local palette
                local space
                continue
            }
            local palette `"`palette'`space'`p'"'
            local space " "
        }
        if `"`palette'"'!="" { // handle syntax after last slash
            _parse comma p rhs : palette
            if `"`p'"'!="" {
                Parse_Graph_Opts `rhs'  // returns rhs and 0
                local palettes `"`palettes'`"`p'`rhs'"'"'
            }
            else local 0: copy local palette
        }
        Palette_Get2 `palettes' `0'
    }
    else {
        Palette_Get `0'
    }
    if "`GRAPH'"=="" {
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

program Parse_Graph_Opts
    syntax [, noGRaph GRopts(passthru) TItle(passthru) rows(passthru) * ]
    if `"`graph'`gropts'`title'`rows'"'!="" {
        c_local 0 `", `graph' `gropts' `title' `rows'"'
    }
    if `"`options'"'!="" c_local rhs `", `options'"'
    else                 c_local rhs
end

program Palette_Get2, rclass
    _parse comma palettes 0 : 0
    syntax [, noGRaph GRopts(str asis) TItle(passthru) rows(passthru) * ]
    c_local GRAPH "`graph'"
    c_local GROPTS `"`rows' `title' `gropts'"'
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
            return local p`i'info  `"`r(p`j'info )'"'
        }
    }
    return scalar n = `i'
    return local p `"`plist'"'
    return local pname "custom"
    return local ptype "color"
end

program Palette_Get
    _parse comma palette 0 : 0
    syntax [, noGRaph GRopts(str asis) TItle(passthru) rows(passthru) ///
        IPolate(numlist max=1 integer >=1) * ]
    c_local GRAPH "`graph'"
    c_local GROPTS `"`rows' `title' `gropts'"'
    _Palette_Get `palette', `options'
    if "`ipolate'"!="" {
        Palette_Ipolate `ipolate'
    }
end

program Palette_Ipolate, rclass
    args n
    mata: st_local("P", _invtokens_quoted(ipolate_colors(tokens(st_global("r(p)"))', `n')))
    local P: list clean P
    local i 0
    foreach p of local P {
        local ++i
        return local p`i' `"`p'"'
    }
    return local p `"`P'"'
    if `"`r(pnote)'"'!="" {
        return local pnote `"`r(pnote)' (interpolated)"'
    }
    else return local pnote "(interpolated)"
    return local pname `"`r(pname)'"'
    return local ptype "color"
    return scalar n = `n'
end

program _Palette_Get, rclass
    syntax [anything(name=palette id="palette" everything equalok)] ///
        [, N(numlist max=1 integer >=1) Select(numlist integer >=1) Reverse ///
        INtensity(numlist >=0 <=255) OPacity(numlist int >=0 <=100) * ]
    local n_in: list sizeof intensity
    local n_op: list sizeof opacity
    if `n_op' {
        if c(stata_version)<15 {
            di as err "{bf:opacity()} requires Stata 15"
            exit 9
        }
    }
    // get palette
    if `"`palette'"'=="" local palette s2
    local islist = (`: list sizeof palette'!=1)
    if `islist'==0 {
        capt confirm name _`palette'
        if _rc local islist 1
    }
    if `islist'==0 {
        capt __Palette_Get `palette', n(`n') `options'
        if _rc==199 {
            capt confirm name `palette'
            if _rc { // numeric palette name: cannot be a named style
                di as err `"palette `palette' not found"'
                exit 198
            }
            local islist 1
        }
        else if _rc { // display error message
            __Palette_Get `palette', n(`n') `options'
        }
    }
    if `islist' {
        local i 0
        foreach p of local palette {
            local ++i
            local p`i' `"`p'"'
        }
        local n `i'
        local palette "custom"
    }
    // select/order
    if "`reverse'"!="" {
        if "`select'"=="" {
            qui numlist "`n'(-1)1"
            local select `r(numlist)'
        }
        else {
            local select0 `select'
            local select
            foreach s of local select0 {
                local select `s' `select'
            }
        }
    }
    else if "`select'"=="" {
        qui numlist "1/`n'"
        local select `r(numlist)'
    }
    // opacity/intensity: prepare lists
    local n: list sizeof select
    if `n_in' {
        if `n_in'<`n' {
            mata: _listrecycle("intensity", `n')
            local n_in `n'
        }
        else if `n_in'>`n' {
            mata: _listrecycle("select", `n_in')
            local n `n_in'
        }
    }
    if `n_op' {
        if `n_op'<`n' {
            mata: _listrecycle("opacity",`n')
            local n_op `n'
        }
        else if `n_op'>`n' {
            mata: _listrecycle("select",`n_op')
            local n `n_op'
            if `n_in' {
                mata: _listrecycle("intensity",`n_op')
                local n_in `n_op'
            }
        }
    }
    // return palette
    local plist
    local i 0
    foreach j of local select {
        local pj `"`p`j''"'
        if `"`pj'"'!="" {
            local ++i
            mata: makeRGB("pj")
            gettoken in intensity : intensity
            if `"`in'"'!="" {
                if strpos(`"`pj'"',"*")==0 local pj `"`pj'*`in'"'
            }
            gettoken op opacity   : opacity
            if `"`op'"'!="" {
                if strpos(`"`pj'"',"%")==0 local pj `"`pj'%`op'"'
            }
            local plist `"`plist'`space'`"`pj'"'"'
            local space " "
            return local p`i' `"`pj'"'
            return local p`i'info `"`p`j'info'"'
        }
    }
    local n `i'
    local plist: list clean plist
    return local p `"`plist'"'
    return local pnote `"`note'"'
    return local pname `"`palette'"'
    return local ptype "color"
    return scalar n = `n'
end

program __Palette_Get
    gettoken palette 0 : 0, parse(" ,")
    syntax [, n(numlist max=1 integer >0) * ]
    colorpalette9_`palette', n(`n') `options'
    if `"`P'"'!="" { // palettes that define P (and I)
        mata: st_local("P", _parse_palette(st_local("P")))
        mata: st_local("I", _parse_palette(st_local("I")))
        local min 1
        local max: list sizeof P
        if "`n'"=="" local n `max'
        local n = max(`min',min(`max',`n'))
    }
    else { // palettes that define P#
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
        mata: st_local("P", _parse_palette(st_local("P")))
        mata: st_local("I", _parse_palette(st_local("I")))
    }
    local i 0
    foreach c of local P {
        gettoken info I : I
        local ++i
        if `i'>`n' continue, break
        c_local p`i' `"`c'"'
        c_local p`i'info `"`info'"'
    }
    c_local note `"`note'"'
    c_local n `n'
end

/*----------------------------------------------------------------------------*/
/* graph of single palette                                                    */
/*----------------------------------------------------------------------------*/

program Graph
    syntax [, rows(int 5) TItle(passthru) * ]
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
        if `"`r(p`i')'"'=="" continue
        local jlab `j'
        local plots `plots' (scatteri `r' `j', mlw(vthin) mlc(black) ///
            msymbol(square) msize(`size') mfcolor(`"`r(p`i')'"'))
        local pnum `pnum' `r' `j' "`i'"
        local lbl `lbl' `r' `jlab' `"`r(p`i')'"'
        if `"`r(p`i'info)'"'!="" {
            local info `info' `r' `jlab' `"`r(p`i'info)'"'
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
    local pnum (scatteri `pnum', ms(i) msize(`size') mlabpos(9) ///
            mlabgap(`lblgap') mlabsize(`pnumsize') mlabcolor(black))
    if `"`lbl'"'!="" {
        local lbl (scatteri `lbl', ms(i) msize(`size') mlabpos(3) ///
            mlabgap(`lblgap') mlabsize(`lblsize') mlabcolor(black))
    }
    if `"`info'"'!="" {
        local info (scatteri `info', ms(i) msize(`size') mlabpos(4) ///
            mlabgap(`infogap') mlabsize(`infosize') mlabcolor(black))
    }
    else local info
    local l = `size'/2 + 9
    local r = `size'/2 + `rgap'
    local b = `size'/2 + 5
    local t = `size'/2 + 4
    if `"`title'"'=="" {
        if `"`r(pnote)'"'=="" local title title(`"`r(pname)'"')
        else                  local title title(`"`r(pname)' `r(pnote)'"')
    }
    two `plots' `pnum' `lbl' `info' , `title' scheme(s2color) ///
        legend(off) ylabel(none) graphr(color(white)) ///
        xlabel(none) xscale(range(1 3) off) ///
        yscale(range(1 `rows') off reverse) ///
        plotr(margin(`l' `r' `b' `t')) graphr(margin(0 0 0 3)) `options'
end

/*----------------------------------------------------------------------------*/
/* graph of multiple palettes                                                 */
/*----------------------------------------------------------------------------*/

program Graph2
    _parse comma palettes 0 : 0
    syntax [, TItle(passthru) LABels(str asis) PLabels(str asis) ///
        GRopts(str asis) LColor(str) LWidth(str) VERTical HORizontal * ]
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
    qui generate `y' = ceil(_n/4) - .35 + inlist(mod(_n-1,4)+1,3,4)*.7 in 1/`r'
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
            if `"`r(pnote)'"'=="" local plab `"`r(pname)'"'
            else                  local plab `"`r(pname)' `r(pnote)'"'
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
    local xlab = ceil(`nxvars'/20)
    numlist "1(`xlab')`nxvars'"
    local xlab `r(numlist)'
    if "`orientation'"=="horizontal" {
        local xscale xscale(lstyle(none) range(1 `nxvars'))
        local xlabel xlabel(`xlab', notick)
        local yscale yscale(lstyle(none) range(0.65 `np'.35) reverse)
        local ylabel ylabel(`ylab', nogrid notick angle(hor))
    }
    else {
        local xscale xscale(lstyle(none) range(0.65 `np'.35) alt)
        local xlabel xlabel(`ylab', notick)
        local yscale yscale(lstyle(none) range(1 `nxvars') reverse)
        local ylabel ylabel(`xlab', nogrid notick angle(hor))
    }
    twoway `plots', `xscale' `xlabel' xti("") `yscale' `ylabel' yti("") ///
        legend(off) graphr(margin(l=2 t=2 b=1 r=2) color(white)) ///
        scheme(s2color) `title' `gropts'
end

/*----------------------------------------------------------------------------*/
/* palettes                                                                   */
/*----------------------------------------------------------------------------*/

program colorpalette9_s1
c_local P dkgreen,orange_red,navy,maroon,teal,sienna,orange,magenta,cyan,red,lime,brown,purple,olive_teal,ltblue
end

program colorpalette9_s1r
c_local P yellow,lime,midblue,magenta,orange,red,ltblue,sandb,mint,olive_teal,orange_red,blue,pink,teal,sienna
end

program colorpalette9_s2
c_local P navy,maroon,forest_green,dkorange,teal,cranberry,lavender,khaki,sienna,emidblue,emerald,brown,erose,gold,bluishgray
end

program colorpalette9_economist
c_local P edkblue,emidblue,eltblue,emerald,erose,ebblue,eltgreen,stone,navy,maroon,brown,lavender,teal,cranberry,khaki
end

program colorpalette9_mono
c_local P gs6,gs10,gs8,gs4,black,gs12,gs2,gs7,gs9,gs11,gs13,gs5,gs3,gs14,gs15
end

program colorpalette9_cblind
c_local P #000000,#999999,#E69F00,#56B4E9,#009E73,#F0E442,#0072B2,#D55E00,#CC79A7
c_local I black,grey,orange,sky blue,bluish green,yellow,blue,vermillion,reddish purple
end

program colorpalette9_plottig
c_local P black, ///
     97 156 255, /// plb1 - blue
      0 192 175, /// plg1 - light greenish
      201 152 0, /// ply1 - yellow/brownish
     185 56 255, /// pll1 - purple
    248 118 109, /// plr1 - red
      0 176 246, /// plb2 - bluish 
       0 186 56, /// plg2 - greenish 
      163 165 0, /// ply2 - yellow/brownish
    231 107 243, /// pll2 - purple 
    255 103 164, /// plr2 - red
      0 188 216, /// plb3 - blue 
      107 177 0, /// plg3 - green
      229 135 0, /// ply3 - orange
     253 97 209  //  pll3 - purple
end

program colorpalette9_538
c_local P 3 144 214, /// 538b
          254 48 11, /// 538r
         120 172 68, /// 538g
          247 187 5, /// 538y
        229 138 233, /// 538m
          254 133 3, /// 538o
        242 242 242, /// 538background
        205 205 206, /// 538axis
        155 155 155, /// 538label
        162 204 246, /// 538bs6 (ci)
        254 181 167, /// 538rs6 (ci2)
         42 161 237, /// 538bs1 (contour_begin)
        255 244 241  //  538rs11 (contour_end)
c_local I ,,,,,,(background),(axes etc.),(labels),(ci),(ci2),(contour_begin),(contour_end)
end

program colorpalette9_tfl
c_local P 220 36 31, /// tflred
           0 25 168, /// tflblue
           0 114 41, /// tflgreen
         232 106 16, /// tflorange
          137 78 36, /// tflbrown
          117 16 86, /// tflpurple
          255 206 0, /// tflyellow
           65 75 86  //  tflgrey
end

program colorpalette9_mrc
c_local P 33 103 126, /// mrcblue
          106 59 119, /// mrcpurple
           130 47 90, /// mrcred
          208 114 50, /// mrcorange
           255 219 0, /// mrcyellow
          181 211 52, /// mrcgreen
         138 121 103  //  mrcgrey
end

program colorpalette9_burd
c_local P 33 102 172, /// Bu from RdBu-7
           178 24 43, /// Rd from RdBu-7
           27 120 55, /// Gn from PRGn-7
            230 97 1, /// Or from PuOr-7
            1 102 94, /// BG from BrBG-7
          197 27 125, /// Pi from PiYG-7
          118 42 131, /// Pu from PuOr-7
           140 81 10, /// Br from BrBG-7
            77 77 77, /// Gy from RdGy-7
         103 169 207, /// (ci_arealine)
         209 229 240, /// (ci_area) 
          239 138 98, /// (ci2_arealine)
         253 219 199  //  (ci2_area)
c_local I Bu from RdBu-7,Rd from RdBu-7,Gn from PRGn-7,Or from PuOr-7, ///
    BG from BrBG-7,Pi from PiYG-7,Pu from PuOr-7,Br from BrBG-7,       ///
    Gy from RdGy-7,(ci_arealine),(ci_area),(ci2_arealine),(ci2_area)
end

program colorpalette9_lean
c_local P gs14,gs10,gs12,gs8,gs16,gs13,gs10,gs7,gs4,gs0,gs14,gs10,gs12,gs0,gs16
end

program colorpalette9_hue
    // translation of pal_hue() from the -scales- package by Hadley Wickham in R
    // see https://github.com/hadley/scales
    syntax [, n(int 15) Hue(numlist max=2) Chroma(numlist max=1 >=0) ///
        Luminance(numlist max=1 >=0 <=100) DIRection(int 1) ///
        hstart(real 0) * ] // hstart() not documented; don't see any use for it
    if !inlist(`direction',-1,1) {
        di as err "direction must be 1 or -1"
        exit 198
    }
    gettoken h1 h2 : hue
    gettoken    h2 : h2
    gettoken c     : chroma
    gettoken l     : luminance
    if "`h1'"=="" local h1 = 0 + 15
    if "`h2'"=="" local h2 = 360 + 15
    if "`c'"==""  local c 100
    if "`l'"==""  local l 65
    if (mod(`h2'-`h1',360) < 1) local h2 = `h2' - 360/`n'
    local P
    forv i=1/`n'{
        local x = `h1' + cond(`n'<=1, 0, (`i'-1) * (`h2'-`h1') / (`n'-1))
        local h = mod(`x' + `hstart', 360) * `direction'
        mata: st_local("P", st_local("P") + "`comma'" + HCL_to_RGB(`h', `c', `l'))
        mata: st_local("I", st_local("I") + "`comma'hcl " + ///
            strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `h', `c', `l'))))
        local comma ","
    }
    c_local P `P'
    c_local I `I'
end

program colorpalette9_hcl
    syntax [, n(int 15) Hue(numlist max=2) Chroma(numlist max=2 >=0) ///
        Luminance(numlist max=2 >=0 <=100) POWer(numlist max=2 >0) ///
        QUALitative intense dark light pastel ///
        SEQuential blues greens grays oranges purples reds ///
        heat heat2 TERrain terrain2 viridis plasma redblue ///
        DIVerging bluered bluered2 bluered3 greenorange browngreen pinkgreen purplegreen * ]
    local pal `qualitative' `intense' `dark' `light' `pastel' ///
        `sequential' `blues' `greens' `grays' `oranges' `purples' `reds' ///
        `heat' `heat2' `terrain' `terrain2' `viridis' `plasma' `redblue' ///
        `diverging' `bluered' `bluered2' `bluered3' `greenorange' `browngreen' `pinkgreen' `purplegreen'
    if `: list sizeof pal'>1 {
        di as err `"only one of '`pal'' allowed"'
        exit 198
    }
    if "`pal'"=="" local pal qualitative
    if      "`pal'"=="qualitative" local ptype qualitative
    else if "`pal'"=="intense"     local ptype qualitative
    else if "`pal'"=="dark"        local ptype qualitative
    else if "`pal'"=="light"       local ptype qualitative
    else if "`pal'"=="pastel"      local ptype qualitative
    else if "`pal'"=="diverging"   local ptype diverging
    else if "`pal'"=="bluered"     local ptype diverging
    else if "`pal'"=="bluered2"    local ptype diverging
    else if "`pal'"=="bluered3"    local ptype diverging
    else if "`pal'"=="greenorange" local ptype diverging
    else if "`pal'"=="browngreen"  local ptype diverging
    else if "`pal'"=="pinkgreen"   local ptype diverging
    else if "`pal'"=="purplegreen" local ptype diverging
    else                           local ptype sequential 
    gettoken h1 h2 : hue
    gettoken    h2 : h2
    gettoken c1 c2 : chroma
    gettoken    c2 : c2
    gettoken l1 l2 : luminance
    gettoken    l2 : l2
    gettoken p1 p2 : power
    gettoken    p2 : p2
    if "`ptype'"=="qualitative" {
        if      "`pal'"=="qualitative" local def 15 .  60 70
        else if "`pal'"=="intense"     local def 15 . 100 65
        else if "`pal'"=="dark"        local def 15 .  80 60
        else if "`pal'"=="light"       local def 15 .  50 80
        else if "`pal'"=="pastel"      local def 15 .  35 85
        foreach m in h1 h2 c1 l1 {
            gettoken d def : def
            if "``m''"=="" {
                if `"`d'"'=="." local `m' = `h1' + 360*(`n'-1)/`n'
                else            local `m' `d'
            }
        }
        forv i=1/`n'{
            if `n'==1 local H `h1'
            else local H = `h1' + (`i'-1) * (`h2'-`h1') / (`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + HCL_to_RGB(`H', `c1', `l1'))
            mata: st_local("I", st_local("I") + "`comma'hcl " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `c1', `l1'))))
            local comma ","
        }
    }
    else if "`ptype'"=="sequential" {
        if      "`pal'"=="sequential"  local def 260    .  80  10 25 95 1   .
        else if "`pal'"=="blues"       local def 260    .  80  10 25 95 1   .
        else if "`pal'"=="greens"      local def 145  125  80  10 25 95 1   .
        else if "`pal'"=="grays"       local def   0    .   0   0 15 95 1   .
        else if "`pal'"=="oranges"     local def  40    . 100  10 50 95 1   .
        else if "`pal'"=="purples"     local def 280    .  70  10 20 95 1   .
        else if "`pal'"=="reds"        local def  10   20  80  10 25 95 1   .
        else if "`pal'"=="heat"        local def   0   90 100  30 50 90 0.2 1.0
        else if "`pal'"=="heat2"       local def   0   90  80  30 30 90 0.2 2.0
        else if "`pal'"=="terrain"     local def 130    0  80   0 60 95 0.1 1.0
        else if "`pal'"=="terrain2"    local def 130   30  65   0 45 90 0.5 1.5
        else if "`pal'"=="viridis"     local def 300   75  35  95 15 90 0.8 1.2
        else if "`pal'"=="plasma"      local def 100  100  60 100 15 95 2.0 0.9
        else if "`pal'"=="redblue"     local def   0 -100  80  40 40 75 1.0 1.0
        foreach m in h1 h2 c1 c2 l1 l2 p1 p2 {
            gettoken d def : def
            if "``m''"=="" {
                if "`d'"=="." local `m' `last'
                else          local `m' `d'
            }
            local last ``m''
        }
        forv j=1/`n'{
            if `n'==1 local i 1
            else local i = (`n'-`j')/(`n'-1)
            local H = `h2' - `i' * (`h2'-`h1')
            local C = `c2' - `i'^`p1' * (`c2'-`c1')
            local L = `l2' - `i'^`p2' * (`l2'-`l1')
            mata: st_local("P", st_local("P") + "`comma'" + HCL_to_RGB(`H', `C', `L'))
            mata: st_local("I", st_local("I") + "`comma'hcl " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `C', `L'))))
            local comma ","
        }
    }
    else if "`ptype'"=="diverging" {
        if      "`pal'"=="diverging"    local def 260   0  80 30 95 1 .
        else if "`pal'"=="bluered"      local def 260   0  80 30 95 1 .
        else if "`pal'"=="bluered2"     local def 260   0 100 50 95 1 .
        else if "`pal'"=="bluered3"     local def 180 330  60 75 95 1 .
        else if "`pal'"=="greenorange"  local def 130  45 100 70 95 1 .
        else if "`pal'"=="browngreen"   local def  55 160  60 35 95 1 .
        else if "`pal'"=="pinkgreen"    local def 340 128  90 35 95 1 . 
        else if "`pal'"=="purplegreen"  local def 300 128  60 30 95 1 .
        foreach m in h1 h2 c1 l1 l2 p1 p2 {
            gettoken d def : def
            if "``m''"=="" {
                if "`d'"=="." local `m' `last'
                else          local `m' `d'
            }
            local last ``m''
        }
        forv j=1/`n'{
            if `n'==1 local i 1
            else local i = (`n' - 2*`j' + 1) / (`n'-1)
            local H = cond(`i'>0, `h1', `h2')
            local C = `c1' * abs(`i')^`p1'
            local L = `l2' - abs(`i')^`p2' * (`l2'-`l1')
            mata: st_local("P", st_local("P") + "`comma'" + HCL_to_RGB(`H', `C', `L'))
            mata: st_local("I", st_local("I") + "`comma'hcl " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `C', `L'))))
            local comma ","
        }
    }
    c_local P `P'
    c_local I `I'
    c_local note `pal'
end

program colorpalette9_hsv
    syntax [, n(int 15) Hue(numlist max=2) SATuration(numlist max=2 >=0 <=1) ///
        VALue(numlist max=2 >=0 <=1) POWer(numlist max=2 >0) ///
        QUALitative intense dark light pastel RAINbow ///
        SEQuential blues greens grays oranges purples reds heat terrain ///
        DIVerging bluered bluered2 bluered3 greenorange browngreen pinkgreen purplegreen ///
        heat0 terrain0 * ] // heat0/terrain0 not documented (same as heat/terrain in spmap)
    local pal `qualitative' `intense' `dark' `light' `pastel' `rainbow'  ///
        `sequential' `blues' `greens' `grays' `oranges' `purples' `reds' `heat' `terrain' ///
        `diverging' `bluered' `bluered2' `bluered3' `greenorange' `browngreen' `pinkgreen' `purplegreen' ///
        `heat0' `terrain0'
    if `: list sizeof pal'>1 {
        di as err `"only one of '`pal'' allowed"'
        exit 198
    }
    if "`pal'"=="" local pal qualitative
    if      "`pal'"=="qualitative" local ptype qualitative
    else if "`pal'"=="intense"     local ptype qualitative
    else if "`pal'"=="dark"        local ptype qualitative
    else if "`pal'"=="light"       local ptype qualitative
    else if "`pal'"=="pastel"      local ptype qualitative
    else if "`pal'"=="rainbow"     local ptype qualitative
    else if "`pal'"=="diverging"   local ptype diverging
    else if "`pal'"=="bluered"     local ptype diverging
    else if "`pal'"=="bluered2"    local ptype diverging
    else if "`pal'"=="bluered3"    local ptype diverging
    else if "`pal'"=="greenorange" local ptype diverging
    else if "`pal'"=="browngreen"  local ptype diverging
    else if "`pal'"=="pinkgreen"   local ptype diverging
    else if "`pal'"=="purplegreen" local ptype diverging
    else if "`pal'"=="heat0"       local ptype heat0
    else if "`pal'"=="terrain0"    local ptype terrain0
    else                           local ptype sequential 
    gettoken h1 h2 : hue
    gettoken    h2 : h2
    gettoken s1 s2 : saturation
    gettoken    s2 : s2
    gettoken v1 v2 : value
    gettoken    v2 : v2
    gettoken p1 p2 : power
    gettoken    p2 : p2
    if "`ptype'"=="qualitative" {
        if      "`pal'"=="qualitative" local def   0 360*(`n'-1)/`n' .4 .85
        else if "`pal'"=="intense"     local def   0 360*(`n'-1)/`n' .6 .9
        else if "`pal'"=="dark"        local def   0 360*(`n'-1)/`n' .6 .7
        else if "`pal'"=="light"       local def   0 360*(`n'-1)/`n' .3 .9
        else if "`pal'"=="pastel"      local def   0 360*(`n'-1)/`n' .2 .9
        else if "`pal'"=="rainbow"     local def   0 360*(`n'-1)/`n'  1  1
        foreach m in h1 h2 s1 v1 {
            gettoken d def : def
            if "``m''"=="" local `m' `d'
        }
        forv i=1/`n'{
            if `n'==1 local h `h1'
            else local H = `h1' + (`i'-1) * (`h2'-`h1') / (`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `s1', `v1'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `s1', `v1'))))
            local comma ","
        }
    }
    else if "`ptype'"=="sequential" {
        if      "`pal'"=="sequential"    local def 240   .  .8  .05  .6  1   1.2 .
        else if "`pal'"=="blues"         local def 240   .  .8  .05  .6  1   1.2 .
        else if "`pal'"=="greens"        local def 140 120  1   .1   .3  1   1.2 .
        else if "`pal'"=="grays"         local def   0   .  0   0    .1  .95 1   .
        else if "`pal'"=="oranges"       local def  30   .  1   .1   .9  1   1.2 .
        else if "`pal'"=="purples"       local def 270   .  1   .1   .6  1   1.2 .
        else if "`pal'"=="reds"          local def   0  20  1   .1   .6  1   1.2 .
        else if "`pal'"=="heat"          local def   0  60  1   .2   1   1   0.3 .
        else if "`pal'"=="terrain"       local def 120   0  1   0    .65 .95 0.7 1.5
        foreach m in h1 h2 s1 s2 v1 v2 p1 p2 {
            gettoken d def : def
            if "``m''"=="" {
                if "`d'"=="." local `m' `last'
                else          local `m' `d'
            }
            local last ``m''
        }
        forv j=1/`n'{
            if `n'==1 local i 1
            else local i = (`n'-`j')/(`n'-1)
            local H = `h2' - `i' * (`h2'-`h1')
            local S = `s2' - `i'^`p1' * (`s2'-`s1')
            local V = `v2' - `i'^`p2' * (`v2'-`v1')
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
    }
    else if "`ptype'"=="diverging" {
        if      "`pal'"=="diverging"    local def 240   0  .8 .6 .95 1.2 .
        else if "`pal'"=="bluered"      local def 240   0  .8 .6 .95 1.2 .
        else if "`pal'"=="bluered2"     local def 240   0  .6 .8 .95 1.2 .
        else if "`pal'"=="bluered3"     local def 175 320  .6 .8 .95 1.2 .
        else if "`pal'"=="greenorange"  local def 130  40  1  .8 .95 1.2 .
        else if "`pal'"=="browngreen"   local def  40 150  .8 .6 .95 1.2 .
        else if "`pal'"=="pinkgreen"    local def 330 120  .9 .6 .95 1.2 .
        else if "`pal'"=="purplegreen"  local def 290 120  .7 .5 .95 1.2 .
        foreach m in h1 h2 s1 v1 v2 p1 p2 {
            gettoken d def : def
            if "``m''"=="" {
                if "`d'"=="." local `m' `last'
                else          local `m' `d'
            }
            local last ``m''
        }
        forv j=1/`n'{
            if `n'==1 local i 1
            else local i = (`n' - 2*`j' + 1) / (`n'-1)
            local H = cond(`i'>0, `h1', `h2')
            local S = `s1' * abs(`i')^`p1'
            local V = `v2' - abs(`i')^`p2' * (`v2'-`v1')
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
    }
    else if "`ptype'"=="heat0" {
        if "`h1'"=="" local h1 = 0
        if "`h2'"=="" local h2 = 60
        if "`s1'"=="" local s1 1
        if "`s2'"=="" local s2 0
        if "`v1'"=="" local v1 1
        local j = trunc(`n' / 4)
        local i = `n' - `j'
        forv ii=1/`i'{
            local H = `h1' + cond(`i'==1, 0, (`ii'-1) * (`h2'-`h1') / (`i'-1))
            local S = `s1'
            local V = `v1'
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
        local S1 = `s1' - (`s1'-`s2') / (2*`j')
        local S2 = `s2' + (`s1'-`s2') / (2*`j')
        forv ii=1/`j' {
            local H = `h2'
            local S = `S1' + cond(`j'==1, 0, (`ii'-1) * (`S2'-`S1') / (`j'-1))
            local V = `v1'
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
    }
    else if "`ptype'"=="terrain0" {
        local h3 `h2'
        if "`h1'"=="" local h1 = 120
        if "`h3'"=="" local h3 = 0
        local h2 = (`h1' + `h3')/2  // 60
        if "`s1'"=="" local s1 1
        if "`s2'"=="" local s2 0
        if "`v1'"=="" local v1 .65
        if "`v2'"=="" local v2 .9
        local v3 = `v2' + (1-`v2')/2  // .95
        local k = trunc(`n' / 2)
        forv i=1/`k'{
            local H = `h1' + cond(`k'==1, 0, (`i'-1) * (`h2'-`h1') / (`k'-1))
            local S = `s1'
            local V = `v1' + cond(`k'==1, 0, (`i'-1) * (`v2'-`v1') / (`k'-1))
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
        local k = `n' - `k' + 1
        forv i=2/`k' {
            local H = `h2' + (`i'-1) * (`h3'-`h2') / (`k'-1)
            local S = `s1' + (`i'-1) * (`s2'-`s1') / (`k'-1)
            local V = `v2' + (`i'-1) * (`v3'-`v2') / (`k'-1)
            mata: st_local("P", st_local("P") + "`comma'" + HSV_to_RGB(`H', `S', `V'))
            mata: st_local("I", st_local("I") + "`comma'hsv " + ///
                strtrim(stritrim(sprintf("%9.3g %9.3g %9.3g", `H', `S', `V'))))
            local comma ","
        }
    }
    c_local P `P'
    c_local I `I'
    c_local note `pal'
end

program colorpalette9_ptol
    syntax [, QUALitativ DIVerging RAINbow * ]
    local pal `qualitative' `diverging' `rainbow'
    if `:list sizeof pal'>1 {
        di as err "only one scheme allowed"
        exit 198
    }
    if "`pal'"=="" local pal qualitative // default
    if "`pal'"=="qualitative" {
        c_local P1  68 119 170
        c_local P2  68 119 170,204 102 119
        c_local P3  68 119 170,221 204 119,204 102 119
        c_local P4  68 119 170,17 119 51,221 204 119,204 102 119
        c_local P5  51 34 136,136 204 238,17 119 51,221 204 119,204 102 119
        c_local P6  51 34 136,136 204 238,17 119 51,221 204 119,204 102 119,170 68 153
        c_local P7  51 34 136,136 204 238,68 170 153,17 119 51,221 204 119,204 102 119,170 68 153
        c_local P8  51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,204 102 119,170 68 153
        c_local P9  51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,204 102 119,136 34 85,170 68 153
        c_local P10 51 34 136,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,136 34 85,170 68 153
        c_local P11 51 34 136,102 153 204,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,136 34 85,170 68 153
        c_local P12 51 34 136,102 153 204,136 204 238,68 170 153,17 119 51,153 153 51,221 204 119,102 17 0,204 102 119,170 68 102,136 34 85,170 68 153
    }
    else if "`pal'"=="diverging" {
        c_local P3  153 199 236,255 250 210,245 162 117
        c_local P4  0 139 206,180 221 247,249 189 126,208 50 50
        c_local P5  0 139 206,180 221 247,255 250 210,249 189 126,208 50 50
        c_local P6  58 137 201,153 199 236,230 245 254,255 227 170,245 162 117,210 77 62
        c_local P7  58 137 201,153 199 236,230 245 254,255 250 210,255 227 170,245 162 117,210 77 62
        c_local P8  58 137 201,119 183 229,180 221 247,230 245 254,255 227 170,249 189 126,237 135 94,210 77 62
        c_local P9  58 137 201,119 183 229,180 221 247,230 245 254,255 250 210,255 227 170,249 189 126,237 135 94,210 77 62
        c_local P10 61 82 161,58 137 201,119 183 229,180 221 247,230 245 254,255 227 170,249 189 126,237 135 94,210 77 62,174 28 62
        c_local P11 61 82 161,58 137 201,119 183 229,180 221 247,230 245 254,255 250 210,255 227 170,249 189 126,237 135 94,210 77 62,174 28 62
    }
    else if "`pal'"=="rainbow" {
        c_local P4  64 64 150,87 163 173,222 167 58,217 33 32
        c_local P5  64 64 150,82 157 183,125 184 116,227 156 55,217 33 32
        c_local P6  64 64 150,73 140 194,99 173 153,190 188 72,230 139 51,217 33 32
        c_local P7  120 28 129,63 96 174,83 158 182,109 179 136,202 184 67,231 133 50,217 33 32
        c_local P8  120 28 129,63 86 167,75 145 192,95 170 159,145 189 97,216 175 61,231 124 48,217 33 32
        c_local P9  120 28 129,63 78 161,70 131 193,87 163 173,109 179 136,177 190 78,223 165 58,231 116 47,217 33 32
        c_local P10 120 28 129,63 71 155,66 119 189,82 157 183,98 172 155,134 187 106,199 185 68,227 156 55,231 109 46,217 33 32
        c_local P11 120 28 129,64 64 150,65 108 183,77 149 190,91 167 167,110 179 135,161 190 86,211 179 63,229 148 53,230 104 45,217 33 32
        c_local P12 120 28 129,65 59 147,64 101 177,72 139 194,85 161 177,99 173 153,127 185 114,181 189 76,217 173 60,230 142 52,230 100 44,217 33 32
    }
    c_local note `pal'
end

program colorpalette9_d3
    syntax [, 10 20 20b 20c * ]
    local pal `10' `20' `20b' `20c'
    if `:list sizeof pal'>1 {
        di as err "only one scheme allowed"
        exit 198
    }
    if "`pal'"=="" local pal 10 // default
    if "`pal'"=="10" {
        c_local P #1f77b4,#ff7f0e,#2ca02c,#d62728,#9467bd,#8c564b,#e377c2,#7f7f7f,#bcbd22,#17becf
    }
    else if "`pal'"=="20" {
        c_local P #1f77b4,#aec7e8,#ff7f0e,#ffbb78,#2ca02c,#98df8a,#d62728,#ff9896,#9467bd,#c5b0d5,#8c564b,#c49c94,#e377c2,#f7b6d2,#7f7f7f,#c7c7c7,#bcbd22,#dbdb8d,#17becf,#9edae5
    }
    else if "`pal'"=="20b" {
        c_local P #393b79,#5254a3,#6b6ecf,#9c9ede,#637939,#8ca252,#b5cf6b,#cedb9c,#8c6d31,#bd9e39,#e7ba52,#e7cb94,#843c39,#ad494a,#d6616b,#e7969c,#7b4173,#a55194,#ce6dbd,#de9ed6
    }
    else if "`pal'"=="20c" {
        c_local P #3182bd,#6baed6,#9ecae1,#c6dbef,#e6550d,#fd8d3c,#fdae6b,#fdd0a2,#31a354,#74c476,#a1d99b,#c7e9c0,#756bb1,#9e9ac8,#bcbddc,#dadaeb,#636363,#969696,#bdbdbd,#d9d9d9
    }
    c_local note `pal'
end

program colorpalette9_lin // values obtained from brewextra.ado v 1.0.0,21MAR2016
    syntax [, TABleau CARcolor food FEATures ACTivities FRUITs VEGetables DRINKs BRANDs ///
        Algorithm * ]
    local pal `tableau' `carcolor' `food' `features' `activities' `fruits' `vegetables' `drinks' `brands'
    if `:list sizeof pal'>1 {
        di as err "only one scheme allowed"
        exit 198
    }
    if "`pal'"=="" local pal tableau // default
    if "`pal'"=="tableau" {
        c_local P #1f77b4,#ff7f0e,#2ca02c,#d62728,#9467bd,#8c564b,#e377c2,#7f7f7f,#bcbd22,#17becf,#aec7e8,#ffbb78,#98df8a,#ff9896,#c5b0d5,#c49c94,#f7b6d2,#c7c7c7,#dbdb8d,#9edae5
        local algorithm
    }
    else if "`pal'"=="carcolor" {
        if "`algorithm'"!="" {
            c_local P 214 39 40,199 199 199,127 127 127,44 160 44,140 86 75,31 119 180
            c_local I Red,Silver,Black,Green,Brown,Blue
        }
        else {
            c_local P 214 39 40,199 199 199,127 127 127,44 160 44,140 86 75,31 119 180
            c_local I Red,Silver,Black,Green,Brown,Blue
            local algorithm Turkers
        }
    }
    else if "`pal'"=="food" {
        if "`algorithm'"!="" {
            c_local P 31 119 180,255 127 14,140 86 75,44 160 44,255 187 120,219 219 141,214 39 40
            c_local I Sour cream,Blue cheese dressing,Porterhouse steak,Iceberg lettuce,Onions (raw),Potato (baked),Tomato
        }
        else {
            c_local P 199 199 199,31 119 180,140 86 75,152 223 138,219 219 141,196 156 148,214 39 40
            c_local I Sour cream,Blue cheese dressing,Porterhouse steak,Iceberg lettuce,Onions (raw),Potato (baked),Tomato
            local algorithm Turkers
        }
    }
    else if "`pal'"=="features" {
        if "`algorithm'"!="" {
            c_local P 214 39 40,31 119 180,140 86 75,255 127 14,44 160 44
            c_local I Speed,Reliability,Comfort,Safety,Efficiency
        }
        else {
            c_local P 214 39 40,31 119 180,174 119 232,44 160 44,152 223 138
            c_local I Speed,Reliability,Comfort,Safety,Efficiency
            local algorithm Turkers
        }
    }
    else if "`pal'"=="activities" {
        if "`algorithm'"!="" {
            c_local P 140 86 75,255 127 14,31 119 180,227 119 194,214 39 40
            c_local I Sleeping,Working,Leisure,Eating,Driving
        }
        else {
            c_local P 31 119 180,214 39 40,152 223 138,44 160 44,127 127 127
            c_local I Sleeping,Working,Leisure,Eating,Driving
            local algorithm Turkers
        }
    }
    else if "`pal'"=="fruits" {
        if "`algorithm'"!="" {
            c_local P 44 160 44,188 189 34,31 119 180,214 39 40,148 103 189,255 187 120,255 127 14
            c_local I Apple,Banana,Blueberry,Cherry,Grape,Peach,Tangerine
        }
        else {
            c_local P 146 195 51,251 222 6,64 105 166,200 0 0,127 34 147,251 162 127,255 86 29
            c_local I Apple,Banana,Blueberry,Cherry,Grape,Peach,Tangerine
            local algorithm expert
        }
    }
    else if "`pal'"=="vegetables" {
        if "`algorithm'"!="" {
            c_local P 255 127 14,44 160 44,188 189 34,148 103 189,140 86 75,152 223 138,214 39 40
            c_local I Carrot,Celery,Corn,Eggplant,Mushroom,Olive,Tomato
        }
        else {
            c_local P 255 141 61,157 212 105,245 208 64,104 59 101,239 197 143,139 129 57,255 26 34
            c_local I Carrot,Celery,Corn,Eggplant,Mushroom,Olive,Tomato
            local algorithm expert
        }
    }
    else if "`pal'"=="drinks" {
        if "`algorithm'"!="" {
            c_local P 140 86 75,214 39 40,227 119 194,31 119 180,44 160 44,255 127 14,148 103 189
            c_local I A&W Root Beer,Coca-Cola,Dr. Pepper,Pepsi,Sprite,Sunkist,Welch's Grape
        }
        else {
            c_local P 119 67 6,254 0 0,151 37 63,1 106 171,1 159 76,254 115 20,104 105 169
            c_local I A&W Root Beer,Coca-Cola,Dr. Pepper,Pepsi,Sprite,Sunkist,Welch's Grape
            local algorithm expert
        }
    }
    else if "`pal'"=="brands" {
        if "`algorithm'"!="" {
            c_local P 152 223 138,31 119 180,255 127 14,140 86 75,44 160 44,214 39 40,148 103 189
            c_local I Apple,AT&T,Home Depot,Kodak,Starbucks,Target,Yahoo!
        }
        else {
            c_local P 161 165 169,44 163 218,242 99 33,255 183 0,0 112 66,204 0 0,123 0 153
            c_local I Apple,AT&T,Home Depot,Kodak,Starbucks,Target,Yahoo!
            local algorithm expert
        }
    }
    if `"`algorithm'"'!="" local algorithm (`algorithm')
    c_local note `pal' `algorithm'
end

program colorpalette9_spmap
    syntax [, n(numlist max=1 integer) ///
        BLues GREENs GREYs REDs RAINbow heat TERrain TOPological * ]
    local pal `blues' `greens' `greys' `reds' `rainbow' `heat' `terrain' `topological'
    if `:list sizeof pal'>1 {
        di as err "only one scheme allowed"
        exit 198
    }
    if "`pal'"=="" local pal blues // default
    if "`pal'"=="blues" {
        if "`n'"==""   local n 15
        local n = max(2,min(`n',99))
        local P
        forv i = 1/`n' {
            local p = (`i'-1)/(`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + ///
                HSV_to_RGB(208, .2 + .8*`p', 1 - .6*`p'))
            local comma ","
        }
        c_local P `P'
        c_local n `n'
    }
    else if "`pal'"=="greens" {
        if "`n'"==""   local n 15
        local n = max(2,min(`n',99))
        local P
        forv i = 1/`n' {
            local p = (`i'-1)/(`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + ///
                HSV_to_RGB(122 + 20*`p', .2 + .8*`p', 1 - .7*`p'))
            local comma ","
        }
        c_local P `P'
        c_local n `n'
    }
    else if "`pal'"=="greys" {
        if "`n'"==""   local n 15
        local n = max(2,min(`n',99))
        local P
        forv i = 1/`n' {
            local p = (`i'-1)/(`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + ///
                HSV_to_RGB(0, 0, .88 - .88*`p'))
            local comma ","
        }
        c_local P `P'
        c_local n `n'
    }
    else if "`pal'"=="reds" {
        if "`n'"==""   local n 15
        local n = max(2,min(`n',99))
        local P
        forv i = 1/`n' {
            local p = (`i'-1)/(`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + ///
                HSV_to_RGB(20 - 20*`p', .2 + .8*`p', 1 - max((0, 1.2*(`p'-.5)))))
            local comma ","
        }
        c_local P `P'
        c_local n `n'
    }
    else if "`pal'"=="rainbow" {
        if "`n'"==""   local n 15
        local n = max(2,min(`n',99))
        local P
        forv i = 1/`n' {
            local p = (`i'-1)/(`n'-1)
            mata: st_local("P", st_local("P") + "`comma'" + ///
                HSV_to_RGB(240 - 240*`p', 1, 1))
            local comma ","
        }
        c_local P `P'
        c_local n `n'
    }
    else if "`pal'"=="heat" {
        c_local P2 255 255 0,255 0 0
        c_local P3 255 255 0,255 128 0,255 0 0
        c_local P4 255 255 128,255 255 0,255 128 0,255 0 0
        c_local P5 255 255 128,255 255 0,255 170 0,255 85 0,255 0 0
        c_local P6 255 255 128,255 255 0,255 191 0,255 128 0,255 64 0,255 0 0
        c_local P7 255 255 128,255 255 0,255 204 0,255 153 0,255 102 0,255 51 0,255 0 0
        c_local P8 255 255 191,255 255 64,255 255 0,255 204 0,255 153 0,255 102 0,255 51 0,255 0 0
        c_local P9 255 255 191,255 255 64,255 255 0,255 213 0,255 170 0,255 128 0,255 85 0,255 42 0,255 0 0
        c_local P10 255 255 191,255 255 64,255 255 0,255 219 0,255 182 0,255 146 0,255 109 0,255 73 0,255 36 0,255 0 0
        c_local P11 255 255 191,255 255 64,255 255 0,255 223 0,255 191 0,255 159 0,255 128 0,255 96 0,255 64 0,255 32 0,255 0 0
        c_local P12 255 255 213,255 255 128,255 255 42,255 255 0,255 223 0,255 191 0,255 159 0,255 128 0,255 96 0,255 64 0,255 32 0,255 0 0
        c_local P13 255 255 213,255 255 128,255 255 42,255 255 0,255 227 0,255 198 0,255 170 0,255 142 0,255 113 0,255 85 0,255 57 0,255 28 0,255 0 0
        c_local P14 255 255 213,255 255 128,255 255 42,255 255 0,255 229 0,255 204 0,255 178 0,255 153 0,255 128 0,255 102 0,255 77 0,255 51 0,255 26 0,255 0 0
        c_local P15 255 255 213,255 255 128,255 255 42,255 255 0,255 232 0,255 209 0,255 185 0,255 162 0,255 139 0,255 116 0,255 93 0,255 70 0,255 46 0,255 23 0,255 0 0
        c_local P16 255 255 223,255 255 159,255 255 96,255 255 32,255 255 0,255 232 0,255 209 0,255 185 0,255 162 0,255 139 0,255 116 0,255 93 0,255 70 0,255 46 0,255 23 0,255 0 0
    }
    else if "`pal'"=="terrain" {
        c_local P2 0 166 0,242 242 242
        c_local P3 0 166 0,236 177 118,242 242 242
        c_local P4 0 166 0,230 230 0,236 177 118,242 242 242
        c_local P5 0 166 0,230 230 0,234 182 78,238 185 159,242 242 242
        c_local P6 0 166 0,99 198 0,230 230 0,234 182 78,238 185 159,242 242 242
        c_local P7 0 166 0,99 198 0,230 230 0,233 189 58,236 177 118,239 194 179,242 242 242
        c_local P8 0 166 0,62 187 0,139 208 0,230 230 0,233 189 58,236 177 118,239 194 179,242 242 242
        c_local P9 0 166 0,62 187 0,139 208 0,230 230 0,232 195 46,235 178 94,237 180 142,240 201 192,242 242 242
        c_local P10 0 166 0,45 182 0,99 198 0,160 214 0,230 230 0,232 195 46,235 178 94,237 180 142,240 201 192,242 242 242
        c_local P11 0 166 0,45 182 0,99 198 0,160 214 0,230 230 0,232 199 39,234 182 78,236 177 118,238 185 159,240 207 200,242 242 242
        c_local P12 0 166 0,36 179 0,76 191 0,122 204 0,173 217 0,230 230 0,232 199 39,234 182 78,236 177 118,238 185 159,240 207 200,242 242 242
        c_local P13 0 166 0,36 179 0,76 191 0,122 204 0,173 217 0,230 230 0,231 203 33,233 186 67,235 177 101,237 179 135,239 190 170,240 211 206,242 242 242
        c_local P14 0 166 0,29 176 0,62 187 0,99 198 0,139 208 0,182 219 0,230 230 0,231 203 33,233 186 67,235 177 101,237 179 135,239 190 170,240 211 206,242 242 242
        c_local P15 0 166 0,29 176 0,62 187 0,99 198 0,139 208 0,182 219 0,230 230 0,231 206 29,233 189 58,234 179 88,236 177 118,237 182 148,239 194 179,241 214 211,242 242 242
        c_local P16 0 166 0,25 175 0,53 184 0,83 193 0,116 202 0,151 211 0,189 220 0,230 230 0,231 206 29,233 189 58,234 179 88,236 177 118,237 182 148,239 194 179,241 214 211,242 242 242
    }
    else if "`pal'"=="topological" {
        c_local P2 76 0 255,0 229 255
        c_local P3 76 0 255,0 255 77,255 255 0
        c_local P4 76 0 255,0 229 255,0 255 77,255 255 0
        c_local P5 76 0 255,0 76 255,0 229 255,0 255 77,255 255 0
        c_local P6 76 0 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
        c_local P7 76 0 255,0 76 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
        c_local P8 76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,230 255 0,255 255 0,255 224 178
        c_local P9 76 0 255,0 76 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
        c_local P10 76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
        c_local P11 76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,77 255 0,230 255 0,255 255 0,255 222 89,255 224 178
        c_local P12 76 0 255,0 25 255,0 128 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
        c_local P13 76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
        c_local P14 76 0 255,15 0 255,0 46 255,0 107 255,0 168 255,0 229 255,0 255 77,26 255 0,128 255 0,230 255 0,255 255 0,255 229 59,255 219 119,255 224 178
        c_local P15 76 0 255,0 0 255,0 76 255,0 153 255,0 229 255,0 255 77,0 255 0,77 255 0,153 255 0,230 255 0,255 255 0,255 234 45,255 222 89,255 219 134,255 224 178
        c_local P16 76 0 255,15 0 255,0 46 255,0 107 255,0 168 255,0 229 255,0 255 77,0 255 0,77 255 0,153 255 0,230 255 0,255 255 0,255 234 45,255 222 89,255 219 134,255 224 178
    }
    c_local note `pal'
end

program colorpalette9_sfso
    syntax [, BRown ORange red PInk PUrple VIolet BLue LTBLue TUrquoise ///
        green OLive black parties LANGuages VOTEs THemes cmyk * ]
    local pal `brown' `orange' `red' `pink' `purple' `violet' `blue' ///
        `ltblue' `turquoise' `green' `olive' `black' `parties' `languages' ///
        `votes' `themes'
    if `:list sizeof pal'>1 {
        di as err "only one scheme allowed"
        exit 198
    }
    if "`pal'"=="" local pal blue // default
    if      "`pal'"=="brown" {
        if "`cmyk'"=="" c_local P #6b0616,#a1534e,#b67d6c,#cca58f,#ddc3a8,#eee3cd
        else c_local P 0 1 .7 .6,0 .74 .57 .32,0 .56 .5 .24,0 .4 .4 .16,0 .27 .35 .1,0 .12 .22 .05
        //sRGB: c_local P #6b0616,#a1524f,#b67d6c,#cca590,#ddc3a8,#eee3cd
    }
    else if "`pal'"=="orange" {
        if "`cmyk'"=="" c_local P #92490d,#ce6725,#d68c25,#e2b224,#eccf76,#f6e7be
        else c_local P 0 .75 1 .4,0 .75 1 0,0 .59 1 0,0 .4 1 0,0 .26 .68 0,0 .13 .35 0
        //sRGB: c_local P #91490d,#cd6725,#d68c25,#e1b124,#eccf76,#f6e7be
    }
    else if "`pal'"=="red" {
        if "`cmyk'"=="" c_local P #6d0724,#a61346,#c62a4f,#d17477,#dea49f,#efd6d1
        else c_local P .1 1 .6 .55,.1 1 .6 .15,0 .95 .64 0,0 .71 .48 0,0 .5 .34 0,0 .25 .16 0
        //sRGB: c_local P #6d0724,#a61346,#c62a4f,#d17377,#dea59f,#efd6d1
    }
    else if "`pal'"=="pink" {
        if "`cmyk'"=="" c_local P #7c0051,#a4006f,#c0007c,#cc669d,#da9dbf,#efd7e5
        else c_local P .12 1 .12 .45,.09 1 .09 .18,0 1 .09 .04,0 .75 .07 .03,0 .53 .04 .02,0 .25 .02 0
        //sRGB: c_local P #7b0051,#a4006f,#c0007b,#cc669d,#da9dbf,#f0d7e5
    }
    else if "`pal'"=="purple" {
        if "`cmyk'"=="" c_local P #5e0059,#890883,#a23392,#bf64a6,#d79dc5,#efd7e8
        else c_local P .45 1 0 .45,.45 1 0 .05,.32 .9 0 0,.15 .75 0 0,.05 .53 0 0,0 .25 0 0
        //sRGB: c_local P #5e0058,#890783,#a23092,#be63a6,#d79dc5,#f0d7e8
    }
    else if "`pal'"=="violet" {
        if "`cmyk'"=="" c_local P #3a0054,#682b86,#8c58a3,#a886bc,#c5b0d5,#e1d7eb
        else c_local P .75 1 0 .5,.65 .9 0 .12,.51 .75 0 0,.38 .56 0 0,.25 .38 0 0,.12 .2 0 0
        //sRGB: c_local P #3a0054,#682b86,#8c58a3,#a886bc,#c5b0d4,#e1d7eb
    }
    else if "`pal'"=="blue" {
        if "`cmyk'"=="" c_local P #1c3259,#374a83,#6473aa,#8497cf,#afbce2,#d8def2,#e8eaf7
        else c_local P .83 .45 0 .7,.85 .55 0 .4,.7 .45 0 .2,.63 .36 0 0,.43 .22 0 0,.22 .1 0 0,.13 .07 0 0
        //sRGB: c_local P #1c3258,#374a83,#6473aa,#8497cf,#afbce2,#d8def2,#e8eaf7
        c_local I ,,,BFS-Blau,,,BFS-Blau 20%
    }
    else if "`pal'"=="ltblue" {
        if "`cmyk'"=="" c_local P #076e8d,#1b9dc9,#76b8da,#abd0e7,#c8e0f2,#edf5fd
        else c_local P .98 0 .14 .45,.98 0 .14 .05,.72 0 .1 .03,.49 0 .07 .02,.35 0 .04 0,.12 0 0 0
        //sRGB: c_local P #086e8c,#159dc9,#76b7da,#abd0e7,#c8e0f2,#edf5fd
    }
    else if "`pal'"=="turquoise" {
        if "`cmyk'"=="" c_local P #005046,#107a6d,#3aa59a,#95c6c3,#cbe1df,#e9f2f5
        else c_local P 1 0 .55 .65,1 0 .55 .35,.94 0 .5 0,.6 0 .3 0,.33 0 .17 0,.15 0 .05 0
        //sRGB: c_local P #005046,#107a6d,#3aa59a,#95c6c3,#cbe1df,#e9f2f5
    }
    else if "`pal'"=="green" {
        if "`cmyk'"=="" c_local P #3b6519,#68a239,#95c15b,#b3d17f,#d3e3af,#ecf2d1
        else c_local P .75 0 1 .6,.75 0 1 .15,.6 0 .85 0,.45 0 .68 0,.28 0 .45 0,.12 0 .28 0
        //sRGB: c_local P #3a6419,#68a139,#95c15c,#b3d07f,#d3e3af,#ecf2d1
    }
    else if "`pal'"=="olive" {
        if "`cmyk'"=="" c_local P #6f6f02,#a3a20a,#c5c00c,#e3df86,#eeecbc,#fefde6
        else c_local P .05 0 1 .7,.05 0 1 .45,0 0 1 .3,0 0 .6 .15,0 0 .35 .09,0 0 .17 0
        //sRGB: c_local P #6f6f01,#a3a20a,#c5c00c,#e3df85,#eeecbc,#fffde6
    }
    else if "`pal'"=="black" {
        if "`cmyk'"=="" c_local P #3f3f3e,#838382,#b2b3b3,#d4d5d5,#e6e6e7,#f7f7f7
        else c_local P 0 0 0 .9,0 0 0 .65,0 0 0 .43,0 0 0 .25,0 0 0 .15,0 0 0 .05
        //sRGB: c_local P #3c3c3c,#828282,#b2b2b2,#d4d4d4,#e6e6e6,#f6f6f6
    }
    else if "`pal'"=="parties" {
        if "`cmyk'"=="" c_local P #6268AF,#f39f5e,#ea546f,#547d34,#cbd401,#ffff00,#26b300,#792a8f,#9fabd9,#f0da9d,#bebebe
        else c_local P .76 .6 .02 0,0 .57 .78 0,0 .85 .58 0,.8 .3 1 .2,.28 .01 .96 0,.01 0 .96 0,.72 0 1 0,.6 .92 0 0,.5 .29 0 0,0 .2 .5 0,0 0 0 .35
        c_local I FDP,CVP,SP,SVP,GLP,BDP,Grüne, "small leftwing parties (PdA, Sol.)","small middle parties (EVP, CSP)","small rightwing parties (EDu, Lega)",other parties
    }
    else if "`pal'"=="languages" {
        if "`cmyk'"=="" c_local P #c73e31,#4570ba,#4ca767,#ecce42,#7f5fa9
        else c_local P 0 .9 .9 0,.9 .5 0 0,.9 0 .8 0,0 .25 .9 0,.6 .7 0 0
        c_local I German,French,Italian,Rhaeto-Romanic,English
    }
    else if "`pal'"=="votes" {
        if "`cmyk'"=="" c_local P #6d2a83,#6d2a83*.8,#6d2a83*.6,#6d2a83*.4,#6d2a83*.2,#45974d*.2,#45974d*.4,#45974d*.6,#45974d*.8,#45974d
        else c_local P .6 .9 0 .15,.6 .9 0 .15*.8,.6 .9 0 .15*.6,.6 .9 0 .15*.4,.6 .9 0 .15*.2,.9 0 .9 .15*.2,.9 0 .9 .15*.4,.9 0 .9 .15*.6,.9 0 .9 .15*.8,.9 0 .9 .15
        c_local I No,,,,,,,,,Yes
    }
    else if "`pal'"=="themes" {
        c_local P .63 .36 0 0,0 .38 1 0,0 .59 1 0,0 .76 .88 0,0 .95 .64 0,0 1 .09 .04,.32 .9 0 0,.51 .57 0 0,.62 .6 0 0,.63 .22 0 .03,.8 .29 0 0,.98 0 .14 .05,.94 0 .5 0,.6 0 .85 0,.42 0 .76 0,0 0 1 .3,0 .19 .98 .35,.13 0 .6 .32,0 .36 .5 .24,0 .74 .57 .32,0 .5 .16 .14,.32 0 .18 .37
        c_local I 00 Basics and overviews,01 Population,02 Territory and environment,03 Work and income,/*
        */04 National economy,05 Prices,06 Industry and services,07 Agriculture and forestry,/*
        */08 Energy,09 Construction and housing,10 Tourism,11 Mobility and transport,/*
        */"12 Money, banks and insurance",13 Social security,14 Health,15 Education and science,/*
        */"16 Culture, media, information society, sport",17 Politics,18 General Government and finance,/*
        */19 Crime and criminal justice,20 Economic and social situation of the population,/*
        */21 Sustainable development
        local cmyk cmyk
    }
    if "`cmyk'"!="" local cmyk (CMYK)
    c_local note `pal' `cmyk'
end

program colorpalette9_Accent
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 127 201 127,190 174 212,253 192 134,255 255 153,56 108 176,240 2 127,191 91 23,102 102 102
}
else {
c_local P .5 0 .5 0,.25 .25 0 0,0 .25 .4 0,0 0 .4 0,.8 .4 0 0,0 1 0 0,.25 .6 .9 0,0 0 0 .6
c_local note (CMYK)
}
end

program colorpalette9_Blues
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 222 235 247,158 202 225,49 130 189
c_local P4 239 243 255,189 215 231,107 174 214,33 113 181
c_local P5 239 243 255,189 215 231,107 174 214,49 130 189,8 81 156
c_local P6 239 243 255,198 219 239,158 202 225,107 174 214,49 130 189,8 81 156
c_local P7 239 243 255,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 69 148
c_local P8 247 251 255,222 235 247,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 69 148
c_local P9 247 251 255,222 235 247,198 219 239,158 202 225,107 174 214,66 146 198,33 113 181,8 81 156,8 48 107
}
else {
c_local P3 .13 .03 0 0,.38 .08 0 0,.82 .27 0 0
c_local P4 .08 .02 0 0,.28 .07 0 0,.57 .14 0 0,.9 .34 0 0
c_local P5 .08 .02 0 0,.28 .07 0 0,.57 .14 0 0,.82 .27 0 0,1 .45 0 .07
c_local P6 .08 .02 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.82 .27 0 0,1 .45 0 .07
c_local P7 .08 .02 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .55 0 .05
c_local P8 .03 .01 0 0,.13 .03 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .55 0 .05
c_local P9 .03 .01 0 0,.13 .03 0 0,.24 .06 0 0,.38 .08 0 0,.57 .14 0 0,.75 .22 0 0,.9 .34 0 0,1 .45 0 .07,1 .55 0 .3
c_local note (CMYK)
}
end

program colorpalette9_BrBG
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 216 179 101,245 245 245,90 180 172
c_local P4 166 97 26,223 194 125,128 205 193,1 133 113
c_local P5 166 97 26,223 194 125,245 245 245,128 205 193,1 133 113
c_local P6 140 81 10,216 179 101,246 232 195,199 234 229,90 180 172,1 102 94
c_local P7 140 81 10,216 179 101,246 232 195,245 245 245,199 234 229,90 180 172,1 102 94
c_local P8 140 81 10,191 129 45,223 194 125,246 232 195,199 234 229,128 205 193,53 151 143,1 102 94
c_local P9 140 81 10,191 129 45,223 194 125,246 232 195,245 245 245,199 234 229,128 205 193,53 151 143,1 102 94
c_local P10 84 48 5,140 81 10,191 129 45,223 194 125,246 232 195,199 234 229,128 205 193,53 151 143,1 102 94,0 60 48
c_local P11 84 48 5,140 81 10,191 129 45,223 194 125,246 232 195,245 245 245,199 234 229,128 205 193,53 151 143,1 102 94,0 60 48
}
else {
c_local P3 .15 .25 .55 0,0 0 0 .05,.65 .05 .23 0
c_local P4 .35 .55 .9 0,.12 .2 .45 0,.5 0 .17 0,1 .1 .55 0
c_local P5 .35 .55 .9 0,.12 .2 .45 0,0 0 0 .05,.5 0 .17 0,1 .1 .55 0
c_local P6 .45 .6 1 0,.15 .25 .55 0,.03 .08 .2 0,.22 0 .06 0,.65 .05 .23 0,1 .3 .6 0
c_local P7 .45 .6 1 0,.15 .25 .55 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.65 .05 .23 0,1 .3 .6 0
c_local P8 .45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0
c_local P9 .45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0
c_local P10 .45 .6 1 .4,.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0,1 .3 .7 .4
c_local P11 .45 .6 1 .4,.45 .6 1 0,.25 .43 .8 0,.12 .2 .45 0,.03 .08 .2 0,0 0 0 .05,.22 0 .06 0,.5 0 .17 0,.8 .12 .35 0,1 .3 .6 0,1 .3 .7 .4
c_local note (CMYK)
}
end

program colorpalette9_BuGn
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 229 245 249,153 216 201,44 162 95
c_local P4 237 248 251,178 226 226,102 194 164,35 139 69
c_local P5 237 248 251,178 226 226,102 194 164,44 162 95,0 109 44
c_local P6 237 248 251,204 236 230,153 216 201,102 194 164,44 162 95,0 109 44
c_local P7 237 248 251,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 88 36
c_local P8 247 252 253,229 245 249,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 88 36
c_local P9 247 252 253,229 245 249,204 236 230,153 216 201,102 194 164,65 174 118,35 139 69,0 109 44,0 68 27
}
else {
c_local P3 .1 0 0 0,.4 0 .15 0,.83 0 .7 0
c_local P4 .07 0 0 0,.3 0 .05 0,.6 0 .3 0,.87 .1 .83 0
c_local P5 .07 0 0 0,.3 0 .05 0,.6 0 .3 0,.83 0 .7 0,1 .2 1 0
c_local P6 .07 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.83 0 .7 0,1 .2 1 0
c_local P7 .07 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .35 1 0
c_local P8 .03 0 0 0,.1 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .35 1 0
c_local P9 .03 0 0 0,.1 0 0 0,.2 0 .06 0,.4 0 .15 0,.6 0 .3 0,.75 0 .55 0,.87 .1 .83 0,1 .2 1 0,1 .5 1 0
c_local note (CMYK)
}
end

program colorpalette9_BuPu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 224 236 244,158 188 218,136 86 167
c_local P4 237 248 251,179 205 227,140 150 198,136 65 157
c_local P5 237 248 251,179 205 227,140 150 198,136 86 167,129 15 124
c_local P6 237 248 251,191 211 230,158 188 218,140 150 198,136 86 167,129 15 124
c_local P7 237 248 251,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,110 1 107
c_local P8 247 252 253,224 236 244,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,110 1 107
c_local P9 247 252 253,224 236 244,191 211 230,158 188 218,140 150 198,140 107 177,136 65 157,129 15 124,77 0 75
}
else {
c_local P3 .12 .03 0 0,.38 .14 0 0,.47 .6 0 0
c_local P4 .07 0 0 0,.3 .1 0 0,.45 .3 0 0,.47 .7 0 0
c_local P5 .07 0 0 0,.3 .1 0 0,.45 .3 0 0,.47 .6 0 0,.47 .95 0 .05
c_local P6 .07 0 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.47 .6 0 0,.47 .95 0 .05
c_local P7 .07 0 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.5 1 0 .15
c_local P8 .03 0 0 0,.12 .03 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.5 1 0 .15
c_local P9 .03 0 0 0,.12 .03 0 0,.25 .09 0 0,.38 .14 0 0,.45 .3 0 0,.45 .5 0 0,.47 .7 0 0,.47 .95 0 .05,.5 1 0 .4
c_local note (CMYK)
}
end

program colorpalette9_Dark2
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 27 158 119,217 95 2,117 112 179,231 41 138,102 166 30,230 171 2,166 118 29,102 102 102
}
else {
c_local P .9 0 .55 0,.15 .6 1 0,.55 .45 0 0,.05 .85 .05 0,.6 .1 1 0,.1 .3 1 0,.35 .45 .9 0,0 0 0 .6
c_local note (CMYK)
}
end

program colorpalette9_GnBu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 224 243 219,168 221 181,67 162 202
c_local P4 240 249 232,186 228 188,123 204 196,43 140 190
c_local P5 240 249 232,186 228 188,123 204 196,67 162 202,8 104 172
c_local P6 240 249 232,204 235 197,168 221 181,123 204 196,67 162 202,8 104 172
c_local P7 240 249 232,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 88 158
c_local P8 247 252 240,224 243 219,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 88 158
c_local P9 247 252 240,224 243 219,204 235 197,168 221 181,123 204 196,78 179 211,43 140 190,8 104 172,8 64 129
}
else {
c_local P3 .12 0 .12 0,.34 0 .25 0,.75 .12 0 0
c_local P4 .06 0 .08 0,.27 0 .23 0,.52 0 .15 0,.8 .2 0 0
c_local P5 .06 0 .08 0,.27 0 .23 0,.52 0 .15 0,.75 .12 0 0,1 .35 0 0
c_local P6 .06 0 .08 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.75 .12 0 0,1 .35 0 0
c_local P7 .06 0 .08 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .42 0 .05
c_local P8 .03 0 .05 0,.12 0 .12 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .42 0 .05
c_local P9 .03 0 .05 0,.12 0 .12 0,.2 0 .2 0,.34 0 .25 0,.52 0 .15 0,.7 .05 0 0,.85 .2 0 0,1 .35 0 0,1 .5 0 .2
c_local note (CMYK)
}
end

program colorpalette9_Greens
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 229 245 224,161 217 155,49 163 84
c_local P4 237 248 233,186 228 179,116 196 118,35 139 69
c_local P5 237 248 233,186 228 179,116 196 118,49 163 84,0 109 44
c_local P6 237 248 233,199 233 192,161 217 155,116 196 118,49 163 84,0 109 44
c_local P7 237 248 233,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 90 50
c_local P8 247 252 245,229 245 224,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 90 50
c_local P9 247 252 245,229 245 224,199 233 192,161 217 155,116 196 118,65 171 93,35 139 69,0 109 44,0 68 27
}
else {
c_local P3 .1 0 .1 0,.37 0 .37 0,.81 0 .76 0
c_local P4 .07 0 .07 0,.27 0 .27 0,.55 0 .55 0,.84 .1 .83 0
c_local P5 .07 0 .07 0,.27 0 .27 0,.55 0 .55 0,.81 0 .76 0,1 .2 1 0
c_local P6 .07 0 .07 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.81 0 .76 0,1 .2 1 0
c_local P7 .07 0 .07 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .35 .9 0
c_local P8 .03 0 .03 0,.1 0 .1 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .35 .9 0
c_local P9 .03 0 .03 0,.1 0 .1 0,.22 0 .22 0,.37 0 .37 0,.55 0 .55 0,.75 0 .7 0,.87 .1 .83 0,1 .2 1 0,1 .5 1 0
c_local note (CMYK)
}
end

program colorpalette9_Greys
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 240 240 240,189 189 189,99 99 99
c_local P4 247 247 247,204 204 204,150 150 150,82 82 82
c_local P5 247 247 247,204 204 204,150 150 150,99 99 99,37 37 37
c_local P6 247 247 247,217 217 217,189 189 189,150 150 150,99 99 99,37 37 37
c_local P7 247 247 247,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37
c_local P8 255 255 255,240 240 240,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37
c_local P9 255 255 255,240 240 240,217 217 217,189 189 189,150 150 150,115 115 115,82 82 82,37 37 37,0 0 0
}
else {
c_local P3 0 0 0 .06,0 0 0 .26,0 0 0 .61
c_local P4 0 0 0 .03,0 0 0 .2,0 0 0 .41,0 0 0 .68
c_local P5 0 0 0 .03,0 0 0 .2,0 0 0 .41,0 0 0 .61,0 0 0 .85
c_local P6 0 0 0 .03,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .61,0 0 0 .85
c_local P7 0 0 0 .03,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85
c_local P8 0 0 0 0,0 0 0 .06,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85
c_local P9 0 0 0 0,0 0 0 .06,0 0 0 .15,0 0 0 .26,0 0 0 .41,0 0 0 .55,0 0 0 .68,0 0 0 .85,0 0 0 1
c_local note (CMYK)
}
end

program colorpalette9_OrRd
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 254 232 200,253 187 132,227 74 51
c_local P4 254 240 217,253 204 138,252 141 89,215 48 31
c_local P5 254 240 217,253 204 138,252 141 89,227 74 51,179 0 0
c_local P6 254 240 217,253 212 158,253 187 132,252 141 89,227 74 51,179 0 0
c_local P7 254 240 217,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,153 0 0
c_local P8 255 247 236,254 232 200,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,153 0 0
c_local P9 255 247 236,254 232 200,253 212 158,253 187 132,252 141 89,239 101 72,215 48 31,179 0 0,127 0 0
}
else {
c_local P3 0 .09 .18 0,0 .27 .4 0,.1 .7 .7 0
c_local P4 0 .06 .12 0,0 .2 .4 0,0 .45 .55 0,.15 .8 .8 0
c_local P5 0 .06 .12 0,0 .2 .4 0,0 .45 .55 0,.1 .7 .7 0,.3 1 1 0
c_local P6 0 .06 .12 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.1 .7 .7 0,.3 1 1 0
c_local P7 0 .06 .12 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.4 1 1 0
c_local P8 0 .03 .06 0,0 .09 .18 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.4 1 1 0
c_local P9 0 .03 .06 0,0 .09 .18 0,0 .17 .32 0,0 .27 .4 0,0 .45 .55 0,.05 .6 .6 0,.15 .8 .8 0,.3 1 1 0,.5 1 1 0
c_local note (CMYK)
}
end

program colorpalette9_Oranges
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 254 230 206,253 174 107,230 85 13
c_local P4 254 237 222,253 190 133,253 141 60,217 71 1
c_local P5 254 237 222,253 190 133,253 141 60,230 85 13,166 54 3
c_local P6 254 237 222,253 208 162,253 174 107,253 141 60,230 85 13,166 54 3
c_local P7 254 237 222,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,140 45 4
c_local P8 255 245 235,254 230 206,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,140 45 4
c_local P9 255 245 235,254 230 206,253 208 162,253 174 107,253 141 60,241 105 19,217 72 1,166 54 3,127 39 4
}
else {
c_local P3 0 .1 .15 0,0 .32 .5 0,.1 .65 .95 0
c_local P4 0 .07 .1 0,0 .26 .4 0,0 .45 .7 0,.15 .7 1 0
c_local P5 0 .07 .1 0,0 .26 .4 0,0 .45 .7 0,.1 .65 .95 0,.35 .75 1 0
c_local P6 0 .07 .1 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.1 .65 .95 0,.35 .75 1 0
c_local P7 0 .07 .1 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.45 .78 1 0
c_local P8 0 .04 .06 0,0 .1 .15 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.45 .78 1 0
c_local P9 0 .04 .06 0,0 .1 .15 0,0 .19 .3 0,0 .32 .5 0,0 .45 .7 0,.05 .58 .9 0,.15 .7 1 0,.35 .75 1 0,.5 .8 1 0
c_local note (CMYK)
}
end

program colorpalette9_PRGn
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 175 141 195,247 247 247,127 191 123
c_local P4 123 50 148,194 165 207,166 219 160,0 136 55
c_local P5 123 50 148,194 165 207,247 247 247,166 219 160,0 136 55
c_local P6 118 42 131,175 141 195,231 212 232,217 240 211,127 191 123,27 120 55
c_local P7 118 42 131,175 141 195,231 212 232,247 247 247,217 240 211,127 191 123,27 120 55
c_local P8 118 42 131,153 112 171,194 165 207,231 212 232,217 240 211,166 219 160,90 174 97,27 120 55
c_local P9 118 42 131,153 112 171,194 165 207,231 212 232,247 247 247,217 240 211,166 219 160,90 174 97,27 120 55
c_local P10 64 0 75,118 42 131,153 112 171,194 165 207,231 212 232,217 240 211,166 219 160,90 174 97,27 120 55,0 68 27
c_local P11 64 0 75,118 42 131,153 112 171,194 165 207,231 212 232,247 247 247,217 240 211,166 219 160,90 174 97,27 120 55,0 68 27
}
else {
c_local P3 .31 .38 0 0,0 0 0 .03,.5 .05 .5 0
c_local P4 .53 .77 0 0,.23 .3 0 0,.35 0 .35 0,1 0 1 0
c_local P5 .53 .77 0 0,.23 .3 0 0,0 0 0 .03,.35 0 .35 0,1 0 1 0
c_local P6 .55 .8 .1 0,.31 .38 0 0,.09 .14 0 0,.15 0 .15 0,.5 .05 .5 0,.9 .2 .9 0
c_local P7 .55 .8 .1 0,.31 .38 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.5 .05 .5 0,.9 .2 .9 0
c_local P8 .55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0
c_local P9 .55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0
c_local P10 .6 1 0 .4,.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0,1 .5 1 0
c_local P11 .6 1 0 .4,.55 .8 .1 0,.4 .49 .05 0,.23 .3 0 0,.09 .14 0 0,0 0 0 .03,.15 0 .15 0,.35 0 .35 0,.65 .05 .65 0,.9 .2 .9 0,1 .5 1 0
c_local note (CMYK)
}
end

program colorpalette9_Paired
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 166 206 227,31 120 180,178 223 138,51 160 44,251 154 153,227 26 28,253 191 111,255 127 0,202 178 214,106 61 154,255 255 153,177 89 40
}
else {
c_local P .35 .07 0 0,.9 .3 0 0,.3 0 .45 0,.8 0 1 0,0 .4 .25 0,.1 .9 .8 0,0 .25 .5 0,0 .5 1 0,.2 .25 0 0,.6 .7 0 0,0 0 .4 0,.23 .73 .98 .12
c_local note (CMYK)
}
end

program colorpalette9_Pastel1
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 251 180 174,179 205 227,204 235 197,222 203 228,254 217 166,255 255 204,229 216 189,253 218 236,242 242 242
}
else {
c_local P 0 .3 .2 0,.3 .1 0 0,.2 0 .2 0,.12 .17 0 0,0 .15 .3 0,0 0 .2 0,.1 .12 .2 0,0 .15 0 0,0 0 0 .05
c_local note (CMYK)
}
end

program colorpalette9_Pastel2
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 179 226 205,253 205 172,203 213 232,244 202 228,230 245 201,255 242 174,241 226 204,204 204 204
}
else {
c_local P .3 0 .15 0,0 .2 .25 0,.2 .1 0 0,.03 .2 0 0,.1 0 .2 0,0 .05 .3 0,.05 .1 .15 0,0 0 0 .2
c_local note (CMYK)
}
end

program colorpalette9_PiYG
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 233 163 201,247 247 247,161 215 106
c_local P4 208 28 139,241 182 218,184 225 134,77 172 38
c_local P5 208 28 139,241 182 218,247 247 247,184 225 134,77 172 38
c_local P6 197 27 125,233 163 201,253 224 239,230 245 208,161 215 106,77 146 33
c_local P7 197 27 125,233 163 201,253 224 239,247 247 247,230 245 208,161 215 106,77 146 33
c_local P8 197 27 125,222 119 174,241 182 218,253 224 239,230 245 208,184 225 134,127 188 65,77 146 33
c_local P9 197 27 125,222 119 174,241 182 218,253 224 239,247 247 247,230 245 208,184 225 134,127 188 65,77 146 33
c_local P10 142 1 82,197 27 125,222 119 174,241 182 218,253 224 239,230 245 208,184 225 134,127 188 65,77 146 33,39 100 25
c_local P11 142 1 82,197 27 125,222 119 174,241 182 218,253 224 239,247 247 247,230 245 208,184 225 134,127 188 65,77 146 33,39 100 25
}
else {
c_local P3 .07 .35 .03 0,0 0 0 .03,.37 0 .6 0
c_local P4 .15 .9 0 0,.04 .28 0 0,.28 0 .47 0,.7 0 1 0
c_local P5 .15 .9 0 0,.04 .28 0 0,0 0 0 .03,.28 0 .47 0,.7 0 1 0
c_local P6 .2 .9 .1 0,.07 .35 .03 0,0 .12 0 0,.1 0 .17 0,.37 0 .6 0,.7 .15 1 0
c_local P7 .2 .9 .1 0,.07 .35 .03 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.37 0 .6 0,.7 .15 1 0
c_local P8 .2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0
c_local P9 .2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0
c_local P10 .1 1 0 .35,.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0,.75 0 1 .4
c_local P11 .1 1 0 .35,.2 .9 .1 0,.11 .52 .06 0,.04 .28 0 0,0 .12 0 0,0 0 0 .03,.1 0 .17 0,.28 0 .47 0,.5 .05 .8 0,.7 .15 1 0,.75 0 1 .4
c_local note (CMYK)
}
end

program colorpalette9_PuBu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 236 231 242,166 189 219,43 140 190
c_local P4 241 238 246,189 201 225,116 169 207,5 112 176
c_local P5 241 238 246,189 201 225,116 169 207,43 140 190,4 90 141
c_local P6 241 238 246,208 209 230,166 189 219,116 169 207,43 140 190,4 90 141
c_local P7 241 238 246,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,3 78 123
c_local P8 255 247 251,236 231 242,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,3 78 123
c_local P9 255 247 251,236 231 242,208 209 230,166 189 219,116 169 207,54 144 192,5 112 176,4 90 141,2 56 88
}
else {
c_local P3 .07 .07 0 0,.35 .15 0 0,.85 .2 0 0
c_local P4 .05 .05 0 0,.26 .13 0 0,.55 .17 0 0,1 .3 0 0
c_local P5 .05 .05 0 0,.26 .13 0 0,.55 .17 0 0,.85 .2 0 0,1 .3 0 .2
c_local P6 .05 .05 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.85 .2 0 0,1 .3 0 .2
c_local P7 .05 .05 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .3
c_local P8 0 .03 0 0,.07 .07 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .3
c_local P9 0 .03 0 0,.07 .07 0 0,.18 .12 0 0,.35 .15 0 0,.55 .17 0 0,.8 .2 0 0,1 .3 0 0,1 .3 0 .2,1 .3 0 .5
c_local note (CMYK)
}
end

program colorpalette9_PuBuGn
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 236 226 240,166 189 219,28 144 153
c_local P4 246 239 247,189 201 225,103 169 207,2 129 138
c_local P5 246 239 247,189 201 225,103 169 207,28 144 153,1 108 89
c_local P6 246 239 247,208 209 230,166 189 219,103 169 207,28 144 153,1 108 89
c_local P7 246 239 247,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 100 80
c_local P8 255 247 251,236 226 240,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 100 80
c_local P9 255 247 251,236 226 240,208 209 230,166 189 219,103 169 207,54 144 192,2 129 138,1 108 89,1 70 54
}
else {
c_local P3 .07 .09 0 0,.35 .15 0 0,.9 .12 .27 0
c_local P4 .03 .05 0 0,.26 .13 0 0,.6 .15 0 0,1 .15 .35 0
c_local P5 .03 .05 0 0,.26 .13 0 0,.6 .15 0 0,.9 .12 .27 0,1 .25 .65 0
c_local P6 .03 .05 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.9 .12 .27 0,1 .25 .65 0
c_local P7 .03 .05 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .3 .7 0
c_local P8 0 .03 0 0,.07 .09 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .3 .7 0
c_local P9 0 .03 0 0,.07 .09 0 0,.18 .12 0 0,.35 .15 0 0,.6 .15 0 0,.8 .2 0 0,1 .15 .35 0,1 .25 .65 0,1 .5 .8 0
c_local note (CMYK)
}
end

program colorpalette9_PuOr
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 241 163 64,247 247 247,153 142 195
c_local P4 230 97 1,253 184 99,178 171 210,94 60 153
c_local P5 230 97 1,253 184 99,247 247 247,178 171 210,94 60 153
c_local P6 179 88 6,241 163 64,254 224 182,216 218 235,153 142 195,84 39 136
c_local P7 179 88 6,241 163 64,254 224 182,247 247 247,216 218 235,153 142 195,84 39 136
c_local P8 179 88 6,224 130 20,253 184 99,254 224 182,216 218 235,178 171 210,128 115 172,84 39 136
c_local P9 179 88 6,224 130 20,253 184 99,254 224 182,247 247 247,216 218 235,178 171 210,128 115 172,84 39 136
c_local P10 127 59 8,179 88 6,224 130 20,253 184 99,254 224 182,216 218 235,178 171 210,128 115 172,84 39 136,45 0 75
c_local P11 127 59 8,179 88 6,224 130 20,253 184 99,254 224 182,247 247 247,216 218 235,178 171 210,128 115 172,84 39 136,45 0 75
}
else {
c_local P3 .05 .35 .7 0,0 0 0 .03,.4 .35 0 0
c_local P4 .1 .6 1 0,0 .28 .55 0,.3 .25 0 0,.65 .7 0 0
c_local P5 .1 .6 1 0,0 .28 .55 0,0 0 0 .03,.3 .25 0 0,.65 .7 0 0
c_local P6 .3 .6 1 0,.05 .35 .7 0,0 .12 .24 0,.15 .1 0 0,.4 .35 0 0,.7 .8 .05 0
c_local P7 .3 .6 1 0,.05 .35 .7 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.4 .35 0 0,.7 .8 .05 0
c_local P8 .3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0
c_local P9 .3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0
c_local P10 .5 .7 1 0,.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0,.75 1 0 .4
c_local P11 .5 .7 1 0,.3 .6 1 0,.12 .46 .92 0,0 .28 .55 0,0 .12 .24 0,0 0 0 .03,.15 .1 0 0,.3 .25 0 0,.5 .45 .05 0,.7 .8 .05 0,.75 1 0 .4
c_local note (CMYK)
}
end

program colorpalette9_PuRd
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 231 225 239,201 148 199,221 28 119
c_local P4 241 238 246,215 181 216,223 101 176,206 18 86
c_local P5 241 238 246,215 181 216,223 101 176,221 28 119,152 0 67
c_local P6 241 238 246,212 185 218,201 148 199,223 101 176,221 28 119,152 0 67
c_local P7 241 238 246,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,145 0 63
c_local P8 247 244 249,231 225 239,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,145 0 63
c_local P9 247 244 249,231 225 239,212 185 218,201 148 199,223 101 176,231 41 138,206 18 86,152 0 67,103 0 31
}
else {
c_local P3 .09 .09 0 0,.2 .38 0 0,.1 .9 .15 0
c_local P4 .05 .05 0 0,.15 .25 0 0,.1 .6 0 0,.17 .95 .35 0
c_local P5 .05 .05 0 0,.15 .25 0 0,.1 .6 0 0,.1 .9 .15 0,.4 1 .47 0
c_local P6 .05 .05 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.1 .9 .15 0,.4 1 .47 0
c_local P7 .05 .05 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.43 1 .5 0
c_local P8 .03 .03 0 0,.09 .09 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.43 1 .5 0
c_local P9 .03 .03 0 0,.09 .09 0 0,.16 .23 0 0,.2 .38 0 0,.1 .6 0 0,.05 .85 .05 0,.17 .95 .35 0,.4 1 .47 0,.6 1 .75 0
c_local note (CMYK)
}
end

program colorpalette9_Purples
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 239 237 245,188 189 220,117 107 177
c_local P4 242 240 247,203 201 226,158 154 200,106 81 163
c_local P5 242 240 247,203 201 226,158 154 200,117 107 177,84 39 143
c_local P6 242 240 247,218 218 235,188 189 220,158 154 200,117 107 177,84 39 143
c_local P7 242 240 247,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,74 20 134
c_local P8 252 251 253,239 237 245,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,74 20 134
c_local P9 252 251 253,239 237 245,218 218 235,188 189 220,158 154 200,128 125 186,106 81 163,84 39 143,63 0 125
}
else {
c_local P3 .06 .05 0 0,.28 .18 0 0,.55 .48 0 0
c_local P4 .05 .04 0 0,.2 .15 0 0,.38 .3 0 0,.6 .6 0 0
c_local P5 .05 .04 0 0,.2 .15 0 0,.38 .3 0 0,.55 .48 0 0,.7 .8 0 0
c_local P6 .05 .04 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.55 .48 0 0,.7 .8 0 0
c_local P7 .05 .04 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.75 .9 0 0
c_local P8 .01 .01 0 0,.06 .05 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.75 .9 0 0
c_local P9 .01 .01 0 0,.06 .05 0 0,.14 .1 0 0,.26 .18 0 0,.38 .3 0 0,.5 .4 0 0,.6 .6 0 0,.7 .8 0 0,.8 1 0 0
c_local note (CMYK)
}
end

program colorpalette9_RdBu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 239 138 98,247 247 247,103 169 207
c_local P4 202 0 32,244 165 130,146 197 222,5 113 176
c_local P5 202 0 32,244 165 130,247 247 247,146 197 222,5 113 176
c_local P6 178 24 43,239 138 98,253 219 199,209 229 240,103 169 207,33 102 172
c_local P7 178 24 43,239 138 98,253 219 199,247 247 247,209 229 240,103 169 207,33 102 172
c_local P8 178 24 43,214 96 77,244 165 130,253 219 199,209 229 240,146 197 222,67 147 195,33 102 172
c_local P9 178 24 43,214 96 77,244 165 130,253 219 199,247 247 247,209 229 240,146 197 222,67 147 195,33 102 172
c_local P10 103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,209 229 240,146 197 222,67 147 195,33 102 172,5 48 97
c_local P11 103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,247 247 247,209 229 240,146 197 222,67 147 195,33 102 172,5 48 97
}
else {
c_local P3 .05 .45 .5 0,0 0 0 .03,.6 .15 0 0
c_local P4 .2 1 .75 0,.03 .35 .38 0,.43 .08 0 0,1 .3 0 0
c_local P5 .2 1 .75 0,.03 .35 .38 0,0 0 0 .03,.43 .08 0 0,1 .3 0 0
c_local P6 .3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,.18 .04 0 0,.6 .15 0 0,.9 .4 0 0
c_local P7 .3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.6 .15 0 0,.9 .4 0 0
c_local P8 .3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0
c_local P9 .3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0
c_local P10 .6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0,1 .5 0 .4
c_local P11 .6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .03,.18 .04 0 0,.43 .08 0 0,.75 .2 0 0,.9 .4 0 0,1 .5 0 .4
c_local note (CMYK)
}
end

program colorpalette9_RdGy
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 239 138 98,255 255 255,153 153 153
c_local P4 202 0 32,244 165 130,186 186 186,64 64 64
c_local P5 202 0 32,244 165 130,255 255 255,186 186 186,64 64 64
c_local P6 178 24 43,239 138 98,253 219 199,224 224 224,153 153 153,77 77 77
c_local P7 178 24 43,239 138 98,253 219 199,255 255 255,224 224 224,153 153 153,77 77 77
c_local P8 178 24 43,214 96 77,244 165 130,253 219 199,224 224 224,186 186 186,135 135 135,77 77 77
c_local P9 178 24 43,214 96 77,244 165 130,253 219 199,255 255 255,224 224 224,186 186 186,135 135 135,77 77 77
c_local P10 103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,224 224 224,186 186 186,135 135 135,77 77 77,26 26 26
c_local P11 103 0 31,178 24 43,214 96 77,244 165 130,253 219 199,255 255 255,224 224 224,186 186 186,135 135 135,77 77 77,26 26 26
}
else {
c_local P3 .05 .45 .5 0,0 0 0 0,0 0 0 .4
c_local P4 .2 1 .75 0,.03 .35 .38 0,0 0 0 .27,0 0 0 .75
c_local P5 .2 1 .75 0,.03 .35 .38 0,0 0 0 0,0 0 0 .27,0 0 0 .75
c_local P6 .3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 .12,0 0 0 .4,0 0 0 .7
c_local P7 .3 .9 .7 0,.05 .45 .5 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .4,0 0 0 .7
c_local P8 .3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7
c_local P9 .3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7
c_local P10 .6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7,0 0 0 .9
c_local P11 .6 1 .75 0,.3 .9 .7 0,.15 .6 .57 0,.03 .35 .38 0,0 .14 .16 0,0 0 0 0,0 0 0 .12,0 0 0 .27,0 0 0 .47,0 0 0 .7,0 0 0 .9
c_local note (CMYK)
}
end

program colorpalette9_RdPu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 253 224 221,250 159 181,197 27 138
c_local P4 254 235 226,251 180 185,247 104 161,174 1 126
c_local P5 254 235 226,251 180 185,247 104 161,197 27 138,122 1 119
c_local P6 254 235 226,252 197 192,250 159 181,247 104 161,197 27 138,122 1 119
c_local P7 254 235 226,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119
c_local P8 255 247 243,253 224 221,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119
c_local P9 255 247 243,253 224 221,252 197 192,250 159 181,247 104 161,221 52 151,174 1 126,122 1 119,73 0 106
}
else {
c_local P3 0 .12 .08 0,0 .38 .12 0,.2 .9 0 0
c_local P4 0 .08 .08 0,0 .3 .15 0,0 .6 .1 0,.3 1 0 0
c_local P5 0 .08 .08 0,0 .3 .15 0,0 .6 .1 0,.2 .9 0 0,.5 1 0 .05
c_local P6 0 .08 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.2 .9 0 0,.5 1 0 .05
c_local P7 0 .08 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05
c_local P8 0 .03 .03 0,0 .12 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05
c_local P9 0 .03 .03 0,0 .12 .08 0,0 .23 .15 0,0 .38 .12 0,0 .6 .1 0,.1 .8 0 0,.3 1 0 0,.5 1 0 .05,.7 1 0 .15
c_local note (CMYK)
}
end

program colorpalette9_RdYlBu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 252 141 89,255 255 191,145 191 219
c_local P4 215 25 28,253 174 97,171 217 233,44 123 182
c_local P5 215 25 28,253 174 97,255 255 191,171 217 233,44 123 182
c_local P6 215 48 39,252 141 89,254 224 144,224 243 248,145 191 219,69 117 180
c_local P7 215 48 39,252 141 89,254 224 144,255 255 191,224 243 248,145 191 219,69 117 180
c_local P8 215 48 39,244 109 67,253 174 97,254 224 144,224 243 248,171 217 233,116 173 209,69 117 180
c_local P9 215 48 39,244 109 67,253 174 97,254 224 144,255 255 191,224 243 248,171 217 233,116 173 209,69 117 180
c_local P10 165 0 38,215 48 39,244 109 67,253 174 97,254 224 144,224 243 248,171 217 233,116 173 209,69 117 180,49 54 149
c_local P11 165 0 38,215 48 39,244 109 67,253 174 97,254 224 144,255 255 191,224 243 248,171 217 233,116 173 209,69 117 180,49 54 149
}
else {
c_local P3 0 .45 .55 0,0 0 .25 0,.43 .11 0 0
c_local P4 .15 .9 .8 0,0 .32 .55 0,.33 .03 0 0,.85 .3 0 0
c_local P5 .15 .9 .8 0,0 .32 .55 0,0 0 .25 0,.33 .03 0 0,.85 .3 0 0
c_local P6 .15 .8 .75 0,0 .45 .55 0,0 .12 .4 0,.12 0 0 0,.43 .11 0 0,.75 .37 0 0
c_local P7 .15 .8 .75 0,0 .45 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.43 .11 0 0,.75 .37 0 0
c_local P8 .15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0
c_local P9 .15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0
c_local P10 .35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .4 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0,.85 .7 0 0
c_local P11 .35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .35 .55 0,0 .12 .4 0,0 0 .25 0,.12 0 0 0,.33 .03 0 0,.55 .15 0 0,.75 .37 0 0,.85 .7 0 0
c_local note (CMYK)
}
end

program colorpalette9_RdYlGn
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 252 141 89,255 255 191,145 207 96
c_local P4 215 25 28,253 174 97,166 217 106,26 150 65
c_local P5 215 25 28,253 174 97,255 255 191,166 217 106,26 150 65
c_local P6 215 48 39,252 141 89,254 224 139,217 239 139,145 207 96,26 152 80
c_local P7 215 48 39,252 141 89,254 224 139,255 255 191,217 239 139,145 207 96,26 152 80
c_local P8 215 48 39,244 109 67,253 174 97,254 224 139,217 239 139,166 217 106,102 189 99,26 152 80
c_local P9 215 48 39,244 109 67,253 174 97,254 224 139,255 255 191,217 239 139,166 217 106,102 189 99,26 152 80
c_local P10 165 0 38,215 48 39,244 109 67,253 174 97,254 224 139,217 239 139,166 217 106,102 189 99,26 152 80,0 104 55
c_local P11 165 0 38,215 48 39,244 109 67,253 174 97,254 224 139,255 255 191,217 239 139,166 217 106,102 189 99,26 152 80,0 104 55
}
else {
c_local P3 0 .45 .55 0,0 0 .25 0,.43 0 .65 0
c_local P4 .15 .9 .8 0,0 .32 .55 0,.35 0 .6 0,.9 0 .9 0
c_local P5 .15 .9 .8 0,0 .35 .55 0,0 0 .25 0,.35 0 .6 0,.9 0 .9 0
c_local P6 .15 .8 .75 0,0 .45 .55 0,0 .12 .42 0,.15 0 .45 0,.43 0 .65 0,.9 0 .9 0
c_local P7 .15 .8 .75 0,0 .45 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.43 0 .65 0,.9 0 .8 0
c_local P8 .15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0
c_local P9 .15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0
c_local P10 .35 1 .7 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0,1 .25 .9 0
c_local P11 .35 1 .75 0,.15 .8 .75 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.15 0 .45 0,.35 0 .6 0,.6 0 .65 0,.9 0 .8 0,1 .25 .9 0
c_local note (CMYK)
}
end

program colorpalette9_Reds
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 254 224 210,252 146 114,222 45 38
c_local P4 254 229 217,252 174 145,251 106 74,203 24 29
c_local P5 254 229 217,252 174 145,251 106 74,222 45 38,165 15 21
c_local P6 254 229 217,252 187 161,252 146 114,251 106 74,222 45 38,165 15 21
c_local P7 254 229 217,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,153 0 13
c_local P8 255 245 240,254 224 210,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,153 0 13
c_local P9 255 245 240,254 224 210,252 187 161,252 146 114,251 106 74,239 59 44,203 24 29,165 15 21,103 0 13
}
else {
c_local P3 0 .12 .12 0,0 .43 .43 0,.12 .82 .75 0
c_local P4 0 .1 .1 0,0 .32 .32 0,0 .59 .59 0,.2 .9 .8 0
c_local P5 0 .1 .1 0,0 .32 .32 0,0 .59 .59 0,.12 .82 .75 0,.35 .95 .85 0
c_local P6 0 .1 .1 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.12 .82 .75 0,.35 .95 .85 0
c_local P7 0 .1 .1 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.4 1 .9 0
c_local P8 0 .04 .04 0,0 .12 .12 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.4 1 .9 0
c_local P9 0 .04 .04 0,0 .12 .12 0,0 .27 .27 0,0 .43 .43 0,0 .59 .59 0,.05 .77 .72 0,.2 .9 .8 0,.35 .95 .85 0,.6 1 .9 0
c_local note (CMYK)
}
end

program colorpalette9_Set1
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 228 26 28,55 126 184,77 175 74,152 78 163,255 127 0,255 255 51,166 86 40,247 129 191,153 153 153
}
else {
c_local P .1 .9 .8 0,.8 .3 0 0,.7 0 .8 0,.4 .65 0 0,0 .5 1 0,0 0 .8 0,.35 .6 .8 0,0 .5 0 0,0 0 0 .4
c_local note (CMYK)
}
end

program colorpalette9_Set2
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 102 194 165,252 141 98,141 160 203,231 138 195,166 216 84,255 217 47,229 196 148,179 179 179
}
else {
c_local P .6 0 .3 0,0 .45 .5 0,.45 .25 0 0,.07 .45 0 0,.35 0 .7 0,0 .15 .8 0,.1 .2 .35 0,0 0 0 .3
c_local note (CMYK)
}
end

program colorpalette9_Set3
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P 141 211 199,255 255 179,190 186 218,251 128 114,128 177 211,253 180 98,179 222 105,252 205 229,217 217 217,188 128 189,204 235 197,255 237 111
}
else {
c_local P .45 0 .15 0,0 0 .3 0,.25 .2 0 0,0 .5 .4 0,.5 .15 0 0,0 .3 .55 0,.3 0 .6 0,0 .2 0 0,0 0 0 .15,.25 .45 0 0,.2 0 .2 0,0 .07 .55 0
c_local note (CMYK)
}
end

program colorpalette9_Spectral
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 252 141 89,255 255 191,153 213 148
c_local P4 215 25 28,253 174 97,171 221 164,43 131 186
c_local P5 215 25 28,253 174 97,255 255 191,171 221 164,43 131 186
c_local P6 213 62 79,252 141 89,254 224 139,230 245 152,153 213 148,50 136 189
c_local P7 213 62 79,252 141 89,254 224 139,255 255 191,230 245 152,153 213 148,50 136 189
c_local P8 213 62 79,244 109 67,253 174 97,254 224 139,230 245 152,171 221 164,102 194 165,50 136 189
c_local P9 213 62 79,244 109 67,253 174 97,254 224 139,255 255 191,230 245 152,171 221 164,102 194 165,50 136 189
c_local P10 158 1 66,213 62 79,244 109 67,253 174 97,254 224 139,230 245 152,171 221 164,102 194 165,50 136 189,94 79 162
c_local P11 158 1 66,213 62 79,244 109 67,253 174 97,254 224 139,255 255 191,230 245 152,171 221 164,102 194 165,50 136 189,94 79 162
}
else {
c_local P3 0 .45 .55 0,0 0 .25 0,.4 0 .4 0
c_local P4 .15 .9 .8 0,0 .32 .55 0,.33 0 .33 0,.85 .25 0 0
c_local P5 .15 .9 .8 0,0 .32 .55 0,0 0 .25 0,.33 0 .33 0,.85 .25 0 0
c_local P6 .15 .75 .5 0,0 .45 .55 0,0 .12 .42 0,.1 0 .4 0,.4 0 .4 0,.82 .23 0 0
c_local P7 .15 .75 .5 0,0 .45 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.4 0 .4 0,.82 .23 0 0
c_local P8 .15 .75 .5 0,.03 .57 .53 0,0 .32 .55 0,0 .12 .42 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0
c_local P9 .15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0
c_local P10 0 1 .2 .35,.15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0,.65 .6 0 0
c_local P11 0 1 .2 .35,.15 .75 .5 0,.03 .57 .63 0,0 .32 .55 0,0 .12 .42 0,0 0 .25 0,.1 0 .4 0,.33 0 .33 0,.6 0 .3 0,.82 .23 0 0,.65 .6 0 0
c_local note (CMYK)
}
end

program colorpalette9_YlGn
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 247 252 185,173 221 142,49 163 84
c_local P4 255 255 204,194 230 153,120 198 121,35 132 67
c_local P5 255 255 204,194 230 153,120 198 121,49 163 84,0 104 55
c_local P6 255 255 204,217 240 163,173 221 142,120 198 121,49 163 84,0 104 55
c_local P7 255 255 204,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 90 50
c_local P8 255 255 229,247 252 185,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 90 50
c_local P9 255 255 229,247 252 185,217 240 163,173 221 142,120 198 121,65 171 93,35 132 67,0 104 55,0 69 41
}
else {
c_local P3 .03 0 .27 0,.32 0 .43 0,.81 0 .76 0
c_local P4 0 0 .2 0,.24 0 .39 0,.53 0 .53 0,.87 .1 .83 0
c_local P5 0 0 .2 0,.24 0 .39 0,.53 0 .53 0,.81 0 .76 0,1 .25 .9 0
c_local P6 0 0 .2 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.81 0 .76 0,1 .25 .9 0
c_local P7 0 0 .2 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .35 .9 0
c_local P8 0 0 .1 0,.03 0 .27 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .35 .9 0
c_local P9 0 0 .1 0,.03 0 .27 0,.15 0 .35 0,.32 0 .43 0,.53 0 .53 0,.75 0 .7 0,.87 .15 .83 0,1 .25 .9 0,1 .5 .9 0
c_local note (CMYK)
}
end

program colorpalette9_YlGnBu
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 237 248 177,127 205 187,44 127 184
c_local P4 255 255 204,161 218 180,65 182 196,34 94 168
c_local P5 255 255 204,161 218 180,65 182 196,44 127 184,37 52 148
c_local P6 255 255 204,199 233 180,127 205 187,65 182 196,44 127 184,37 52 148
c_local P7 255 255 204,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,12 44 132
c_local P8 255 255 217,237 248 177,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,12 44 132
c_local P9 255 255 217,237 248 177,199 233 180,127 205 187,65 182 196,29 145 192,34 94 168,37 52 148,8 29 88
}
else {
c_local P3 .07 0 .3 0,.5 0 .2 0,.85 .27 0 0
c_local P4 0 0 .2 0,.37 0 .25 0,.75 0 .1 0,.9 .45 0 0
c_local P5 0 0 .2 0,.37 0 .25 0,.75 0 .1 0,.85 .27 0 0,.9 .7 0 0
c_local P6 0 0 .2 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.85 .27 0 0,.9 .7 0 0
c_local P7 0 0 .2 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,1 .7 0 .1
c_local P8 0 0 .15 0,.07 0 .3 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,1 .7 0 .1
c_local P9 0 0 .15 0,.07 0 .3 0,.22 0 .27 0,.5 0 .2 0,.75 0 .1 0,.9 .15 0 0,.9 .45 0 0,.9 .7 0 0,1 .7 0 .4
c_local note (CMYK)
}
end

program colorpalette9_YlOrBr
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 255 247 188,254 196 79,217 95 14
c_local P4 255 255 212,254 217 142,254 153 41,204 76 2
c_local P5 255 255 212,254 217 142,254 153 41,217 95 14,153 52 4
c_local P6 255 255 212,254 227 145,254 196 79,254 153 41,217 95 14,153 52 4
c_local P7 255 255 212,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,140 45 4
c_local P8 255 255 229,255 247 188,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,140 45 4
c_local P9 255 255 229,255 247 188,254 227 145,254 196 79,254 153 41,236 112 20,204 76 2,153 52 4,102 37 6
}
else {
c_local P3 0 .03 .25 0,0 .23 .65 0,.15 .6 .95 0
c_local P4 0 0 .17 0,0 .15 .4 0,0 .4 .8 0,.2 .67 1 0
c_local P5 0 0 .17 0,0 .15 .4 0,0 .4 .8 0,.15 .6 .95 0,.4 .75 1 0
c_local P6 0 0 .17 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.15 .6 .95 0,.4 .75 1 0
c_local P7 0 0 .17 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.45 .78 1 0
c_local P8 0 0 .1 0,0 .03 .25 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.45 .78 1 0
c_local P9 0 0 .1 0,0 .03 .25 0,0 .11 .4 0,0 .23 .65 0,0 .4 .8 0,.07 .55 .9 0,.2 .67 1 0,.4 .75 1 0,.6 .8 1 0
c_local note (CMYK)
}
end

program colorpalette9_YlOrRd
syntax [, cmyk * ]
if "`cmyk'"=="" {
c_local P3 255 237 160,254 178 76,240 59 32
c_local P4 255 255 178,254 204 92,253 141 60,227 26 28
c_local P5 255 255 178,254 204 92,253 141 60,240 59 32,189 0 38
c_local P6 255 255 178,254 217 118,254 178 76,253 141 60,240 59 32,189 0 38
c_local P7 255 255 178,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,177 0 38
c_local P8 255 255 204,255 237 160,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,177 0 38
c_local P9 255 255 204,255 237 160,254 217 118,254 178 76,253 141 60,252 78 42,227 26 28,189 0 38,128 0 38
}
else {
c_local P3 0 .07 .35 0,0 .3 .65 0,.05 .77 .8 0
c_local P4 0 0 .3 0,0 .2 .6 0,0 .45 .7 0,.1 .9 .8 0
c_local P5 0 0 .3 0,0 .2 .6 0,0 .45 .7 0,.05 .77 .8 0,.25 1 .7 0
c_local P6 0 0 .3 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,.05 .77 .8 0,.25 1 .7 0
c_local P7 0 0 .3 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.3 1 .7 0
c_local P8 0 0 .2 0,0 .07 .35 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.3 1 .7 0
c_local P9 0 0 .2 0,0 .07 .35 0,0 .15 .5 0,0 .3 .65 0,0 .45 .7 0,0 .7 .75 0,.1 .9 .8 0,.25 1 .7 0,.5 1 .7 0
c_local note (CMYK)
}
end

/*----------------------------------------------------------------------------*/
/* mata                                                                       */
/*----------------------------------------------------------------------------*/

version 9.2
mata:
mata set matastrict on

string scalar _parse_palette(string scalar p0)
{
    real scalar      i, j, blank
    string rowvector p
    
    p = tokens(p0, ",")
    if (length(p)<1) return("")
    blank = 1
    j = 0
    for (i=1;i<=length(p);i++) {
        if (p[i]==",") {
            if (blank) {
                j++
                p[j] = ""
            }
            blank = 1
            continue
        }
        j++
        p[j] = strtrim(p[i])
        blank = 0
    }
    return(_invtokens_quoted(p[|1\j|]))
}

void _listrecycle(string scalar lname, real scalar n)
{
    real scalar      i, l, l2
    string rowvector In, Out
    
    In = tokens(st_local(lname))
    l = length(In)
    if (l==0) return
    Out = J(1, n, "")
    for (i=1; i<=n; i=i+l) {
        l2 = n-i+1
        if (l2<l) Out[|i \ i+l2-1|] = In[|1 \ l2|]
        else      Out[|i \ i+l-1|]  = In
    }
    st_local(lname, _invtokens(Out))
}

string scalar _invtokens(string vector In)
{
    real scalar   i
    string scalar Out

    if (length(In)<1) return("")
    Out = In[1] 
    for (i=2; i<=length(In); i++) Out = Out + " " + In[i]
    return(Out)
}

string scalar _invtokens_quoted(string vector In)
{
    real scalar   i
    string scalar Out

    if (length(In)<1) return("")
    Out = "`" + `"""' + In[1] + `"""' + "'"
    for (i=2; i<=length(In); i++) {
        Out = Out + " `" + `"""' + In[i] + `"""' + "'"
    }
    return(Out)
}

void makeRGB(string scalar lname)
{
    string scalar c
    
    c = strtrim(st_local(lname))
    if      (substr(c,1,1)=="#")    c = HEXtoRGB(c)
    else if (substr(c,1,4)=="hcl ") c = HCLtoRGB(c)
    else return
    if (c!="") st_local(lname, c)
}

string scalar HEXtoRGB(string scalar c)
{
    string scalar r, g, b, rest
    real scalar   l
    
    c = strtrim(substr(c,2,.))  // get rid of #; allow blanks after #
    if ((l = strpos(c,"*"))) {  // take care of *... (intensity)
        rest = substr(c,l,.)
        c = strrtrim(substr(c,1,l-1)) // allow blanks before *
    }
    if ((l = strpos(c,"%"))) {  // take care of %... (opacity)
        rest = substr(c,l,.) + rest
        c = strrtrim(substr(c,1,l-1)) // allow blanks before %
    }
    l = strlen(c)
    if (l==3) c = substr(c,1,1)*2 + substr(c,2,1)*2 + substr(c,3,1)*2
    else if (l!=6) return("")
    r = strofreal(_HEXtoRGB(substr(c,1,2)))
    if (r==".") return("")
    g = strofreal(_HEXtoRGB(substr(c,3,2)))
    if (g==".") return("")
    b = strofreal(_HEXtoRGB(substr(c,5,2)))
    if (g==".") return("")
    return(r + " " + g + " " + b + rest)
}
real scalar _HEXtoRGB(string scalar s0)
{
    real scalar   d1, d2
    string scalar s, digits
    
    s = strlower(strtrim(s0))
    digits = "0123456789abcdef"
    d1 = strpos(digits,substr(s,1,1))
    if (d1==0) return(.)
    d2 = strpos(digits,substr(s,2,1))
    if (d2==0) return(.)
    return((d1-1)*16 + (d2-1))
}

string scalar HCLtoRGB(string scalar c)
{
    real scalar      l
    string scalar    rest
    string rowvector hcl
    real scalar      H, C, L
    
    c = strtrim(substr(c,5,.))  // get rid of hcl; allow blanks after hcl
    if ((l = strpos(c,"*"))) {  // take care of *... (intensity)
        rest = substr(c,l,.)
        c = strrtrim(substr(c,1,l-1)) // allow blanks before *
    }
    if ((l = strpos(c,"%"))) {  // take care of %... (opacity)
        rest = substr(c,l,.) + rest
        c = strrtrim(substr(c,1,l-1)) // allow blanks before %
    }
    hcl = tokens(c)
    if (cols(hcl)!=3) return("")
    H = strtoreal(hcl[1])
    if (H>=.) return("")
    C = strtoreal(hcl[2])
    if (C>=. | C<0) return("")
    L = strtoreal(hcl[3])
    if (L>100 | C<0) return("")
    return(HCL_to_RGB(H, C, L) + rest)
}

// The following colorspace translators are based on R's colorspace package

string scalar HSV_to_RGB(H, S, V) // H in [0,360], 0 <= S <=1, 0 <= V <=1
{
    real scalar    h, i, f, v, m, n
    
    h = H/60
    i = floor(h)
    f = h - i
    i = mod(i, 6) // wrap around if H outside [0,360)
    if (mod(i,2)==0) f = 1 - f
    v = V
    n = V * (1 - f * S)
    m = V * (1 - S)
         if (i==0) return(strRGB(v, n, m))
    else if (i==1) return(strRGB(n, v, m))
    else if (i==2) return(strRGB(m, v, n))
    else if (i==3) return(strRGB(m, n, v))
    else if (i==4) return(strRGB(n, m, v))
    else if (i==5) return(strRGB(v, m, n))
}

string scalar HCL_to_RGB(H, C, L)
{
    real scalar XN, YN, ZN
    real scalar h, U, V
    real scalar X, Y, Z, t, x, y, uN, vN, u, v
    real scalar R, G, B
    
    // CheckWhite()
    XN =  95.047; YN = 100.000; ZN = 108.883
    
    // polarLUV_to_LUV()
    h = H * pi() / 180
    U = C * cos(h)
    V = C * sin(h)
    
    // LUV_to_XYZ()
    if (L<=0 & U==0 & V==0) {
        X = 0; Y = 0; Z = 0
    }
    else {
        Y = YN * (L>7.999592 ? ((L + 16)/116)^3 : L / 903.3)
        // XYZ_to_uv()
            t = XN + YN + ZN
            x = XN / t; y = YN / t
            uN = 2 * x / (6 * y - x + 1.5)
            vN = 4.5 * y / (6 * y - x + 1.5)
        // done
        u = U / (13 * L) + uN
        v = V / (13 * L) + vN
        X = 9.0 * Y * u/(4 * v)
        Z = -X/3 - 5*Y + 3*Y/v
    }
    
    // XYZ_to_sRGB()
    R = gtrans(( 3.240479 * X - 1.537150 * Y - 0.498535 * Z) / YN, 2.4)
    G = gtrans((-0.969256 * X + 1.875992 * Y + 0.041556 * Z) / YN, 2.4)
    B = gtrans(( 0.055648 * X - 0.204043 * Y + 1.057311 * Z) / YN, 2.4)
    
    // return
    return(strRGB(R, G, B))
}

real scalar gtrans(real scalar u, real scalar gamma)
{
    if (u > 0.00304) return(1.055 * u^(1 / gamma) - 0.055)
    else             return(12.92 * u)
}

string scalar strRGB(real scalar r, real scalar g, real scalar b)
{
    return(strofreal(min((max((0 , trunc(255 * r + 0.5))), 255))) + " " + 
           strofreal(min((max((0 , trunc(255 * g + 0.5))), 255))) + " " + 
           strofreal(min((max((0 , trunc(255 * b + 0.5))), 255))))
}


// Functions for color interpolation

struct color {
    real scalar     type // 0 RGB, 1 HSV, 2 CMYK (>1), 3 CMYK (<=1)
    real scalar     intensity, opacity
    real rowvector  code
}

string colvector ipolate_colors(string colvector P, real scalar n)
{
    /* 
    P: input vector containing color codes (RGB, CMYK, HSV) or named colors,
       possible including intensity and opacity adjustment
    n: number of desired output colors
    */
    real scalar      r, i
    real scalar      ctype  // 0 RGB, 1 HSV, 2 CMYK (>1), 3 CMYK (<=1)
    string colvector S
    real matrix      C
    real colvector   from, to, I, O 
    struct color scalar color

    // analyze first element
    if ((r = length(P))==0) return("") // empty list
    color = parse_color(P[1])
    ctype = color.type

    // collect colors
    C = J(r, cols(color.code), .)
    C[1,] = color.code
    if (color.intensity<.) {
        I = J(r, 1, 1)
        I[1] = color.intensity
    }
    if (color.opacity<.) {
        O = J(r, 1, 1)
        O[1] = color.opacity
    }
    for (i=2; i<=r; i++) {
        color = parse_color(P[i])
        if (color.type!=ctype) ERROR_incompatible_colortypes()
        C[i,] = color.code
        if (color.intensity<.) {
            if (length(I)==0) I = J(r, 1, 1)
            I[i] = color.intensity
        }
        if (color.opacity<.) {
            if (length(O)==0) O = J(r, 1, 100)
            O[i] = color.opacity
        }
    }

    // if only one color: duplicate to make interpolation work
    if (r==1) {
        C = C \ C; I = I \ I; O = O \ O
        r = 2
    }
    
    // interpolate
    from = rangen(0, 1, r)
    to   = rangen(0, 1, n)
    if (length(I)>0) I = _ipolate(from, I, to)
    if (length(O)>0) O = _ipolate(from, O, to)
    if (cols(C)==4) { // CMYK
        C = _ipolate(from, C[,1], to), _ipolate(from, C[,2], to), 
            _ipolate(from, C[,3], to), _ipolate(from, C[,4], to)
    }
    else { // RGB/HSV
        C = _ipolate(from, C[,1], to), _ipolate(from, C[,2], to), 
            _ipolate(from, C[,3], to)
    }
    if (ctype==0)      C = round(C) // RGB
    else if (ctype==1) C = round(C, (1, .001, .001)) // HSV
    else if (ctype==2) C = round(C) // CMYK (>1)
    else if (ctype==3) C = round(C, .001) // CMYK (>1)

    // return
    return(                 (ctype==1 ? "hsv " : "") :+
                              strofreal(C[,1])       :+ 
                       " " :+ strofreal(C[,2])       :+ 
                       " " :+ strofreal(C[,3])       :+ 
        (cols(C)==4  ? " " :+ strofreal(C[,4]) : "") :+ 
        (length(I)>0 ? "*" :+ strofreal(I)     : "") :+ 
        (length(O)>0 ? "%" :+ strofreal(O)     : "")
        )
}

void ERROR_incompatible_colortypes()
{
    display("{err}interpolation not possible due to incompatible color specifications")
    exit(499)
}

real colvector _ipolate(real colvector x, real colvector y,
 real colvector xnew, | real scalar outer)
{   /* renamed mm_ipolate() (version 1.0.6, Ben Jann, 10jul2006) from moremata */
    real scalar i, j0b, j0e, j1b, j1e, r, xlo, xup, xi, y0, y1
    real colvector p, pnew, ynew

    r = rows(x)
    if (rows(y)!=r) _error(3200)
    if (r<1) return(J(rows(xnew), 1, .))
    if (args()<4) outer = 0

    p = order(x, 1)
    pnew = order(xnew, 1)
    ynew = J(rows(xnew),1,.)
    xlo = x[p[1]]; xup = x[p[rows(p)]]
    j0b = j1e = j0e = 1
    for (i=1; i<=rows(xnew); i++) {
        xi = xnew[pnew[i]]
        if (outer==0) {
            if (xi<xlo) continue
            if (xi>xup) return(ynew)
        }
        while (j0e<r) {
            if (x[p[j0e+1]]>xi) break
            j0e++
            if (x[p[j0e]]>x[p[j0b]]) j0b = j0e
        }
        if (j0e>=j1e) {
            j1b = j0e
            while (j1b<=r) {
                if (x[p[j1b]]>=xi) break
                j1b++
            }
            if (j1b>r) j1b = r
            j1e = j1b
            while (j1e<r) {
                if (x[p[j1e+1]]>x[p[j1b]]) break
                j1e++
            }
        }
        y0 = (j0b==j0e ? y[p[j0b]] : mean(y[p[|j0b \ j0e|]],1))
        y1 = (j1b==j1e ? y[p[j1b]] : mean(y[p[|j1b \ j1e|]],1))
        if (outer) {
            if (xi<xlo) {
                ynew[pnew[i]] = y1
                continue
            }
            if (xi>xup) {
                ynew[pnew[|i \ rows(pnew)|]] = J(rows(pnew)-i+1,1,y0)
                return(ynew)
            }
        }
        ynew[pnew[i]] = ( j0e==j1e ? y0 :
         y0 + (y1-y0) *  (xi-x[p[j0e]])/(x[p[j1b]]-x[p[j0e]]) )
    }
    return(ynew)
}

struct color scalar parse_color(string scalar s)
{
    /* function to parse a color specification; also reads the definitions
       of named colors from style files */
    real scalar         l
    string colvector    tmp
    struct color scalar color
    
    tmp = tokens(s, " *%")
    l = length(tmp)
    if (l<1) ERROR_invalid_color(s)
    // check for hsv prefix
    if (tmp[1]=="hsv") {
        if (l<4) ERROR_invalid_color(s)
        color.type = 1
        tmp = tmp[|2 \ l|]
        l = length(tmp)
    }
    else if (strtoreal(tmp[1])>=.) { // named color
        if (l>1) tmp = read_colorstyle(tmp[1]), tmp[|2 \ l|]
        else     tmp = read_colorstyle(tmp[1])
        l = length(tmp)
    }
    if (l<3) ERROR_invalid_color(s)
    // read first three codes
    color.code = strtoreal(tmp[|1 \ 3|])
    if (missing(color.code)) ERROR_invalid_color(s)
    if (l==3) {
        if (missing(color.type)) color.type = 0 // RGB
        return(color)
    }
    tmp = tmp[|4 \ l|]
    l = length(tmp)
    // read fourth code (CMYK)
    if (strtoreal(tmp[1])<.) {
        color.code = color.code, strtoreal(tmp[1])
        if (missing(color.type)) color.type = 2 // CMYK
        else ERROR_invalid_color(s)
        if (all(color.code:<=1)) color.type = 3 // CMYK (<=1)
        if (l==1) return(color)
        tmp = tmp[|2 \ l|]
        l = length(tmp)
    }
    else if (missing(color.type)) color.type = 0 // RGB
    // read intensity and transparency
    if (tmp[1]=="*") {
        if (l<2) ERROR_invalid_color(s)
        color.intensity = strtoreal(tmp[2])
        if (missing(color.intensity)) ERROR_invalid_color(s)
        if (l==2) return(color)
        tmp = tmp[|3 \ l|]
        l = length(tmp)
        if (l!=2) ERROR_invalid_color(s)
        if (tmp[1]!="%") ERROR_invalid_color(s)
        color.opacity = strtoreal(tmp[2])
        if (missing(color.opacity)) ERROR_invalid_color(s)
        return(color)
    }
    // read transparency and intensity
    if (tmp[1]=="%") {
        if (l<2) ERROR_invalid_color(s)
        color.opacity = strtoreal(tmp[2])
        if (missing(color.opacity)) ERROR_invalid_color(s)
        if (l==2) return(color)
        tmp = tmp[|3 \ l|]
        l = length(tmp)
        if (l!=2) ERROR_invalid_color(s)
        if (tmp[1]!="*") ERROR_invalid_color(s)
        color.intensity = strtoreal(tmp[2])
        if (missing(color.intensity)) ERROR_invalid_color(s)
        return(color)
    }
    // can only be reached if invalid
    ERROR_invalid_color(s)
}

string rowvector read_colorstyle(string scalar s)
{
    /* read RGB code of named color from style file */
    real scalar      fh, i
    string scalar    fn, line
    string matrix    EOF
    
    if (!st_isname(s)) ERROR_invalid_color(s)
    fn = findfile("color-"+s+".style")
    if (fn=="") ERROR_color_not_found(s)
    fh  = fopen(fn, "r")
    EOF = J(0, 0, "")
    while ((line=fget(fh))!=EOF) {
        line = strtrim(stritrim(line))
        if (substr(line, 1, 8)=="set rgb ") {
            line = tokens(substr(line, 9, .))
            if (length(line)!=1) continue
            fclose(fh)
            return(tokens(line))
        }
    }
    fclose(fh)
    ERROR_color_not_found(s)
}

void ERROR_invalid_color(string scalar s)
{
    display("{err}" + s + ": invalid color specification")
    exit(499)
}

void ERROR_color_not_found(string scalar s)
{
    display("{err}" + s + ": color definition not found")
    exit(499)
}

end

exit
