*! version 1.0.1  27dec2018  Ben Jann

program linepalette
    version 9.2
    capt _on_colon_parse `0'
    if _rc==0 {
        local 0 `"`s(before)'"'
        local rhs `"`s(after)'"'
        _parse comma lhs 0 : 0
        if `"`lhs'"'!="" error 198
        if `"`rhs'"'=="" local rhs default
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
    Palette_Get `0'
    if "`GRAPH'"=="" {
        tempname hcurrent
        _return hold `hcurrent'
        _return restore `hcurrent', hold // make copy
        Graph, `GROPTS'
        _return restore `hcurrent'
    }
end

/*----------------------------------------------------------------------------*/
/* retrieve palette                                                           */
/*----------------------------------------------------------------------------*/

program Palette_Get, rclass
    syntax [anything(name=palette id="palette" everything equalok)] ///
        [, noGRaph GRopts(str asis) TItle(passthru) LWidth(passthru) rows(passthru) ///
        N(numlist max=1 integer >=1) Select(numlist integer >=1) Reverse * ]
    c_local GRAPH "`graph'"
    c_local GROPTS `"`rows' `title' `lwidth' `gropts'"'
    // get palette
    if `"`palette'"'=="" local palette default
    local islist = (`: list sizeof palette'!=1)
    if `islist'==0 {
        capt confirm name _`palette'
        if _rc local islist 1
    }
    if `islist'==0 {
        capt _Palette_Get `palette', n(`n') `options'
        if _rc==199 {
            capt confirm name `palette'
            if _rc { // numeric palette name: cannot be a named style
                di as err `"palette `palette' not found"'
                exit 198
            }
            local islist 1
        }
        else if _rc { // display error message
            _Palette_Get `palette', n(`n') `options'
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
    // return palette
    local plist
    local i 0
    foreach j of local select {
        if `"`p`j''"'!="" {
            local ++i
            local plist `"`plist'`space'`"`p`j''"'"'
            local space " "
            return local p`i' `"`p`j''"'
            return local p`i'info `"`p`j'info'"'
        }
    }
    local n `i'
    local plist: list clean plist
    return local p `"`plist'"'
    return local pnote `"`note'"'
    return local pname `"`palette'"'
    return local ptype "line"
    return scalar n = `n'
end

program _Palette_Get
    gettoken palette 0 : 0, parse(" ,")
    syntax [, n(numlist max=1 integer >0) * ]
    linepalette_`palette', n(`n') `options'
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
    syntax [, rows(int 5) TItle(passthru) LWidth(passthru) * ]
    if `"`lwidth'"'=="" local lwidth lwidth(medthick)
    local n = r(n)
    local c = max(3,ceil(sqrt(`n'/12*3)))
    local cut = max(`rows',ceil(`n'/`c'))
    local rows = max(5, `cut')
    local lblgap = 10/`rows'
    local infogap = 10/`rows'
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
        local j2 = `j' + .6
        local jlab = `jlab' + .3
        local plots `plots' (pci `r' `j' `r' `j2', recast(pcspike) ///
                lpattern(`"`r(p`i')'"') `lwidth' lcolor(black))
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
        local pnumsize vsmall
        local lblsize tiny
        local infosize half_tiny
    }
    else if `rows'>=15 {
        local pnumsize small
        local lblsize vsmall
        local infosize tiny
    }
    else if `rows'>=10 {
        local pnumsize medsmall
        local lblsize small
        local infosize vsmall
    }
    else {
        local pnumsize medium 3.8194
        local lblsize medsmall
        local infosize small
    }
    local pnum (scatteri `pnum', ms(i) msize(`size') mlabpos(9) ///
            mlabgap(`lblgap') mlabsize(`pnumsize') mlabcolor(gray))
    if `"`lbl'"'!="" {
        local lbl (scatteri `lbl', ms(i) msize(`size') mlabpos(6) ///
            mlabgap(`lblgap') mlabsize(`lblsize') mlabcolor(gray))
    }
    if `"`info'"'!="" {
        local info (scatteri `info', ms(i) msize(`size') mlabpos(12) ///
            mlabgap(`infogap') mlabsize(`infosize') mlabcolor(gray))
    }
    else local info
    local l = 1/2 + 9
    local r = 1/2 + 5
    local b = 1/2 + 10
    local t = 1/2 + 5
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
        GRopts(str asis) LWidth(passthru) VERTical HORizontal * ]
    if `"`labels'"'!="" local plabels `"`labels'"'
    if `"`lwidth'"'=="" local lwidth lwidth(medthick)
    local orientation `vertical' `horizontal'
    if "`orientation'"=="" local orientation vertical
    if "`orientation'"=="horizontal" {
        local ii i
        local jj j
    }
    else {
        local ii j
        local jj i
    }
    local N 1
    local plots
    local i 0
    foreach p of local palettes {
        local ++i
        _parse comma pnm popts : p
        if `"`popts'"'=="" local popts ,
        Palette_Get `pnm' `popts' `options'
        local n = r(n)
        local N = max(`n',`N')
        gettoken plab plabels : plabels
        if `"`plab'"'=="" {
            if `"`r(pnote)'"'=="" local plab `"`r(pname)'"'
            else                  local plab `"`r(pname)' `r(pnote)'"'
        }
        local ylab `ylab' `i' `"`plab'"'
        forv j=1/`n' {
            local plots `plots' ///
                (pci ``ii'' `=``jj''-.35' ``ii'' `=``jj''+.35', ///
                recast(pcspike) lpattern(`"`r(p`j')'"') `lwidth' lcolor(black))
        }
    }
    if `"`plots'"'=="" {
        di as txt "(nothing to display)"
        exit
    }
    if "`orientation'"=="horizontal" {
        local xscale xscale(lstyle(none) range(.5 `N'.5))
        local xlabel xlabel(1/`N', notick)
        local yscale yscale(lstyle(none) range(.5 `i'.5) reverse)
        local ylabel ylabel(`ylab', nogrid notick angle(hor))
    }
    else {
        local xscale xscale(lstyle(none) range(.5 `i'.5) alt)
        local xlabel xlabel(`ylab', notick)
        local yscale yscale(lstyle(none) range(.5 `N'.5) reverse)
        local ylabel ylabel(1/`N', nogrid notick angle(hor))
    }
    two `plots', `xscale' `xlabel' xti("") `yscale' `ylabel' yti("") ///
        legend(off) graphr(margin(l=2 t=2 b=2 r=2) color(white)) ///
        scheme(s2color) `title' `gropts'
end

/*----------------------------------------------------------------------------*/
/* palettes                                                                   */
/*----------------------------------------------------------------------------*/

program linepalette_default
c_local P solid,dash,vshortdash,longdash_dot,longdash,dash_dot,dot, ///
          shortdash_dot,tight_dot,dash_dot_dot,longdash_shortdash,dash_3dot, ///
          longdash_dot_dot,shortdash_dot_dot,longdash_3dot
end

program linepalette_pplain // plotplain (p#lineplot)
c_local P solid,dash,vshortdash,dot,dash_dot_dot,tight_dot,longdash,dash_dot, ///
          shortdash_dot,longdash_dot,longdash_shortdash,dash_3dot,longdash_dot_dot, ///
          shortdash_dot_dot,longdash_3dot
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

end

exit
