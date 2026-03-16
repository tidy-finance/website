*! version 1.0.1  18may2024  Ben Jann

program colorcheck
    version 14.2
    syntax [, NOGRaph ///
        TItle(passthru) GRopts(passthru) HORizontal VERTical ///
        span LABels(passthru) LColor(passthru) LWidth(passthru) ///
        BARWidth(passthru) NONUMbers sort SORT2(passthru) * ]
    if "`horizontal'"!="" & "`vertical'"!="" {
        di as err "may not combine options horizontal and vertical"
        exit 198
    }
    local noinfo 0
    if `"`r(ptype)'"'!="color" local noinfo 1
    else if `"`r(p)'"'==""     local noinfo 1
    if `noinfo' {
        di as err "no color info found; need to run {bf:colorpalette} " /*
            */ "before applying {bf:colorcheck}"
        exit 498
    }
    tempname rhold
    _return hold `rhold'
    _return restore `rhold', hold
    capt n _colorcheck, `options'
    if _rc {
        _return restor `rhold'
        exit _rc
    }
    else {
        _return drop `rhold'
    }
    if "`change'`nograph'"!="" {
        _colorcheck_display
    }
    if "`nograph'"!="" exit
    _return hold `rhold'
    _return restore `rhold', hold
    capt n _colorcheck_graph, `title' `gropts' `horizontal' /*
        */ `vertical' `labels' `lcolor' `lwidth' `barwidth' /*
        */ `nonumbers' `sort' `sort2'
    _return restor `rhold'
    if _rc exit _rc
end

program _colorcheck_display
    di 
    di _col(20) as txt "Number of colors    =" as res %8.0g r(n)
    di _col(20) as txt "N. of comparisons   =" as res %8.0g comb(r(n),2)
    di _col(20) as txt "CVD severity        =" as res %8.0g r(cvd)
    di _col(20) as txt "Proportion of gray  =" as res %8.0g r(mono)
    di _col(20) as txt "Grayscale method    =" as res %8s r(mono_method)
    di _col(20) as txt "Difference metric   =" as res %8s r(metric)
    matlist r(delta), rowtitle("Delta E") border(rows) twidth(14)
end

program _colorcheck, rclass
    // defaults
    local d_metric      "Jab"
    local d_mono        1
    local d_mono_method "LCh"
    local d_cvd         1
    // syntax
    syntax [, Metric(str) mono(str) cvd(numlist max=1 >=0 <=1) ]
    _parse_mono `mono'
    if `"`metric'"'==""      local metric      "`d_metric'"
    if `"`mono'"'==""        local mono        `d_mono'
    if `"`mono_method'"'=="" local mono_method "`d_mono_method'"
    if `"`cvd'"'==""         local cvd         `d_cvd'
    // check whether results are already computed and settings did not change
    capt confirm matrix r(delta)
    if _rc==1 exit _rc
    if _rc==0 {
        // r(delta) exists
        local change 0
        if      `"`metric'"'!=`"`r(metric)'"'           local change 1
        else if `"`mono'"'!=`"`r(mono)'"'               local change 1
        else if `"`mono_method'"'!=`"`r(mono_method)'"' local change 1
        else if `"`cvd'"'!=`"`r(cvd)'"'                 local change 1
        if `change'==0 {
            return add
            exit // nothing to do
        }
    }
    c_local change "change"
    // obtain color info
    local pclass `"`r(pclass)'"'
    local colors `"`r(p)'"'
    return add
    // compute results
    tempname delta
    mata: colorcheck()
    mat coln `delta' = minimum maximum mean
    mat rown `delta' = "normal sight" /*
        */ monochromacy deuteranomaly protanomaly tritanomaly
    // return results
    return local  metric      `"`metric'"'
    return scalar mono        = `mono'
    return local  mono_method `"`mono_method'"'
    return scalar cvd         = `cvd'
    return matrix delta       = `delta'
    return local  p_norm      `"`p_norm'"'
    return local  p_mono      `"`p_mono'"'
    return local  p_deut      `"`p_deut'"'
    return local  p_prot      `"`p_prot'"'
    return local  p_trit      `"`p_trit'"'
end

program _parse_mono
    _parse comma p opts : 0
    gettoken comma opts : opts, parse(",")
    local 0 `", mono(`p') `opts'"'
    syntax [, mono(numlist >=0 <=1) * ]
    if "`mono'"=="" local mono 1
    c_local mono `mono'
    c_local mono_method `"`options'"'
end

program _colorcheck_graph
    syntax [, TItle(passthru) GRopts(str asis) HORizontal VERTical ///
              LABels(passthru) LColor(passthru) LWidth(passthru) ///
              BARWidth(passthru) NONUMbers sort SORT2(str) ]
    if `"`sort2'"'!="" local sort sort
    if "`sort'"!=""    _parse_sort, `sort2'
    if `"`title'"'=="" local title title(`r(pname)')
    local pct `:di % 7.0g r(mono)*100'
    local cvd `:di % 7.0g r(cvd)*100'
    if "`vertical'"!="" {
        if `"`labels'"'=="" {
            local haspct = !(`pct'==100 & `cvd'==100)
            local lbl "Normal sight"
            if `haspct' local lbl `""`lbl'" " ""'
            local labels `"`"`lbl'"'"'
            local lbl "Monochromacy"
            if `pct'!=100    local lbl `""`lbl'" "(`pct' percent)""'
            else if `haspct' local lbl `""`lbl'" " ""'
            local labels `"`labels' `"`lbl'"'"'
            local i 2
            foreach nm in Deuter Prot Trit {
                local ++i
                if `cvd'!=100    local lbl `""`nm'anomaly" "(`cvd' percent)""'
                else if `haspct' local lbl `""`nm'anopia" " ""'
                else             local lbl "`nm'anopia"
                local labels `"`labels' `"`lbl'"'"'
            }
            local labels labels(`labels')
        }
        local mlab
        forv i = 1/5 {
            local di `: di %9.1f el(r(delta),`i',1)'
            local mlab `"`mlab'`i' "{&Delta}E {&ge} `di'" "'
        }
        local gopts xlabel(,labgap(4)) xmlabel(`mlab', labsize(medsmall) notick labgap(-1))
    }
    else {
        local txt .5 .5 "Normal sight"
        if `pct'==100 local txt `txt' 1.5 .5 "Monochromacy"
        else local txt `txt' 1.5 .5 "Monochromacy (`pct' percent)"
        if `cvd'==100 local txt `txt' /*
            */ 2.5 .5 "Deuteranopia" 3.5 .5 "Protanopia" 4.5 .5 "Tritanopia"
        else local txt `txt' 2.5 .5 "Deuteranomaly (`cvd' percent)" /*
            */ 3.5 .5 "Protanomaly (`cvd' percent)" /*
            */ 4.5 .5 "Tritanomaly (`cvd' percent)"
        local gopts text(`txt', place(e))
        if `"`labels'"'=="" {
            forv i = 1/5 {
                local di `: di %9.1f el(r(delta),`i',1)'
                local labels `"`labels'"{&Delta}E {&ge} `di'" "'
            }
            local labels labels(`labels')
            local gopts `gopts' yscale(alt)
        }
        if `"`barwidth'"'=="" local barwidth barwidth(0.6)
    }
    mata: colorcheck_sort(`"`sort2'"')
    colorpalette, `title' `horizontal' `vertical' `lcolor' `lwidth' /*
        */ `barwidth' `nonumbers' `labels' gropts(`gopts' `gropts'): /*
        */ `p_norm' / `p_mono' / `p_deut'  / `p_prot' / `p_trit'
end

program _parse_sort
    syntax [, Normal Mono Deuter Prot Trit ]
    local sort `normal' `mono' `deuter' `prot' `trit'
    if `:list sizeof sort'>1 {
        di as err "sort(): too many keyword specified"
        exit 198
    }
    if "`sort'"=="" local sort normal
    c_local sort2 `sort'
end

version 14
mata:
mata set matastrict on

void colorcheck()
{
    real scalar      i, gs_p, cvd_p
    string scalar    metric, gs_space
    string rowvector cvd
    real matrix      delta, RGB, P
    real colvector   E
    class ColrSpace scalar S
    
    // settings
    gs_p     = strtoreal(st_local("mono"))
    cvd_p    = strtoreal(st_local("cvd"))
    gs_space = st_local("mono_method")
    metric   = st_local("metric")
    // import colors and prepare analysis
    S.colors(st_local("colors"))
    if (S.N()<=1) {
        stata(`"di as err "need at least two colors""')
        exit(error(498))
    }
    S.Intensify() // resolve intensity multipliers
    if (any(S.opacity():<.)) {
        displayas("txt")
        printf("(ignoring opacity settings)\n")
    }
    S.opacity(.)  // remove opacity
    RGB   = S.get("RGB1")
    P     = colorcheck_subsets(S.N())
    delta = J(5,3,.)
    // original
    E = S.delta(P, metric)
    delta[1,] = minmax(E), mean(E)
    st_local("p_norm", S.colors())
    // grayscale
    S.gray(gs_p, gs_space)
    E = S.delta(P, metric)
    delta[2,] = minmax(E), mean(E)
    st_local("p_mono", S.colors())
    // CVD
    cvd = ("deut","prot","trit")
    for (i=length(cvd);i;i--) {
        S.set(RGB, "RGB1")
        S.cvd(cvd_p, cvd[i])
        E = S.delta(P, metric)
        delta[2+i,] = minmax(E), mean(E)
        st_local("p_"+cvd[i], S.colors())
    }
    // return results
    st_matrix(st_local("delta"), delta)
}

real matrix colorcheck_subsets(real scalar n)
{
    real scalar i, j, k
    real matrix P

    P = J(comb(n, 2),2,.)
    k = 0
    for (i=1; i<=n; i++) {
        for (j=i+1;j<=n;j++) P[++k,] = (i,j)
    }
    return(P)
}

void colorcheck_sort(string scalar sort)
{
    real colvector p
    string scalar  xlab
    class ColrSpace scalar S
    
    if (sort=="") {
        st_local("p_norm", st_global("r(p_norm)"))
        st_local("p_mono", st_global("r(p_mono)"))
        st_local("p_deut", st_global("r(p_deut)"))
        st_local("p_prot", st_global("r(p_prot)"))
        st_local("p_trit", st_global("r(p_trit)"))
        return
    }
    if (sort=="mono") {
        S.colors(st_global("r(p_mono)"))
        p = order(S.get("RGB1")[,1],1)
    }
    else if (sort=="deuter") {
        S.colors(st_global("r(p_deut)"))
        p = order(S.get("HSL")[,1],1)
    }
    else if (sort=="prot") {
        S.colors(st_global("r(p_prot)"))
        p = order(S.get("HSL")[,1],1)
    }
    else if (sort=="trit") {
        S.colors(st_global("r(p_trit)"))
        p = order(S.get("HSL")[,1],1)
    }
    else if (sort=="normal") {
        S.colors(st_global("r(p_norm)"))
        p = order(S.get("HSL")[,1],1)
    }
    st_local("p_norm", invtokens((`"""':+tokens(st_global("r(p_norm)")):+`"""')[p]))
    st_local("p_mono", invtokens((`"""':+tokens(st_global("r(p_mono)")):+`"""')[p]))
    st_local("p_deut", invtokens((`"""':+tokens(st_global("r(p_deut)")):+`"""')[p]))
    st_local("p_prot", invtokens((`"""':+tokens(st_global("r(p_prot)")):+`"""')[p]))
    st_local("p_trit", invtokens((`"""':+tokens(st_global("r(p_trit)")):+`"""')[p]))
    if (st_local("nonumbers")=="") {
        xlab = invtokens((strofreal(1::S.N()):+" ":+`"""':+strofreal(p):+`"""')')
        if (st_local("vertical")!="") st_local("gopts", st_local("gopts") +
            " ylabel(" + xlab+", nogrid notick angle(hor))")
        else st_local("gopts", st_local("gopts") + 
            " xlabel(" + xlab+", notick)")
        st_local("nonumbers", "nonumbers")
    }
}

end

exit
