*! version 1.0.1  19jul2021  Ben Jann

program hexplot
    version 13
    _parse comma spec 0 : 0
    syntax [, VERTical HORizontal odd even left right ///
        HEXagon HEXagon2(passthru) scatter scatter2(passthru) ///
        BCuts(passthru) XBCuts(passthru) YBCuts(passthru) * ]
    if `"`hexagon'`hexagon2'"'!="" {
        di as err "hexagon() not allowed"
        exit 198
    }
    if `"`scatter'`scatter2'"'!="" {
        di as err "scatter() not allowed"
        exit 198
    }
    if `"`bcuts'"'!="" {
        di as err "bcuts() not allowed"
        exit 198
    }
    if `"`xbcuts'"'!="" {
        di as err "xbcuts() not allowed"
        exit 198
    }
    if `"`ybcuts'"'!="" {
        di as err "ybcuts() not allowed"
        exit 198
    }
    heatplot `spec', hexagon ///
        hexagon(`vertical' `horizontal' `right' `left' `even' `odd') ///
        `options' 
end

