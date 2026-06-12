# Clean Enhanced TRACE with Python

This appendix contains code to clean enhanced TRACE with Python. It is also available via the following GitHub [gist](https://gist.githubusercontent.com/patrick-weiss/86ddef6de978fbdfb22609a7840b5d8b). Hence, you could also source the file with the following chunk.

``` python
gist_url = (
  "https://gist.githubusercontent.com/patrick-weiss/"
  "86ddef6de978fbdfb22609a7840b5d8b/raw/"
  "8fbcc6c6f40f537cd3cd37368be4487d73569c6b/"
)

with httpimport.remote_repo(gist_url):
  from clean_enhanced_TRACE_python import clean_enhanced_trace
```

We need this function in [TRACE and FISD](../python/trace-and-fisd.llms.md) to download and clean enhanced TRACE trade messages following Dick-Nielsen ([2009](#ref-Dick2009)) and Dick-Nielsen ([2014](#ref-Dick2014)) for enhanced TRACE specifically. This code is based on the resources provided by the project [Open Source Bond Asset Pricing](https://openbondassetpricing.com/) and their related publication Dickerson et al. ([2023](#ref-Dickerson2023)). We encourage that you acknowledge their effort. Relatedly, WRDS provides SAS code to clean enhanced TRACE data.

The function takes a vector of CUSIPs (in `cusips`), a connection to WRDS (`connection`) explained in Chapter 3, and a start and end date (`start_date` and `end_date`, respectively). Specifying too many CUSIPs will result in very slow downloads and a potential failure due to the size of the request to WRDS. The dates should be within the coverage of TRACE itself, i.e., starting after 2002, and the dates should be supplied as a string indicating MM/DD/YYYY. The output of the function contains all valid trade messages for the selected CUSIPs over the specified period.

``` python
def clean_enhanced_trace(cusips,
                         connection,
                         start_date="'01/01/2002'",
                         end_date="'12/31/2023'"):
  """Clean enhanced TRACE data."""

  import polars as pl

  # Load main file
  trace_query = (
    "SELECT cusip_id, bond_sym_id, trd_exctn_dt, "
           "trd_exctn_tm, days_to_sttl_ct, lckd_in_ind, "
           "wis_fl, sale_cndtn_cd, msg_seq_nb, "
           "trc_st, trd_rpt_dt, trd_rpt_tm, "
           "entrd_vol_qt, rptd_pr, yld_pt, "
           "asof_cd, orig_msg_seq_nb, rpt_side_cd, "
           "cntra_mp_id, stlmnt_dt, spcl_trd_fl "
    "FROM trace.trace_enhanced "
   f"WHERE cusip_id IN {cusips} "
         f"AND trd_exctn_dt BETWEEN {start_date} AND {end_date}"
  )

  # Pin dtypes for columns that can be entirely null within a batch,
  # so that all batches share the same schema
  trace_all = (pl.read_database(
      query=trace_query,
      connection=connection,
      schema_overrides={
        "cusip_id": pl.String, "days_to_sttl_ct": pl.String,
        "yld_pt": pl.Float64, "asof_cd": pl.String,
        "orig_msg_seq_nb": pl.String, "spcl_trd_fl": pl.String,
        "lckd_in_ind": pl.String, "stlmnt_dt": pl.Date,
      },
    )
    .with_columns(pl.col(pl.Decimal).cast(pl.Float64))
  )

  # Post 2012-06-02
  ## Trades (trc_st = T) and correction (trc_st = R)
  trace_post_TR = (trace_all
    .filter(pl.col("trc_st").is_in(["T", "R"]))
    .filter(pl.col("trd_rpt_dt") >= pl.lit("2012-06-02").str.to_date())
  )

  # Cancellations (trc_st = X) and correction cancellations (trc_st = C)
  trace_post_XC = (trace_all
    .filter(pl.col("trc_st").is_in(["X", "C"]))
    .filter(pl.col("trd_rpt_dt") >= pl.lit("2012-06-02").str.to_date())
    .select(["cusip_id", "msg_seq_nb", "entrd_vol_qt",
             "rptd_pr", "rpt_side_cd", "cntra_mp_id",
             "trd_exctn_dt", "trd_exctn_tm"])
    .with_columns(drop=pl.lit(True))
  )

  ## Cleaning corrected and cancelled trades
  trace_post_TR = (trace_post_TR
    .join(trace_post_XC, on=["cusip_id", "msg_seq_nb", "entrd_vol_qt",
                             "rptd_pr", "rpt_side_cd", "cntra_mp_id",
                             "trd_exctn_dt", "trd_exctn_tm"], how="left")
    .filter(pl.col("drop").is_null())
    .drop("drop")
  )

  # Reversals (trc_st = Y)
  trace_post_Y = (trace_all
    .filter(pl.col("trc_st") == "Y")
    .filter(pl.col("trd_rpt_dt") >= pl.lit("2012-06-02").str.to_date())
    .select(["cusip_id", "orig_msg_seq_nb", "entrd_vol_qt",
             "rptd_pr", "rpt_side_cd", "cntra_mp_id",
             "trd_exctn_dt", "trd_exctn_tm"])
    .with_columns(drop=pl.lit(True))
    .rename({"orig_msg_seq_nb": "msg_seq_nb"})
  )

  # Clean reversals
  ## Match the orig_msg_seq_nb of Y-message to msg_seq_nb of main message
  trace_post = (trace_post_TR
    .join(trace_post_Y, on=["cusip_id", "msg_seq_nb", "entrd_vol_qt",
                            "rptd_pr", "rpt_side_cd", "cntra_mp_id",
                            "trd_exctn_dt", "trd_exctn_tm"], how="left")
    .filter(pl.col("drop").is_null())
    .drop("drop")
  )

  # Pre 06-02-12
  ## Trades (trc_st = T)
  trace_pre_T = (trace_all
    .filter(pl.col("trd_rpt_dt") < pl.lit("2012-06-02").str.to_date())
  )

  # Cancellations (trc_st = C)
  trace_pre_C = (trace_all
    .filter(pl.col("trc_st") == "C")
    .filter(pl.col("trd_rpt_dt") < pl.lit("2012-06-02").str.to_date())
    .select(["cusip_id", "orig_msg_seq_nb", "entrd_vol_qt",
             "rptd_pr", "rpt_side_cd", "cntra_mp_id",
             "trd_exctn_dt", "trd_exctn_tm"])
    .with_columns(drop=pl.lit(True))
    .rename({"orig_msg_seq_nb": "msg_seq_nb"})
  )

  # Remove cancellations from trades
  ## Match orig_msg_seq_nb of C-message to msg_seq_nb of main message
  trace_pre_T = (trace_pre_T
    .join(trace_pre_C, on=["cusip_id", "msg_seq_nb", "entrd_vol_qt",
                           "rptd_pr", "rpt_side_cd", "cntra_mp_id",
                           "trd_exctn_dt", "trd_exctn_tm"], how="left")
    .filter(pl.col("drop").is_null())
    .drop("drop")
  )

  # Corrections (trc_st = W)
  trace_pre_W = (trace_all
    .filter(pl.col("trc_st") == "W")
    .filter(pl.col("trd_rpt_dt") < pl.lit("2012-06-02").str.to_date())
  )

  # Implement corrections in a loop
  ## Correction control
  correction_control = len(trace_pre_W)
  correction_control_last = len(trace_pre_W)

  ## Correction loop
  while (correction_control > 0):
    # Create placeholder
    ## Only identifying columns of trace_pre_T (for joins)
    placeholder_trace_pre_T = (trace_pre_T
      .select(["cusip_id", "trd_exctn_dt", "msg_seq_nb"])
      .rename({"msg_seq_nb": "orig_msg_seq_nb"})
      .with_columns(matched_T=pl.lit(True))
    )

    # Corrections that correct some msg
    trace_pre_W_correcting = (trace_pre_W
      .join(placeholder_trace_pre_T,
            on=["cusip_id", "trd_exctn_dt", "orig_msg_seq_nb"], how="left")
      .filter(pl.col("matched_T") == True)
      .drop("matched_T")
    )

    # Corrections that do not correct some msg
    trace_pre_W = (trace_pre_W
      .join(placeholder_trace_pre_T,
            on=["cusip_id", "trd_exctn_dt", "orig_msg_seq_nb"], how="left")
      .filter(pl.col("matched_T").is_null())
      .drop("matched_T")
    )

    # Create placeholder
    ## Only identifying columns of trace_pre_W_correcting (for anti-joins)
    placeholder_trace_pre_W_correcting = (trace_pre_W_correcting
      .select(["cusip_id", "trd_exctn_dt", "orig_msg_seq_nb"])
      .rename({"orig_msg_seq_nb": "msg_seq_nb"})
      .with_columns(corrected=pl.lit(True))
    )

    # Delete msgs that are corrected
    trace_pre_T = (trace_pre_T
      .join(placeholder_trace_pre_W_correcting,
            on=["cusip_id", "trd_exctn_dt", "msg_seq_nb"], how="left")
      .filter(pl.col("corrected").is_null())
      .drop("corrected")
    )

    # Add correction msgs
    trace_pre_T = pl.concat([trace_pre_T, trace_pre_W_correcting])

    # Escape if no corrections remain or they cannot be matched
    correction_control = len(trace_pre_W)

    if correction_control == correction_control_last:
      break
    else:
      correction_control_last = len(trace_pre_W)
      continue

  # Reversals (asof_cd = R)
  ## Record reversals
  trace_pre_R = (trace_pre_T
    .filter(pl.col("asof_cd") == "R")
    .sort(["cusip_id", "trd_exctn_dt",
           "trd_exctn_tm", "trd_rpt_dt", "trd_rpt_tm"])
  )

  ## Prepare final data
  trace_pre = (trace_pre_T
    .filter(
      pl.col("asof_cd").is_null() | ~pl.col("asof_cd").is_in(["R", "X", "D"])
    )
    .sort(["cusip_id", "trd_exctn_dt",
           "trd_exctn_tm", "trd_rpt_dt", "trd_rpt_tm"])
  )

  ## Add grouped row numbers
  trace_pre_R = (trace_pre_R
    .with_columns(
      seq=pl.int_range(pl.len()).over(
        ["cusip_id", "trd_exctn_dt", "entrd_vol_qt",
         "rptd_pr", "rpt_side_cd", "cntra_mp_id"]
      )
    )
  )

  trace_pre = (trace_pre
    .with_columns(
      seq=pl.int_range(pl.len()).over(
        ["cusip_id", "trd_exctn_dt", "entrd_vol_qt",
         "rptd_pr", "rpt_side_cd", "cntra_mp_id"]
      )
    )
  )

  ## Select columns for reversal cleaning
  trace_pre_R = (trace_pre_R
    .select(["cusip_id", "trd_exctn_dt", "entrd_vol_qt",
             "rptd_pr", "rpt_side_cd", "cntra_mp_id", "seq"])
    .with_columns(reversal=pl.lit(True))
  )

  ## Remove reversals and the reversed trade
  trace_pre = (trace_pre
    .join(trace_pre_R, on=["cusip_id", "trd_exctn_dt", "entrd_vol_qt",
                           "rptd_pr", "rpt_side_cd", "cntra_mp_id", "seq"],
          how="left")
    .filter(pl.col("reversal").is_null())
    .drop(["reversal", "seq"])
  )

  # Combine pre and post trades
  trace_clean = pl.concat([trace_pre, trace_post], how="diagonal")

  # Keep agency sells and unmatched agency buys
  trace_agency_sells = (trace_clean
    .filter((pl.col("cntra_mp_id") == "D") & (pl.col("rpt_side_cd") == "S"))
  )

  # Placeholder for trace_agency_sells with relevant columns
  placeholder_trace_agency_sells = (trace_agency_sells
    .select(["cusip_id", "trd_exctn_dt",
             "entrd_vol_qt", "rptd_pr"])
    .with_columns(matched=pl.lit(True))
  )

  # Agency buys that are unmatched
  trace_agency_buys_filtered = (trace_clean
    .filter((pl.col("cntra_mp_id") == "D") & (pl.col("rpt_side_cd") == "B"))
    .join(placeholder_trace_agency_sells,
          on=["cusip_id", "trd_exctn_dt", "entrd_vol_qt", "rptd_pr"],
          how="left")
    .filter(pl.col("matched").is_null())
    .drop("matched")
  )

  # Non-agency
  trace_nonagency = (trace_clean
    .filter(pl.col("cntra_mp_id") == "C")
  )

  # Agency cleaned
  trace_clean = pl.concat([trace_nonagency,
                           trace_agency_sells,
                           trace_agency_buys_filtered])

  # Additional Filters
  trace_add_filters = (trace_clean
    .with_columns(
      days_to_sttl_ct2=(
        pl.col("stlmnt_dt") - pl.col("trd_exctn_dt")
      ).dt.total_days()
    )
    .with_columns(
      days_to_sttl_ct=pl.col("days_to_sttl_ct").cast(pl.Float64, strict=False)
    )
    .filter(pl.col("days_to_sttl_ct").is_null() | (pl.col("days_to_sttl_ct") <= 7))
    .filter(pl.col("days_to_sttl_ct2").is_null() | (pl.col("days_to_sttl_ct2") <= 7))
    .filter(pl.col("wis_fl") == "N")
    .filter(pl.col("spcl_trd_fl").is_null() | (pl.col("spcl_trd_fl") == ""))
    .filter(pl.col("asof_cd").is_null() | (pl.col("asof_cd") == ""))
  )

  # Only keep necessary columns
  trace_final = (trace_add_filters
    .sort(["cusip_id", "trd_exctn_dt", "trd_exctn_tm"])
    .select(["cusip_id", "trd_exctn_dt", "trd_exctn_tm", "rptd_pr",
             "entrd_vol_qt", "yld_pt", "rpt_side_cd", "cntra_mp_id"])
  )

  return trace_final
```

## References

Dickerson, Alexander, Philippe Mueller, and Cesare Robotti. 2023. “Priced Risk in Corporate Bonds.” *Journal of Financial Economics* 150 (2): 103707. <https://doi.org/10.1016/j.jfineco.2023.103707>.

Dick-Nielsen, Jens. 2009. “Liquidity biases in TRACE.” *The Journal of Fixed Income* 19 (2): 43–55. <https://doi.org/10.3905/jfi.2009.19.2.043>.

Dick-Nielsen, Jens. 2014. “How to clean enhanced TRACE data.” *Working Paper*. <https://ssrn.com/abstract=2337908>.
