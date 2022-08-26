# TRACE and FISD

In this chapter, we dive into the US corporate bond market. Bond markets are far more diverse than stock markets, as they feature a broad set of issuers, ranging from corporations to governments and municipalities. Moreover, most issuers have multiple bonds outstanding simultaneously with potentially very different indentures. This market segment is exciting due to its size (roughly $10 trillion outstanding), heterogeneity of issuers (as opposed to government bonds), market structure (mostly over-the-counter trades), and data availability. We introduce how to use bond characteristics from FISD and trade reports from TRACE and provide code to download and clean TRACE in R. 

Many researchers study liquidity in the US corporate bond market [see, e.g., @bessembinder2006, @Edwards2007, and @Ohara2021, among many others]. We do not cover bond returns here, but you could compute them from TRACE data. Instead, we refer to studies on the topic such as @Bessembinder2008, @bai2019, and @kelly2020 and a survey by @Huang2021. Moreover, WRDS includes bond returns computed from TRACE data at a monthly frequency.

The current chapter relies on this set of packages. 

```r
library(tidyverse)
library(lubridate)
library(dbplyr)
library(RSQLite)
library(RPostgres)
library(devtools)
```

Compared to previous chapters, we load the `devtools` package [@devtools] to source code that we provided to the public via [gist.](https://docs.github.com/en/get-started/writing-on-github/editing-and-sharing-content-with-gists/creating-gists)

## Bond data from WRDS 

Both bond databases we need are available on [WRDS](https://wrds-www.wharton.upenn.edu/) to which we establish the `RPostgres`-connection described in the previous chapter. Additionally, we connect to our local `SQLite`-database, which we also introduced in the previous chapters.\index{WRDS}


```r
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("user"),
  password = Sys.getenv("password")
)

tidy_finance <- dbConnect(
  SQLite(), 
  "data/tidy_finance.sqlite", 
  extended_types = TRUE
)
```

## Mergent FISD

For research on US corporate bonds, the Mergent Fixed Income Securities Database (FISD) is the primary resource for bond characteristics.\index{Data!FISD} There is a [detailed manual](https://wrds-www.wharton.upenn.edu/documents/1364/FixedIncome_Securities_Master_Database_User_Guide_v4.pdf) on WRDS, so we only cover the necessary subjects here. FISD data comes in two main variants, namely, centered on issuers or issues. In either case, the most useful identifiers are [CUSIPs.](https://www.cusip.com/index.html) 9-digit CUSIPs identify securities issued by issuers. The issuers can be identified from the first six digits of a security CUSIP, which is also called 6-digit CUSIP. Both stocks and bonds have CUSIPs. This connection would, in principle, allow matching them easily, but due to changing issuer details, this approach only yields small coverage.

We use the issue-centered version of FISD to identify the subset of US corporate bonds that meet the standard criteria [@bessembinder2006]. The WRDS table `fisd_mergedissue` contains most of the information we need on a 9-digit CUSIP level. As mentioned in the introduction of this chapter, corporate bonds are very diverse, and details in the indenture vary significantly. We focus on common bonds that make up the majority of trading volume in this market without diverging too much in indentures. 

The following chunk connects to the data and selects the bond sample to remove certain bond types that are less commonly (see, e.g., @Dick2012, @Ohara2021, among many others).


```r
mergent <- tbl(wrds, 
               in_schema("fisd", "fisd_mergedissue")) |>
  filter(
    security_level == "SEN",   # senior bonds
    slob == "N",               # secured lease obligation
    is.na(security_pledge),    # unsecured bonds
    asset_backed == "N",       # not asset backed
    defeased == "N",           # not defeased
    bond_type %in% c("CDEB",   # US Corporate Debentures
                     "CMTN",   # US Corporate MTN (Medium Term Note)
                     "CMTZ",   # US Corporate MTN Zero
                     "CZ",     # US Corporate Zero,
                     "USBN"),  # US Corporate Bank Note
    pay_in_kind != "Y",        # not payable in kind
    yankee == "N",             # no foreign issuer
    canadian == "N",           # not Canadian 
    foreign_currency == "N",   # USD
    coupon_type %in% c("F",    # fixed coupon
                       "Z"),   # zero coupon
    is.na(fix_frequency), 
    coupon_change_indicator == "N", 
    interest_frequency %in% c("0", # per year
                              "1", 
                              "2", 
                              "4", 
                              "12"),
    rule_144a == "N",          # publicly traded
    private_placement == "N", 
    defaulted == "N",          # not defaulted
    is.na(filing_date),
    is.na(settlement),
    convertible == "N",        # not convertible
    is.na(exchange),
    putable == "N",            # not putable
    unit_deal == "N",          # not issued with another security
    exchangeable == "N",       # not exchangeable
    perpetual == "N",          # not perpetual
    preferred_security == "N")  |> # not preferred
  select(
    complete_cusip, maturity, 
    offering_amt, offering_date, 
    dated_date, first_interest_date, 
    interest_frequency, coupon, 
    last_interest_date, day_count_basis, 
    issue_id, issuer_id
    ) |> 
  collect()
```

We also pull issuer information from `fisd_mergedissuer` regarding the industry and country of the firm that issued a particular bond. Then, we filter to include only US-domiciled firms' bonds. We match the data by `issuer_id`.


```r
mergent_issuer <- tbl(wrds, in_schema("fisd", "fisd_mergedissuer")) |>  
  select(issuer_id, sic_code, country_domicile) |> 
  collect()

mergent <- mergent |> 
  inner_join(mergent_issuer, by = "issuer_id")  |> 
  filter(country_domicile == "USA") |> 
  select(-country_domicile)
```

Finally, we save the bond characteristics to our local database. This selection of bonds also constitutes the sample for which we will collect trade reports from TRACE below.


```r
mergent |> 
  dbWriteTable(conn = tidy_finance,
               name = "mergent",
               value = _,
               overwrite = TRUE)
```

The FISD database also contains other data. The issue-based file contains information on covenants, i.e., restrictions included in bond indentures to limit specific actions by firms [e.g., @handler2021]. Moreover, FISD also provides information on bond ratings. We do not need either here.

## TRACE

The Financial Industry Regulatory Authority (FINRA) provides the Trade Reporting and Compliance Engine (TRACE).\index{Data!TRACE} In TRACE, dealers that trade corporate bonds must report such trades individually. Hence, we observe trade messages in TRACE that contain information on the bond traded, the trade time, price, and volume. TRACE comes in two variants; standard and enhanced TRACE. We show how to download and clean enhanced TRACE as it contains uncapped volume, a crucial quantity missing in the standard distribution. Moreover, enhanced TRACE also provides information on the respective parties' roles and the direction of the trade report. These items become essential in cleaning the messages.

Why do we repeatedly talk about cleaning TRACE? Trade messages are submitted within a short time window after a trade is executed (less than 15 minutes). These messages can contain errors, and the reporters subsequently correct them or they cancel a trade altogether. The cleaning needs are described by @Dick2009 in detail, and @Dick2014 shows how to clean the enhanced TRACE data using SAS. We do not go into the cleaning steps here, since the code is lengthy and serves no educational purpose. However, downloading and cleaning enhanced TRACE data is straightforward with our setup.

We store the code for cleaning enhanced TRACE with R on the following Github [gist.](https://gist.github.com/patrick-weiss/3a05b3ab281563b2e94858451c2eb3a4) \index{gist} as a function. The appendix also contains the code for reference. We only need to source the code from the gist, which we can do with `source_gist()`. Alternatively, you can also go to the [gist](https://gist.github.com/patrick-weiss/3a05b3ab281563b2e94858451c2eb3a4), download it, and `source()` the respective R-file. The `clean_enhanced_trace()` function takes a vector of CUSIPs, a connection to WRDS explained in Chapter 3, and a start and end date, respectively. 


```r
source_gist("3a05b3ab281563b2e94858451c2eb3a4")
```

The TRACE database is considerably large. Therefore, we only download subsets of data at once. Specifying too many CUSIPs over a long time horizon will result in very long download times and a potential failure due to the size of the request to WRDS. The size limit depends on many parameters, and we cannot give you a guideline here. If we were working with the complete TRACE data for all CUSIPs above, splitting the data into 100 parts takes roughly two hours using our setup. For the applications in this book, we need data around the Paris Agreement in December 2015 and download the data in ten sets, which we defined below.


```r
mergent_cusips <- mergent |>
  pull(complete_cusip)

set.seed(123)
mergent_parts <- split(mergent_cusips, 
                       sample(1:10, 
                              length(mergent_cusips), 
                              replace = TRUE))
```

Finally, we run a loop in the same style as in Chapter 2 where we download daily returns from CRSP. For each of the CUSIP sets defined above, we call the cleaning function and save the resulting output. We add new data to the existing dataframe for batch two and all following batches.


```r
progress <- txtProgressBar(min = 0, 
                           max = length(mergent_parts), 
                           initial = 0, 
                           style = 3)
```

```
  |                                                                               |                                                                       |   0%
```

```r
for(j in 1:length(mergent_parts)) {
  trace_enhanced <- clean_enhanced_trace(
    cusips = mergent_parts[[j]],
    connection = wrds,
    start_date = ymd("2014-01-01"),
    end_date = ymd("2016-11-30")
  )
    
  trace_enhanced |> 
    dbWriteTable(conn = tidy_finance,
                 name = "trace_enhanced",
                 value = _,
                 overwrite = ifelse(j == 1, TRUE, FALSE), 
                 append = ifelse(j != 1, TRUE, FALSE))
  
  setTxtProgressBar(progress, j)
}
```

```
  |                                                                               |=======                                                                |  10%  |                                                                               |==============                                                         |  20%  |                                                                               |=====================                                                  |  30%  |                                                                               |============================                                           |  40%  |                                                                               |====================================                                   |  50%  |                                                                               |===========================================                            |  60%  |                                                                               |==================================================                     |  70%  |                                                                               |=========================================================              |  80%  |                                                                               |================================================================       |  90%  |                                                                               |=======================================================================| 100%
```

```r
close(progress)
```

# Summary statistics

Insights into bonds

Insights into trading activity

