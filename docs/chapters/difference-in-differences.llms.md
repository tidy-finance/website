# Difference in Differences

In this chapter, we illustrate the concept of *difference in differences* (DiD) estimators by evaluating the effects of climate change regulation on the pricing of bonds across firms. DiD estimators are typically used to recover the treatment effects of natural or quasi-natural experiments that trigger sharp changes in the environment of a specific group. Instead of looking at differences in just one group (e.g., the effect in the treated group), DiD investigates the treatment effects by looking at the difference between differences in two groups. Such experiments are usually exploited to address endogeneity concerns (e.g., [Roberts and Whited 2013](#ref-RobertsWhited2013)). The identifying assumption is that the outcome variable would change equally in both groups without the treatment. This assumption is also often referred to as the assumption of parallel trends. Moreover, we would ideally also want a random assignment to the treatment and control groups. Due to lobbying or other activities, this randomness is often violated in (financial) economics.

In the context of our setting, we investigate the impact of the Paris Agreement (PA), signed on December 12, 2015, on the bond yields of polluting firms. We first estimate the treatment effect of the agreement using panel regression techniques that we discuss in [Fixed Effects and Clustered Standard Errors](../chapters/fixed-effects-and-clustered-standard-errors.llms.md). We then present two methods to illustrate the treatment effect over time graphically. Although we demonstrate that the treatment effect of the agreement is anticipated by bond market participants well in advance, the techniques we present below can also be applied to many other settings.

The approach we use here replicates the results of Seltzer et al. ([2022](#ref-Seltzer2022)) partly. Specifically, we borrow their industry definitions for grouping firms into green and brown types. Overall, the literature on environmental, social, and governance (ESG) effects in corporate bond markets is already large but continues to grow (for recent examples, see, e.g., Halling et al. ([2021](#ref-Halling2021)), Handler et al. ([2022](#ref-Handler2022)), Huynh and Xia ([2021](#ref-Huynh2021)), among many others).

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(nanoparquet)
library(fixest)
library(broom)

theme_set(theme_minimal())
```

## Python

``` python
import polars as pl
import datetime as dt
import pyfixest as pf

from plotnine import *
from scipy.stats import norm

theme_set(theme_minimal())
```

Compared to previous chapters, we introduce the `scipy.stats` module from `scipy` ([Virtanen et al. 2020](#ref-scipy)) for simple retrieval of quantiles of the standard normal distribution.

## Data Preparation

We use TRACE and Mergent FISD as data sources from our Parquet files introduced in [Accessing and Managing Financial Data](../chapters/accessing-and-managing-financial-data.llms.md) and [TRACE and FISD](../chapters/trace-and-fisd.llms.md).

## R

``` r
fisd <- read_parquet("data/fisd.parquet") |>
  select(complete_cusip, maturity, offering_amt, sic_code) |>
  drop_na()

trace_enhanced <- list.files(
    "data/trace_enhanced", pattern = "\\.parquet$",
    recursive = TRUE, full.names = TRUE
  ) |>
  map(read_parquet) |>
  list_rbind() |>
  select(cusip_id, trd_exctn_dt, rptd_pr, entrd_vol_qt, yld_pt) |>
  drop_na()
```

## Python

``` python
fisd = (pl.read_parquet("data/fisd.parquet")
    .select(["complete_cusip", "maturity", "offering_amt", "sic_code"])
    .drop_nulls()
)

trace_enhanced = (pl.scan_parquet("data/trace_enhanced", hive_partitioning=True)
    .select(["cusip_id", "trd_exctn_dt", "rptd_pr", "entrd_vol_qt", "yld_pt"])
    .collect()
    .drop_nulls()
)
```

We start our analysis by preparing the sample of bonds. We only consider bonds with a time to maturity of more than one year to the signing of the PA, so that we have sufficient data to analyze the yield behavior after the treatment date. This restriction also excludes all bonds issued after the agreement. We also consider only the first two digits of the SIC industry code to identify the polluting industries (in line with [Seltzer et al. 2022](#ref-Seltzer2022)).

## R

``` r
treatment_date <- ymd("2015-12-12")

polluting_industries <- c(
  49,
  13,
  45,
  29,
  28,
  33,
  40,
  20,
  26,
  42,
  10,
  53,
  32,
  99,
  37
)

bonds <- fisd |>
  filter(offering_amt > 0) |>
  mutate(
    time_to_maturity = as.numeric(maturity - treatment_date) / 365,
    sic_code = as.integer(substr(sic_code, 1, 2)),
    log_offering_amt = log(offering_amt)
  ) |>
  filter(time_to_maturity >= 1) |>
  select(
    cusip_id = complete_cusip,
    time_to_maturity,
    log_offering_amt,
    sic_code
  ) |>
  mutate(polluter = sic_code %in% polluting_industries)
```

## Python

``` python
treatment_date = dt.datetime(2015, 12, 12)
treatment_month = dt.datetime(2015, 12, 1)
polluting_industries = [
    49, 13, 45, 29, 28, 33, 40, 20, 26, 42, 10, 53, 32, 99, 37
]

bonds = (fisd
    .filter(pl.col("offering_amt") > 0)
    .with_columns(
        time_to_maturity=(pl.col("maturity") - treatment_date).dt.total_days() / 365,
        sic_code=pl.col("sic_code").cast(pl.Utf8).str.slice(0, 2).cast(pl.Int64),
        log_offering_amt=pl.col("offering_amt").log()
    )
    .filter(pl.col("time_to_maturity") >= 1)
    .rename({"complete_cusip": "cusip_id"})
    .select(["cusip_id", "time_to_maturity", "log_offering_amt", "sic_code"])
    .with_columns(polluter=pl.col("sic_code").is_in(polluting_industries))
)
```

Next, we aggregate the individual transactions as reported in TRACE to a monthly panel of bond yields. We consider bond yields for a bond’s last trading day in a month. Therefore, we first aggregate bond data to daily frequency and apply common restrictions from the literature (see, e.g., [Bessembinder et al. 2008](#ref-Bessembinder2008)). We weigh each transaction by volume to reflect a trade’s relative importance and avoid emphasizing small trades. Moreover, we only consider transactions with reported prices `rptd_pr` larger than 25 (to exclude bonds that are close to default) and only bond-day observations with more than five trades on a corresponding day (to exclude prices based on too few, potentially non-representative transactions).

## R

``` r
trace_aggregated <- trace_enhanced |>
  filter(rptd_pr > 25) |>
  group_by(cusip_id, trd_exctn_dt) |>
  summarize(
    avg_yield = weighted.mean(yld_pt, entrd_vol_qt * rptd_pr),
    trades = n(),
    .groups = "drop"
  ) |>
  drop_na(avg_yield) |>
  filter(trades >= 5) |>
  mutate(date = floor_date(trd_exctn_dt, "month")) |>
  group_by(cusip_id, date) |>
  slice_max(trd_exctn_dt) |>
  ungroup() |>
  select(cusip_id, date, avg_yield)
```

## Python

``` python
trace_enhanced = (trace_enhanced
    .filter(pl.col("rptd_pr") > 25)
    .with_columns(weight=pl.col("entrd_vol_qt")*pl.col("rptd_pr"))
    .with_columns(weighted_yield=pl.col("weight")*pl.col("yld_pt"))
)

trace_aggregated = (trace_enhanced
    .group_by(["cusip_id", "trd_exctn_dt"])
    .agg(
        weighted_yield_sum=pl.col("weighted_yield").sum(),
        weight_sum=pl.col("weight").sum(),
        trades=pl.col("rptd_pr").count()
    )
    .with_columns(avg_yield=pl.col("weighted_yield_sum")/pl.col("weight_sum"))
    .drop_nulls("avg_yield")
    .filter(pl.col("trades") >= 5)
    .with_columns(date=pl.col("trd_exctn_dt").dt.truncate("1mo"))
)

trace_aggregated = (trace_aggregated
    .sort("trd_exctn_dt")
    .group_by(["cusip_id", "date"])
    .last()
    .select(["cusip_id", "date", "avg_yield"])
)
```

By combining the bond-specific information from Mergent FISD for our bond sample with the aggregated TRACE data, we arrive at the main sample for our analysis.

## R

``` r
bonds_panel <- bonds |>
  inner_join(trace_aggregated, join_by(cusip_id), multiple = "all") |>
  drop_na()
```

## Python

``` python
bonds_panel = (bonds
    .join(trace_aggregated, how="inner", on="cusip_id")
    .drop_nulls()
)
```

Before we can run the first regression, we need to define the `treated` indicator,[^1] which is the product of the `post_period` (i.e., all months after the signing of the PA) and the `polluter` indicator defined above.

## R

``` r
bonds_panel <- bonds_panel |>
  mutate(post_period = date >= floor_date(treatment_date, "months")) |>
  mutate(treated = polluter & post_period)
```

## Python

``` python
bonds_panel = (bonds_panel
    .with_columns(
        post_period=(pl.col("date") >= treatment_month)
    )
    .with_columns(treated=pl.col("polluter") & pl.col("post_period"))
)
```

As usual, we tabulate summary statistics of the variables that enter the regression to check the validity of our variable definitions.

## R

``` r
bonds_panel |>
  pivot_longer(
    cols = c(avg_yield, time_to_maturity, log_offering_amt),
    names_to = "measure"
  ) |>
  group_by(measure) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    q05 = quantile(value, 0.05),
    q50 = quantile(value, 0.50),
    q95 = quantile(value, 0.95),
    max = max(value),
    n = n(),
    .groups = "drop"
  )
```

    # A tibble: 3 × 9
      measure           mean    sd    min   q05   q50   q95   max      n
      <chr>            <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <int>
    1 avg_yield         4.08 4.21  0.0595  1.27  3.38  8.10 128.  127530
    2 log_offering_amt 13.3  0.823 4.64   12.2  13.2  14.5   16.5 127530
    3 time_to_maturity  8.55 8.41  1.01    1.50  5.81 27.4  101.  127530

## Python

``` python
bonds_panel_summary = (bonds_panel
    .unpivot(
        on=["avg_yield", "time_to_maturity", "log_offering_amt"],
        variable_name="measure"
    )
    .group_by("measure")
    .agg(
        count=pl.len(),
        mean=pl.col("value").mean(),
        std=pl.col("value").std(),
        min=pl.col("value").min(),
        q05=pl.col("value").quantile(0.05, interpolation="linear"),
        median=pl.col("value").median(),
        q95=pl.col("value").quantile(0.95, interpolation="linear"),
        max=pl.col("value").max(),
    )
    .sort("measure")
    .with_columns(pl.col(pl.Float64).round(2))
)
bonds_panel_summary
```

shape: (3, 9)

| measure            | count  | mean  | std  | min  | q05   | median | q95   | max    |
|--------------------|--------|-------|------|------|-------|--------|-------|--------|
| str                | u32    | f64   | f64  | f64  | f64   | f64    | f64   | f64    |
| "avg_yield"        | 127530 | 4.08  | 4.21 | 0.06 | 1.27  | 3.38   | 8.1   | 127.97 |
| "log_offering_amt" | 127530 | 13.27 | 0.82 | 4.64 | 12.21 | 13.22  | 14.51 | 16.52  |
| "time_to_maturity" | 127530 | 8.55  | 8.41 | 1.01 | 1.5   | 5.81   | 27.41 | 100.7  |

## Panel Regressions

The PA is a legally binding international treaty on climate change. It was adopted by 196 parties at COP 21 in Paris on December 12, 2015 and entered into force on November 4, 2016. The PA obliges developed countries to support efforts to build clean, climate-resilient futures. One may thus hypothesize that adopting climate-related policies may affect financial markets. To measure the magnitude of this effect, we first run an ordinary least square (OLS) regression without fixed effects where we include the `treated`, `post_period`, and `polluter` dummies, as well as the bond-specific characteristics `log_offering_amt` and `time_to_maturity`. This simple model assumes that there are essentially two periods (before and after the PA) and two groups (polluters and non-polluters). Nonetheless, it should indicate whether polluters have higher yields following the PA compared to non-polluters.

The second model follows the typical DiD regression approach by including individual (`cusip_id`) and time (`date`) fixed effects. In this model, we do not include any other variables from the simple model because the fixed effects subsume them, and we observe the coefficient of our main variable of interest: `treated`.

## R

``` r
model_without_fe <- feols(
  avg_yield ~ treated +
    post_period +
    polluter +
    log_offering_amt +
    time_to_maturity,
  vcov = "iid",
  data = bonds_panel
)

model_with_fe <- feols(
  avg_yield ~ treated | cusip_id + date,
  vcov = "iid",
  data = bonds_panel
)
```

    NOTE: 351/0 fixed-effect singletons were removed (351 observations).

``` r
etable(
  model_without_fe,
  model_with_fe,
  coefstat = "tstat",
  digits = 3,
  digits.stats = 3
)
```

                      model_without_fe   model_with_fe
    Dependent Var.:          avg_yield       avg_yield
                                                      
    Constant            10.7*** (57.0)                
    treatedTRUE        0.462*** (9.31) 0.983*** (29.5)
    post_periodTRUE  -0.174*** (-5.92)                
    polluterTRUE       0.481*** (15.3)                
    log_offering_amt -0.551*** (-39.0)                
    time_to_maturity   0.058*** (41.6)                
    Fixed-Effects:   ----------------- ---------------
    cusip_id                        No             Yes
    date                            No             Yes
    ________________ _________________ _______________
    VCOV type                      IID             IID
    Observations               127,530         127,179
    R2                           0.032           0.647
    Within R2                       --           0.007
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Python

``` python
model_without_fe = pf.feols(
    "avg_yield ~ treated + post_period + polluter + log_offering_amt + time_to_maturity",
    vcov = "iid",
    data = bonds_panel
)

model_with_fe = pf.feols(
    "avg_yield ~ treated | cusip_id + date",
    vcov = "iid",
    data = bonds_panel
)

pf.etable([model_without_fe, model_with_fe], coef_fmt = "b (t)")
```

    GT(_tbl_data=  __index_level_0__ __index_level_1__                 0               1
    0              coef           treated     0.462 (9.308)  0.983 (29.534)
    1              coef       post_period   -0.174 (-5.923)                
    2              coef          polluter    0.481 (15.280)                
    3              coef  log_offering_amt  -0.551 (-38.976)                
    4              coef  time_to_maturity    0.058 (41.556)                
    5              coef         Intercept   10.734 (57.015)                
    6                fe              date                 -               x
    7                fe         cusip_id                  -               x
    8             stats      Observations           127,530         127,179
    9             stats                R²             0.032           0.647, _body=<great_tables._gt_data.Body object at 0x000002D31AA1FB50>, _boxhead=Boxhead([ColInfo(var='__index_level_0__', type=<ColInfoTypeEnum.row_group: 3>, column_label='__index_level_0__', column_align='center', column_width=None), ColInfo(var='__index_level_1__', type=<ColInfoTypeEnum.stub: 2>, column_label='__index_level_1__', column_align='center', column_width=None), ColInfo(var='0', type=<ColInfoTypeEnum.default: 1>, column_label='(1)', column_align='center', column_width=None), ColInfo(var='1', type=<ColInfoTypeEnum.default: 1>, column_label='(2)', column_align='center', column_width=None)]), _stub=<great_tables._gt_data.Stub object at 0x000002D31AA09190>, _spanners=Spanners([SpannerInfo(spanner_id='avg_yield', spanner_level=1, spanner_label='avg_yield', spanner_units=None, spanner_pattern=None, vars=['0', '1'], built=None)]), _heading=Heading(title=None, subtitle=None, preheader=None), _stubhead=None, _summary_rows=<great_tables._gt_data.SummaryRows object at 0x000002D31AA1F290>, _summary_rows_grand=<great_tables._gt_data.SummaryRows object at 0x000002D31AA83A10>, _source_notes=['Format of coefficient cell: Coefficient (t-stats)'], _footnotes=[], _styles=[], _locale=<great_tables._gt_data.Locale object at 0x000002D31AA093D0>, _formats=[], _substitutions=[], _col_merge=[], _transforms=[], _options=Options(table_id=OptionsInfo(scss=False, category='table', type='value', value=None), table_caption=OptionsInfo(scss=False, category='table', type='value', value=None), table_width=OptionsInfo(scss=True, category='table', type='px', value='auto'), table_layout=OptionsInfo(scss=True, category='table', type='value', value='fixed'), table_margin_left=OptionsInfo(scss=True, category='table', type='px', value='auto'), table_margin_right=OptionsInfo(scss=True, category='table', type='px', value='auto'), table_background_color=OptionsInfo(scss=True, category='table', type='value', value='#FFFFFF'), table_additional_css=OptionsInfo(scss=False, category='table', type='values', value=[]), table_font_names=OptionsInfo(scss=False, category='table', type='values', value=['-apple-system', 'BlinkMacSystemFont', 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Helvetica Neue', 'Fira Sans', 'Droid Sans', 'Arial', 'sans-serif']), table_font_size=OptionsInfo(scss=True, category='table', type='px', value='16px'), table_font_weight=OptionsInfo(scss=True, category='table', type='value', value='normal'), table_font_style=OptionsInfo(scss=True, category='table', type='value', value='normal'), table_font_color=OptionsInfo(scss=True, category='table', type='value', value='#333333'), table_font_color_light=OptionsInfo(scss=True, category='table', type='value', value='#FFFFFF'), table_border_top_include=OptionsInfo(scss=False, category='table', type='boolean', value=True), table_border_top_style=OptionsInfo(scss=True, category='table', type='value', value='solid'), table_border_top_width=OptionsInfo(scss=True, category='table', type='px', value='2px'), table_border_top_color=OptionsInfo(scss=True, category='table', type='value', value='#A8A8A8'), table_border_right_style=OptionsInfo(scss=True, category='table', type='value', value='none'), table_border_right_width=OptionsInfo(scss=True, category='table', type='px', value='2px'), table_border_right_color=OptionsInfo(scss=True, category='table', type='value', value='#D3D3D3'), table_border_bottom_include=OptionsInfo(scss=False, category='table', type='boolean', value=True), table_border_bottom_style=OptionsInfo(scss=True, category='table', type='value', value='hidden'), table_border_bottom_width=OptionsInfo(scss=True, category='table', type='px', value='2px'), table_border_bottom_color=OptionsInfo(scss=True, category='table', type='value', value='#A8A8A8'), table_border_left_style=OptionsInfo(scss=True, category='table', type='value', value='none'), table_border_left_width=OptionsInfo(scss=True, category='table', type='px', value='2px'), table_border_left_color=OptionsInfo(scss=True, category='table', type='value', value='#D3D3D3'), heading_background_color=OptionsInfo(scss=True, category='heading', type='value', value=None), heading_align=OptionsInfo(scss=True, category='heading', type='value', value='center'), heading_title_font_size=OptionsInfo(scss=True, category='heading', type='px', value='16px'), heading_title_font_weight=OptionsInfo(scss=True, category='heading', type='value', value='initial'), heading_subtitle_font_size=OptionsInfo(scss=True, category='heading', type='px', value='85%'), heading_subtitle_font_weight=OptionsInfo(scss=True, category='heading', type='value', value='initial'), heading_padding=OptionsInfo(scss=True, category='heading', type='px', value='6px'), heading_padding_horizontal=OptionsInfo(scss=True, category='heading', type='px', value='5px'), heading_border_bottom_style=OptionsInfo(scss=True, category='heading', type='value', value='solid'), heading_border_bottom_width=OptionsInfo(scss=True, category='heading', type='px', value='2px'), heading_border_bottom_color=OptionsInfo(scss=True, category='heading', type='value', value='#D3D3D3'), heading_border_lr_style=OptionsInfo(scss=True, category='heading', type='value', value='none'), heading_border_lr_width=OptionsInfo(scss=True, category='heading', type='px', value='1px'), heading_border_lr_color=OptionsInfo(scss=True, category='heading', type='value', value='#D3D3D3'), column_labels_background_color=OptionsInfo(scss=True, category='column_labels', type='value', value=None), column_labels_font_size=OptionsInfo(scss=True, category='column_labels', type='px', value='16px'), column_labels_font_weight=OptionsInfo(scss=True, category='column_labels', type='value', value='normal'), column_labels_text_transform=OptionsInfo(scss=True, category='column_labels', type='value', value='inherit'), column_labels_padding=OptionsInfo(scss=True, category='column_labels', type='px', value='2px'), column_labels_padding_horizontal=OptionsInfo(scss=True, category='column_labels', type='px', value='5px'), column_labels_vlines_style=OptionsInfo(scss=True, category='table_body', type='value', value='none'), column_labels_vlines_width=OptionsInfo(scss=True, category='table_body', type='px', value='0px'), column_labels_vlines_color=OptionsInfo(scss=True, category='table_body', type='value', value='white'), column_labels_border_top_style=OptionsInfo(scss=True, category='column_labels', type='value', value='solid'), column_labels_border_top_width=OptionsInfo(scss=True, category='column_labels', type='px', value='2px'), column_labels_border_top_color=OptionsInfo(scss=True, category='column_labels', type='value', value='black'), column_labels_border_bottom_style=OptionsInfo(scss=True, category='column_labels', type='value', value='solid'), column_labels_border_bottom_width=OptionsInfo(scss=True, category='column_labels', type='px', value='0.25px'), column_labels_border_bottom_color=OptionsInfo(scss=True, category='column_labels', type='value', value='black'), column_labels_border_lr_style=OptionsInfo(scss=True, category='column_labels', type='value', value='none'), column_labels_border_lr_width=OptionsInfo(scss=True, category='column_labels', type='px', value='1px'), column_labels_border_lr_color=OptionsInfo(scss=True, category='column_labels', type='value', value='#D3D3D3'), column_labels_hidden=OptionsInfo(scss=False, category='column_labels', type='boolean', value=False), row_group_background_color=OptionsInfo(scss=True, category='row_group', type='value', value=None), row_group_font_size=OptionsInfo(scss=True, category='row_group', type='px', value='0px'), row_group_font_weight=OptionsInfo(scss=True, category='row_group', type='value', value='initial'), row_group_text_transform=OptionsInfo(scss=True, category='row_group', type='value', value='inherit'), row_group_padding=OptionsInfo(scss=True, category='row_group', type='px', value='0px'), row_group_padding_horizontal=OptionsInfo(scss=True, category='row_group', type='px', value='5px'), row_group_border_top_style=OptionsInfo(scss=True, category='row_group', type='value', value='solid'), row_group_border_top_width=OptionsInfo(scss=True, category='row_group', type='px', value='0.25px'), row_group_border_top_color=OptionsInfo(scss=True, category='row_group', type='value', value='black'), row_group_border_right_style=OptionsInfo(scss=True, category='row_group', type='value', value='none'), row_group_border_right_width=OptionsInfo(scss=True, category='row_group', type='px', value='1px'), row_group_border_right_color=OptionsInfo(scss=True, category='row_group', type='value', value='white'), row_group_border_bottom_style=OptionsInfo(scss=True, category='row_group', type='value', value='solid'), row_group_border_bottom_width=OptionsInfo(scss=True, category='row_group', type='px', value='0.25px'), row_group_border_bottom_color=OptionsInfo(scss=True, category='row_group', type='value', value='black'), row_group_border_left_style=OptionsInfo(scss=True, category='row_group', type='value', value='none'), row_group_border_left_width=OptionsInfo(scss=True, category='row_group', type='px', value='1px'), row_group_border_left_color=OptionsInfo(scss=True, category='row_group', type='value', value='white'), row_group_as_column=OptionsInfo(scss=False, category='row_group', type='boolean', value=False), table_body_hlines_style=OptionsInfo(scss=True, category='table_body', type='value', value='none'), table_body_hlines_width=OptionsInfo(scss=True, category='table_body', type='px', value='1px'), table_body_hlines_color=OptionsInfo(scss=True, category='table_body', type='value', value='#D3D3D3'), table_body_vlines_style=OptionsInfo(scss=True, category='table_body', type='value', value='none'), table_body_vlines_width=OptionsInfo(scss=True, category='table_body', type='px', value='0px'), table_body_vlines_color=OptionsInfo(scss=True, category='table_body', type='value', value='white'), table_body_border_top_style=OptionsInfo(scss=True, category='table_body', type='value', value='solid'), table_body_border_top_width=OptionsInfo(scss=True, category='table_body', type='px', value='0px'), table_body_border_top_color=OptionsInfo(scss=True, category='table_body', type='value', value='black'), table_body_border_bottom_style=OptionsInfo(scss=True, category='table_body', type='value', value='solid'), table_body_border_bottom_width=OptionsInfo(scss=True, category='table_body', type='px', value='2px'), table_body_border_bottom_color=OptionsInfo(scss=True, category='table_body', type='value', value='black'), data_row_padding=OptionsInfo(scss=True, category='data_row', type='px', value='2px'), data_row_padding_horizontal=OptionsInfo(scss=True, category='data_row', type='px', value='5px'), stub_background_color=OptionsInfo(scss=True, category='stub', type='value', value=None), stub_font_size=OptionsInfo(scss=True, category='stub', type='px', value='16px'), stub_font_weight=OptionsInfo(scss=True, category='stub', type='value', value='initial'), stub_text_transform=OptionsInfo(scss=True, category='stub', type='value', value='inherit'), stub_border_style=OptionsInfo(scss=True, category='stub', type='value', value='hidden'), stub_border_width=OptionsInfo(scss=True, category='stub', type='px', value='2px'), stub_border_color=OptionsInfo(scss=True, category='stub', type='value', value='#D3D3D3'), stub_row_group_background_color=OptionsInfo(scss=True, category='stub', type='value', value=None), stub_row_group_font_size=OptionsInfo(scss=True, category='stub', type='px', value='100%'), stub_row_group_font_weight=OptionsInfo(scss=True, category='stub', type='value', value='initial'), stub_row_group_text_transform=OptionsInfo(scss=True, category='stub', type='value', value='inherit'), stub_row_group_border_style=OptionsInfo(scss=True, category='stub', type='value', value='solid'), stub_row_group_border_width=OptionsInfo(scss=True, category='stub', type='px', value='2px'), stub_row_group_border_color=OptionsInfo(scss=True, category='stub', type='value', value='#D3D3D3'), summary_row_padding=OptionsInfo(scss=True, category='summary_row', type='px', value='8px'), summary_row_padding_horizontal=OptionsInfo(scss=True, category='summary_row', type='px', value='5px'), summary_row_background_color=OptionsInfo(scss=True, category='summary_row', type='value', value=None), summary_row_text_transform=OptionsInfo(scss=True, category='summary_row', type='value', value='inherit'), summary_row_border_style=OptionsInfo(scss=True, category='summary_row', type='value', value='solid'), summary_row_border_width=OptionsInfo(scss=True, category='summary_row', type='px', value='2px'), summary_row_border_color=OptionsInfo(scss=True, category='summary_row', type='value', value='#D3D3D3'), grand_summary_row_padding=OptionsInfo(scss=True, category='grand_summary_row', type='px', value='8px'), grand_summary_row_padding_horizontal=OptionsInfo(scss=True, category='grand_summary_row', type='px', value='5px'), grand_summary_row_background_color=OptionsInfo(scss=True, category='grand_summary_row', type='value', value=None), grand_summary_row_text_transform=OptionsInfo(scss=True, category='grand_summary_row', type='value', value='inherit'), grand_summary_row_border_style=OptionsInfo(scss=True, category='grand_summary_row', type='value', value='double'), grand_summary_row_border_width=OptionsInfo(scss=True, category='grand_summary_row', type='px', value='6px'), grand_summary_row_border_color=OptionsInfo(scss=True, category='grand_summary_row', type='value', value='#D3D3D3'), footnotes_marks=OptionsInfo(scss=False, category='footnotes', type='values', value='numbers'), source_notes_padding=OptionsInfo(scss=True, category='source_notes', type='px', value='4px'), source_notes_padding_horizontal=OptionsInfo(scss=True, category='source_notes', type='px', value='5px'), source_notes_background_color=OptionsInfo(scss=True, category='source_notes', type='value', value=None), source_notes_font_size=OptionsInfo(scss=True, category='source_notes', type='px', value='10px'), source_notes_border_bottom_style=OptionsInfo(scss=True, category='source_notes', type='value', value='none'), source_notes_border_bottom_width=OptionsInfo(scss=True, category='source_notes', type='px', value='2px'), source_notes_border_bottom_color=OptionsInfo(scss=True, category='source_notes', type='value', value='#D3D3D3'), source_notes_border_lr_style=OptionsInfo(scss=True, category='source_notes', type='value', value='none'), source_notes_border_lr_width=OptionsInfo(scss=True, category='source_notes', type='px', value='2px'), source_notes_border_lr_color=OptionsInfo(scss=True, category='source_notes', type='value', value='#D3D3D3'), source_notes_multiline=OptionsInfo(scss=False, category='source_notes', type='boolean', value=True), source_notes_sep=OptionsInfo(scss=False, category='source_notes', type='value', value=' '), row_striping_background_color=OptionsInfo(scss=True, category='row', type='value', value='#F4F4F4'), row_striping_include_stub=OptionsInfo(scss=False, category='row', type='boolean', value=False), row_striping_include_table_body=OptionsInfo(scss=False, category='row', type='boolean', value=False), container_width=OptionsInfo(scss=False, category='container', type='px', value='auto'), container_height=OptionsInfo(scss=False, category='container', type='px', value='auto'), container_padding_x=OptionsInfo(scss=False, category='container', type='px', value='0px'), container_padding_y=OptionsInfo(scss=False, category='container', type='px', value='10px'), container_overflow_x=OptionsInfo(scss=False, category='container', type='overflow', value='auto'), container_overflow_y=OptionsInfo(scss=False, category='container', type='overflow', value='auto'), quarto_disable_processing=OptionsInfo(scss=False, category='quarto', type='logical', value=False), quarto_use_bootstrap=OptionsInfo(scss=False, category='quarto', type='logical', value=False)), _google_font_imports=GoogleFontImports(imports=frozenset()), _has_built=False)

Both models indicate that polluters have significantly higher yields after the PA than non-polluting firms. Note that the magnitude of the `treated` coefficient varies considerably across models.

## Visualizing Parallel Trends

Even though the regressions above indicate that there is an impact of the PA on bond yields of polluters, the tables do not tell us anything about the dynamics of the treatment effect. In particular, the models provide no indication about whether the crucial *parallel trends* assumption is valid. This assumption requires that in the absence of treatment, the difference between the two groups is constant over time. Although there is no well-defined statistical test for this assumption, visual inspection typically provides a good indication.

To provide such visual evidence, we revisit the simple OLS model and replace the `treated` and `post_period` indicators with month dummies for each group. This approach estimates the average yield change of both groups for each period and provides corresponding confidence intervals. Plotting the coefficient estimates for both groups around the treatment date shows us the dynamics of our panel data.

## R

``` r
model_without_fe_time <- feols(
  avg_yield ~ polluter + date:polluter + time_to_maturity + log_offering_amt,
  vcov = "iid",
  data = bonds_panel |>
    mutate(date = factor(date))
)

model_without_fe_coefs <- tidy(model_without_fe_time) |>
  filter(str_detect(term, "date")) |>
  mutate(
    date = ymd(substr(term, nchar(term) - 9, nchar(term))),
    treatment = str_detect(term, "TRUE"),
    ci_up = estimate + qnorm(0.975) * std.error,
    ci_low = estimate + qnorm(0.025) * std.error
  )

model_without_fe_coefs |>
  ggplot(
    aes(date, color = treatment, linetype = treatment, shape = treatment)
  ) +
  geom_vline(
    aes(xintercept = floor_date(treatment_date, "month")),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = 0),
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_up),
    alpha = 0.5
  ) +
  guides(linetype = "none") +
  geom_point(aes(y = estimate)) +
  labs(
    x = NULL,
    y = "Yield",
    shape = "Polluter?",
    color = "Polluter?",
    title = "Polluters respond stronger to Paris Agreement than green firms"
  )
```

[![Title: Polluters respond stronger to Paris Agreement than green firms. The figure shows a sequence of monthly dots for two groups. Before the agreement, the dots mainly overlap. Ahead of the agreement, yields start to increase. Then, after the agreement, there is a strong divergence in yields. Polluters have significantly higher yields than non-polluters in the months before and after the signing of the Paris Agreement. However, this yield difference vanishes again towards the end of 2016.](difference-in-differences_files/figure-html/fig-1301-1.png)](difference-in-differences_files/figure-html/fig-1301-1.png "Figure 1: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

Figure 1: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

## Python

``` python
model_without_fe_time = pf.feols(
    "avg_yield ~ C(polluter)/C(date) + time_to_maturity + log_offering_amt",
    vcov = "iid",
    data = bonds_panel.with_columns(pl.col("date").cast(pl.Utf8))
)

model_without_fe_coefs = (pl.DataFrame({
        "term": model_without_fe_time.coef().index.to_list(),
        "estimate": model_without_fe_time.coef().to_numpy(),
        "std_error": model_without_fe_time.se().to_numpy()
    })
    .filter(pl.col("term").str.contains("date"))
    .with_columns(
        treatment=pl.col("term").str.contains("True", literal=True),
        date=pl.col("term").str.extract(r"\[T\.(\d{4}-\d{2}-\d{2})"),
        ci_up=pl.col("estimate")+norm.ppf(0.975)*pl.col("std_error"),
        ci_low=pl.col("estimate")+norm.ppf(0.025)*pl.col("std_error")
    )
    .with_columns(date=pl.col("date").str.to_datetime("%Y-%m-%d"))
)

model_without_fe_figure = (
    ggplot(
        model_without_fe_coefs,
        aes(x="date", y="estimate", color="treatment",
            linetype="treatment", shape="treatment")
    )
    + geom_vline(xintercept=treatment_month, linetype="dashed")
    + geom_hline(yintercept=0, linetype="dashed")
    + geom_errorbar(aes(ymin="ci_low", ymax="ci_up"), alpha=0.5)
    + geom_point()
    + guides(linetype="none")
    + labs(
        x="", y="Yield", shape="Polluter?", color="Polluter?",
        title="Polluters respond stronger to Paris Agreement than green firms"
        )
    + scale_linetype_manual(values=["solid", "dashed"])
    + scale_x_date(date_breaks="1 year", date_labels="%Y")
)
model_without_fe_figure.show()
```

[![Title: Polluters respond stronger to Paris Agreement than green firms. The figure shows a sequence of monthly dots for two groups. Before the agreement, the dots mainly overlap. Ahead of the agreement, yields start to increase. Then, after the agreement, there is a strong divergence in yields. Polluters have significantly higher yields than non-polluters in the months before and after the signing of the Paris Agreement. However, this yield difference vanishes again towards the end of 2016.](difference-in-differences_files/figure-html/difference-in-differences-fig-1301-py-1.png)](difference-in-differences_files/figure-html/difference-in-differences-fig-1301-py-1.png "The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

[Figure 1](#fig-1301) shows that throughout most of 2014, the yields of the two groups changed in unison. However, starting at the end of 2014, the yields start to diverge, reaching the highest difference around the signing of the PA. Afterward, the yields for both groups fall again, and the polluters arrive at the same level as at the beginning of 2014. The non-polluters, on the other hand, even experience significantly lower yields than polluters after the signing of the agreement.

Instead of plotting both groups using the simple model approach, we can also use the fixed-effects model and focus on the polluter’s yield response to the signing relative to the non-polluters. To perform this estimation, we need to replace the `treated` indicator with separate time dummies for the polluters, each marking a one-month period relative to the treatment date. We then regress the monthly yields on the set of time dummies and `cusip_id` and `date` fixed effects.

## R

``` r
bonds_panel_alt <- bonds_panel |>
  mutate(
    diff_to_treatment = interval(
      floor_date(treatment_date, "month"),
      date
    ) %/%
      months(1)
  )

variables <- bonds_panel_alt |>
  distinct(diff_to_treatment, date) |>
  arrange(date) |>
  mutate(variable_name = as.character(NA))

formula <- "avg_yield ~ "

for (j in 1:nrow(variables)) {
  if (variables$diff_to_treatment[j] != 0) {
    old_names <- names(bonds_panel_alt)
    bonds_panel_alt <- bonds_panel_alt |>
      mutate(
        new_var = diff_to_treatment == variables$diff_to_treatment[j] &
          polluter
      )
    new_var_name <- ifelse(
      variables$diff_to_treatment[j] < 0,
      str_c("lag", abs(variables$diff_to_treatment[j])),
      str_c("lead", variables$diff_to_treatment[j])
    )
    variables$variable_name[j] <- new_var_name
    names(bonds_panel_alt) <- c(old_names, new_var_name)
    formula <- str_c(
      formula,
      ifelse(j == 1, new_var_name, str_c("+", new_var_name))
    )
  }
}
formula <- str_c(formula, "| cusip_id + date")

model_with_fe_time <- feols(
  as.formula(formula),
  vcov = "iid",
  data = bonds_panel_alt
)
```

    NOTE: 351/0 fixed-effect singletons were removed (351 observations).

``` r
model_with_fe_time_coefs <- tidy(model_with_fe_time) |>
  mutate(
    term = str_remove(term, "TRUE"),
    ci_up = estimate + qnorm(0.975) * std.error,
    ci_low = estimate + qnorm(0.025) * std.error
  ) |>
  left_join(
    variables,
    join_by(term == variable_name)
  ) |>
  bind_rows(tibble(
    term = "lag0",
    estimate = 0,
    ci_up = 0,
    ci_low = 0,
    date = floor_date(treatment_date, "month")
  ))

model_with_fe_time_coefs |>
  ggplot(aes(x = date, y = estimate)) +
  geom_vline(
    aes(xintercept = floor_date(treatment_date, "month")),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = 0),
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_up),
    alpha = 0.5
  ) +
  geom_point(aes(y = estimate)) +
  labs(
    x = NULL,
    y = "Yield",
    title = "Polluters' yield patterns around Paris Agreement signing"
  )
```

[![Title: Polluters' yield patterns around Paris Agreement signing. The figure shows a sequence of monthly dots for the treated group. Ahead of the agreement, yields of polluters start to increase. Then, after the agreement, there is a small reversal and yields drop again.](difference-in-differences_files/figure-html/fig-1302-3.png)](difference-in-differences_files/figure-html/fig-1302-3.png "Figure 2: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

Figure 2: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

## Python

We first compute, for each observation, the number of months relative to the treatment date and collect the distinct period markers.

``` python
bonds_panel_alt = (bonds_panel
    .with_columns(
        diff_to_treatment=(
            (pl.col("date").dt.year() - treatment_month.year) * 12
            + (pl.col("date").dt.month() - treatment_month.month)
        ).cast(pl.Int64)
    )
)

variables = (bonds_panel_alt
    .select(["diff_to_treatment", "date"])
    .unique()
    .sort("date")
    .with_columns(
        variable_name=pl.when(pl.col("diff_to_treatment") == 0)
        .then(pl.lit(None, dtype=pl.Utf8))
        .otherwise(
            pl.when(pl.col("diff_to_treatment") < 0)
            .then(pl.lit("lag"))
            .otherwise(pl.lit("lead"))
            + pl.col("diff_to_treatment").abs().cast(pl.Utf8)
        )
    )
)
```

In the next code chunk, we assemble the model formula and regress the monthly yields on the set of time dummies and `cusip_id` and `date` fixed effects.

``` python
formula = "avg_yield ~ 1"
lead_lag_columns = []

for row in variables.iter_rows(named=True):
    if row["diff_to_treatment"] != 0:
        new_var_name = row["variable_name"]
        lead_lag_columns.append(
            (
                (pl.col("diff_to_treatment") == row["diff_to_treatment"])
                & pl.col("polluter")
            ).alias(new_var_name)
        )
        formula += f" + {new_var_name}"

formula = formula + " | cusip_id + date "

bonds_panel_alt = bonds_panel_alt.with_columns(lead_lag_columns)

model_with_fe_time = pf.feols(
    formula,
    vcov = "iid",
    data = bonds_panel_alt
)
```

We then collect the regression results into a data frame that contains the estimates and corresponding 95 percent confidence intervals. Note that we also add a row with zeros for the (omitted) reference point of the time dummies.

``` python
lag0_row = pl.DataFrame({
    "term": ["lag0"],
    "estimate": [0.0],
    "std_error": [0.0],
    "ci_up": [0.0],
    "ci_low": [0.0],
    "diff_to_treatment": [0],
    "date": [treatment_month.date()]
})

model_with_fe_time_coefs = (pl.DataFrame({
        "term": model_with_fe_time.coef().index.to_list(),
        "estimate": model_with_fe_time.coef().to_numpy(),
        "std_error": model_with_fe_time.se().to_numpy()
    })
    .with_columns(
        ci_up=pl.col("estimate")+norm.ppf(0.975)*pl.col("std_error"),
        ci_low=pl.col("estimate")+norm.ppf(0.025)*pl.col("std_error")
    )
    .join(
        variables,
        how="left", left_on="term", right_on="variable_name"
    )
)

model_with_fe_time_coefs = pl.concat(
    [model_with_fe_time_coefs, lag0_row],
    how="diagonal"
)
```

[Figure 2](#fig-1302) shows the resulting coefficient estimates.

``` python
model_with_fe_time_figure = (
    ggplot(
        model_with_fe_time_coefs,
        aes(x="date", y="estimate")
    )
    + geom_vline(aes(xintercept=treatment_month),
                    linetype="dashed")
    + geom_hline(aes(yintercept=0), linetype="dashed")
    + geom_errorbar(aes(ymin="ci_low", ymax="ci_up"), alpha=0.5)
    + geom_point(aes(y="estimate"))
    + labs(
        x="", y="Yield",
        title="Polluters' yield patterns around Paris Agreement signing"
        )
    + scale_x_date(date_breaks="1 year", date_labels="%Y")
)
model_with_fe_time_figure.show()
```

[![Title: Polluters' yield patterns around Paris Agreement signing. The figure shows a sequence of monthly dots for the treated group. Ahead of the agreement, yields of polluters start to increase. Then, after the agreement, there is a small reversal and yields drop again.](difference-in-differences_files/figure-html/difference-in-differences-fig-1302-py-1.png)](difference-in-differences_files/figure-html/difference-in-differences-fig-1302-py-1.png "The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

The resulting graph shown in [Figure 2](#fig-1302) confirms the main conclusion of the previous image: polluters’ yield patterns show a considerable anticipation effect starting toward the end of 2014. Yields only marginally increase after the signing of the agreement. However, as opposed to the simple model, we do not see a complete reversal back to the pre-agreement level. Yields of polluters stay at a significantly higher level even one year after the signing.

Notice that during the year after the PA was signed, Donald Trump, the 45th president of the United States, was elected on November 8, 2016. During his campaign there were some indications of intentions to withdraw the US from the PA, which ultimately happened on November 4, 2020. Hence, reversal effects are potentially driven by these actions.

## Key Takeaways

- Difference-in-differences is a powerful tool for estimating causal effects in financial settings, especially when analyzing the impact of policy changes or shocks.
- It is important to assess the parallel trends assumption using graphical methods.
- The `fixest` (R) and `pyfixest` (Python) packages allow you to implement difference-in-differences regressions and visualize parallel trends.
- By combining panel data from TRACE and Mergent FISD with fixed effects regressions, you can evaluate how the Paris Agreement influenced corporate bond yields.
- The application shows that polluting firms experienced significantly higher yields leading up to and after the agreement, invalidating the parallel trends assumption.

## Exercises

1.  The 46th President of the US, Joe Biden, rejoined the Paris Agreement on February 19, 2021. Repeat the difference in differences analysis for the day of his election victory. Note that you will also have to download new TRACE data. How did polluters’ yields react to this action?
2.  Based on the exercise on ratings in [TRACE and FISD](../chapters/trace-and-fisd.llms.md), include ratings as a control variable in the analysis above. Do the results change?

## References

Bessembinder, Hendrik, Kathleen M Kahle, William F Maxwell, and Danielle Xu. 2008. “Measuring abnormal bond performance.” *Review of Financial Studies* 22 (10): 4219–58. <https://doi.org/10.1093/rfs/hhn105>.

Halling, Michael, Jin Yu, and Josef Zechner. 2021. “Primary Corporate Bond Markets and Social Responsibility.” *Working Paper*. <https://dx.doi.org/10.2139/ssrn.3681666>.

Handler, Lukas, Rainer Jankowitsch, and Alexander Pasler. 2022. “The Effects of ESG Performance and Preferences on US Corporate Bond Prices.” *Working Paper*. <https://dx.doi.org/10.2139/ssrn.4099566>.

Huynh, Thanh D., and Ying Xia. 2021. “Climate Change News Risk and Corporate Bond Returns.” *Journal of Financial and Quantitative Analysis* 56 (6): 1985–2009. <https://doi.org/10.1017/S0022109020000757>.

Roberts, Michael R., and Toni M. Whited. 2013. “Endogeneity in Empirical Corporate Finance.” In *Handbook of the Economics of Finance*, vol. 2. Elsevier. <https://EconPapers.repec.org/RePEc:eee:finchp:2-a-493-572>.

Seltzer, Lee H., Laura Starks, and Qifei Zhu. 2022. “Climate Regulatory Risk and Corporate Bonds.” *Working Paper*. <https://www.nber.org/papers/w29994>.

Virtanen, Pauli, Ralf Gommers, Travis E. Oliphant, et al. 2020. “SciPy 1.0: Fundamental Algorithms for Scientific Computing in Python.” *Nature Methods* 17: 261–72. <https://doi.org/10.1038/s41592-019-0686-2>.

## Footnotes

[^1]: Note that by using a generic name here, everybody can replace ours with their sample data and run the code to produce standard regression tables and illustrations.
