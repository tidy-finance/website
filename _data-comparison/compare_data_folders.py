# Compare the contents of data-r/ and data-python/ table by table.
#
# Goal: verify both books build on the same data, as groundwork for a joint
# data folder. Reports schema differences, row-count gaps, and value-level
# differences on key-matched rows, then ranks the most relevant findings.
#
# Usage:  uv run python _data-comparison/compare_data_folders.py [--skip-big]
#   --skip-big  skips the partitioned datasets (crsp_daily, trace_enhanced)
#
# The folder name starts with "_" so quarto ignores it during project renders.

import sys
from pathlib import Path

import polars as pl

ROOT = Path(__file__).resolve().parents[1]
R_DIR, P_DIR = ROOT / "data-r", ROOT / "data-python"
SKIP_BIG = "--skip-big" in sys.argv

# R name -> Python name (tables that differ in naming)
# (compustat was renamed to compustat_annual in the R book; mapping kept as
#  a fallback for stale data folders that predate the rename)
NAME_MAP = {"compustat": "compustat_annual"}

# join keys per (python) table name; None -> aggregate comparison only
KEYS = {
    "factors_ff3_monthly": ["date"],
    "factors_ff5_monthly": ["date"],
    "factors_ff3_daily": ["date"],
    "factors_q_monthly": ["date"],
    "industries_ff_monthly": ["date"],
    "macro_predictors": ["date"],
    "cpi_monthly": ["date"],
    "crsp_monthly": ["permno", "date"],
    "beta": ["permno", "date", "return_type"],
    "compustat_annual": ["gvkey", "datadate"],
    "fisd": ["complete_cusip"],
}

TOL = 1e-8          # threshold above which a numeric diff counts as real
EPS_NOTE = 1e-12    # below this, treat as machine epsilon

highlights = []     # (severity, message); severity 0=critical .. 3=info


def note(severity, msg):
    highlights.append((severity, msg))


def harmonize_keys(df, keys):
    """Cast join keys to comparable dtypes across R/pandas/polars vintages."""
    casts = []
    for k in keys:
        if k not in df.columns:
            return None
        dt = df.schema[k]
        if dt in (pl.Datetime("ns"), pl.Datetime("us"), pl.Datetime("ms")):
            casts.append(pl.col(k).cast(pl.Date))
        elif k == "permno":
            casts.append(pl.col(k).cast(pl.Int64))
        elif dt == pl.Decimal:
            casts.append(pl.col(k).cast(pl.Float64))
    return df.with_columns(casts) if casts else df


def compare_table(name_r, name_p):
    r_path, p_path = R_DIR / f"{name_r}.parquet", P_DIR / f"{name_p}.parquet"
    print(f"\n=== {name_p} (R: {name_r}) ===")
    if not r_path.exists() or not p_path.exists():
        print(f"  MISSING: R={r_path.exists()} PY={p_path.exists()}")
        note(0, f"{name_p}: file missing on one side (R={r_path.exists()}, PY={p_path.exists()})")
        return

    r, p = pl.read_parquet(r_path), pl.read_parquet(p_path)

    # --- schema ---
    rs, ps = dict(r.schema), dict(p.schema)
    only_r = sorted(set(rs) - set(ps))
    only_p = sorted(set(ps) - set(rs))
    dtype_diff = {c: (str(rs[c]), str(ps[c])) for c in rs if c in ps and rs[c] != ps[c]}
    if only_r:
        print(f"  cols only in R:  {only_r}")
        note(1, f"{name_p}: columns only in R: {only_r}")
    if only_p:
        print(f"  cols only in PY: {only_p}")
        note(1, f"{name_p}: columns only in PY: {only_p}")
    if dtype_diff:
        print(f"  dtype diffs (R vs PY): {dtype_diff}")
        # Date vs Datetime is representation, everything else is structural
        bad = {c: v for c, v in dtype_diff.items()
               if not ("Date" in v[0] and "Datetime" in v[1])
               and not ("Datetime" in v[0] and "Date" in v[1])}
        note(1 if bad else 2, f"{name_p}: dtype differences {dtype_diff}")
    if not (only_r or only_p or dtype_diff):
        print(f"  schema: IDENTICAL ({len(rs)} cols)")

    # --- rows ---
    flag = "" if r.height == p.height else "  <-- DIFFERS"
    print(f"  rows: R={r.height:,} PY={p.height:,}{flag}")
    if r.height != p.height:
        note(1, f"{name_p}: row counts differ (R={r.height:,}, PY={p.height:,})")

    # --- values on matched keys ---
    keys = KEYS.get(name_p)
    if keys is None:
        return
    r2, p2 = harmonize_keys(r, keys), harmonize_keys(p, keys)
    if r2 is None or p2 is None:
        print(f"  (join keys {keys} not present on both sides; skipping value check)")
        note(2, f"{name_p}: could not value-compare (missing keys {keys})")
        return

    j = r2.join(p2, on=keys, how="inner", suffix="_py")
    unmatched_r, unmatched_p = r.height - j.height, p.height - j.height
    print(f"  matched on {keys}: {j.height:,} rows"
          + (f" (unmatched R: {unmatched_r:,}, PY: {unmatched_p:,})"
             if (unmatched_r or unmatched_p) else ""))
    if max(unmatched_r, unmatched_p) > 0.01 * max(r.height, p.height):
        note(1, f"{name_p}: >1% of rows unmatched on {keys} "
                f"(R: {unmatched_r:,}, PY: {unmatched_p:,})")

    common = [c for c in rs if c in ps and c not in keys]
    worst = []
    for c in common:
        a, b = j[c], j[f"{c}_py"]
        if a.dtype == pl.String or b.dtype == pl.String:
            n_diff = (a.cast(pl.String) != b.cast(pl.String)).sum()
            status = "identical" if n_diff == 0 else f"{n_diff:,} string mismatches"
            if n_diff:
                note(1, f"{name_p}.{c}: {n_diff:,} string mismatches")
        elif a.dtype in (pl.Date, pl.Datetime("ns"), pl.Datetime("us")) or \
             b.dtype in (pl.Date, pl.Datetime("ns"), pl.Datetime("us")):
            n_diff = (a.cast(pl.Date) != b.cast(pl.Date)).sum()
            status = "identical" if n_diff == 0 else f"{n_diff:,} date mismatches"
            if n_diff:
                note(1, f"{name_p}.{c}: {n_diff:,} date mismatches")
        else:
            a, b = a.cast(pl.Float64), b.cast(pl.Float64)
            null_mm = int((a.is_null() != b.is_null()).sum())
            d = (a - b).abs()
            mx = d.max()
            n_real = int((d > TOL).sum())
            if mx is None or (mx <= EPS_NOTE and null_mm == 0):
                status = "identical" + (f" (eps {mx:.1e})" if mx else "")
            else:
                status = (f"max|diff|={mx:.3e}, >tol: {n_real:,}/{j.height:,}, "
                          f"null-mismatch: {null_mm:,}")
                if n_real or null_mm:
                    worst.append((mx if mx else 0, c, n_real, null_mm))
        print(f"    {c}: {status}")

    for mx, c, n_real, null_mm in sorted(worst, reverse=True)[:3]:
        note(1, f"{name_p}.{c}: max|diff|={mx:.3e} on {n_real:,} rows"
                + (f", {null_mm:,} null mismatches" if null_mm else ""))


def compare_partitioned(name, agg_keys):
    """Aggregate-level comparison for large partitioned datasets."""
    r_path, p_path = R_DIR / name, P_DIR / name
    print(f"\n=== {name}/ (partitioned, aggregate comparison) ===")
    if not r_path.is_dir() or not p_path.is_dir():
        print(f"  MISSING: R={r_path.is_dir()} PY={p_path.is_dir()}")
        note(0, f"{name}: directory missing on one side")
        return
    try:
        lr = pl.scan_parquet(str(r_path / "**" / "*.parquet"), extra_columns="ignore")
        lp = pl.scan_parquet(str(p_path / "**" / "*.parquet"), extra_columns="ignore")
        rsch = lr.collect_schema()
        psch = lp.collect_schema()
        num_cols = [c for c in rsch if c in psch and rsch[c].is_numeric()
                    and c not in agg_keys][:3]
        date_col = next((c for c in rsch if "date" in c.lower() or c == "trd_exctn_dt"), None)
        aggs = [pl.len().alias("n")] + [pl.col(c).cast(pl.Float64).mean().alias(f"mean_{c}") for c in num_cols]
        if date_col:
            group = pl.col(date_col).cast(pl.Date).dt.year().alias("year")
            ar = lr.group_by(group).agg(aggs).sort("year").collect()
            ap = lp.group_by(group).agg(aggs).sort("year").collect()
            jj = ar.join(ap, on="year", suffix="_py")
            n_mismatch = int((jj["n"] != jj["n_py"]).sum())
            print(f"  years: R={ar.height} PY={ap.height} | per-year row-count mismatches: {n_mismatch}")
            if n_mismatch:
                bad_years = jj.filter(pl.col("n") != pl.col("n_py"))["year"].to_list()
                note(1, f"{name}: per-year row counts differ in {n_mismatch} years (e.g., {bad_years[:5]})")
            for c in num_cols:
                dmax = (jj[f"mean_{c}"] - jj[f"mean_{c}_py"]).abs().max()
                print(f"  per-year mean {c}: max|diff|={dmax:.3e}" if dmax is not None else f"  {c}: n/a")
                if dmax and dmax > TOL:
                    note(1, f"{name}: per-year mean of {c} differs up to {dmax:.3e}")
        else:
            nr = lr.select(pl.len()).collect().item()
            np_ = lp.select(pl.len()).collect().item()
            print(f"  total rows: R={nr:,} PY={np_:,}")
            if nr != np_:
                note(1, f"{name}: total rows differ (R={nr:,}, PY={np_:,})")
    except Exception as e:
        print(f"  comparison failed: {type(e).__name__}: {e}")
        note(2, f"{name}: aggregate comparison failed ({type(e).__name__})")


# ---------------------------------------------------------------- main ---
r_tables = {f.stem for f in R_DIR.glob("*.parquet")}
p_tables = {f.stem for f in P_DIR.glob("*.parquet")}
r_mapped = {NAME_MAP.get(t, t): t for t in r_tables}

only_r = sorted(set(r_mapped) - p_tables)
only_p = sorted(p_tables - set(r_mapped))
if only_r:
    print(f"tables only in data-r:     {only_r}")
    note(2, f"tables only in data-r: {only_r}")
if only_p:
    print(f"tables only in data-python: {only_p}")
    note(2, f"tables only in data-python: {only_p}")

for p_name in sorted(set(r_mapped) & p_tables):
    compare_table(r_mapped[p_name], p_name)

if not SKIP_BIG:
    for d in ("crsp_daily", "trace_enhanced"):
        compare_partitioned(d, agg_keys=[])

# ----------------------------------------------------------- highlights ---
print("\n" + "=" * 64)
print("HIGHLIGHTS (most relevant differences first)")
print("=" * 64)
if not highlights:
    print("  None - folders are equivalent at the chosen tolerance.")
for sev, msg in sorted(highlights, key=lambda x: x[0]):
    label = ["CRITICAL", "RELEVANT", "MINOR", "INFO"][sev]
    print(f"  [{label}] {msg}")
