# Legacy pandas archive (issue #258)

This folder is a **frozen, unmaintained** snapshot of the original *pandas*
edition of *Tidy Finance with Python*. It exists because the unified-chapters
work replaces the old `/python/*.html` URLs with redirect stubs (PR #256), which
would otherwise remove the only browsable copy of the pandas edition.

## Provenance

- **Source:** `docs/python/`, `docs/site_libs/`, and the referenced
  `docs/assets/` files from commit
  `a56b3ceb5bcb7898932d9d5a2d3a8f73063ac15f` (the last `main` commit carrying
  the full rendered pandas output).
- **Recovery tag:** `legacy-pandas-snapshot` points at that commit. To re-capture
  from scratch:

  ```sh
  mkdir -p legacy-python-pandas
  git archive legacy-pandas-snapshot docs/python    | tar -x -C legacy-python-pandas --strip-components=2
  git archive legacy-pandas-snapshot docs/site_libs | tar -x -C legacy-python-pandas --strip-components=1
  # plus the 11 referenced assets/ files (see scripts/build-legacy-archive.py)
  ```

## Contents

- 31 chapter `*.html` pages (real content, ~28 MB with figures).
- `*_files/figure-html/*.png` — the figures referenced by each page.
- `site_libs/` — a **vendored, frozen** copy of the Bootstrap/Quarto bundles the
  pages reference (their content-hashed filenames differ from the live unified
  site, so sharing the live `site_libs` would render these pages unstyled).
- `assets/` — only the css/img files the pages actually reference.

## Build step (run once, before publishing)

The captured HTML still points at `../site_libs`, `../assets`, and
`../python/...`, embeds Google Analytics + a cookie-consent wall, and has no
"unmaintained" banner. Run the post-processor to fix all of that in place:

```sh
python scripts/build-legacy-archive.py --dry-run   # preview
python scripts/build-legacy-archive.py             # apply
```

It rewrites paths to the vendored bundles, makes chapter-to-chapter navigation
stay inside the archive, injects the deprecation banner, strips tracking, adds
`noindex`, and removes the search box and the `.llms.md` companions.

## Publishing

After the build step, register the folder so Quarto copies it verbatim into the
output without rendering or indexing it. In `_quarto.yml`:

```yaml
project:
  type: website
  output-dir: docs
  resources:
    - legacy-python-pandas
```

It is reachable from the blog hub post at `blog/legacy-python-pandas/index.qmd`.

## Maintenance

**None.** These bytes are static and never re-executed. If a page goes stale,
that is by design and is disclosed in the per-page banner.
