#!/usr/bin/env python3
"""Post-process the captured legacy pandas archive (GitHub issue #258).

DRAFT — review before running. This script has NOT been executed yet.

Context
-------
The folder ``legacy-python-pandas/`` holds a byte-for-byte capture of the last
``main`` commit that still carried the full rendered *Tidy Finance with Python*
(pandas) output at ``docs/python/`` (recovery tag: ``legacy-pandas-snapshot``).
Once ``prep/unified-chapters`` merges, the old ``/python/*.html`` URLs become
redirect stubs (PR #256), so this frozen copy is the only browsable pandas
edition. This script turns the raw capture into a self-contained, clearly
"unmaintained" mini-site that can be published verbatim under
``docs/legacy-python-pandas/``.

What it does (one pass over the captured HTML)
----------------------------------------------
1. Safety assertion: refuse to run if the pages look like 18-line redirect
   stubs instead of real content (guards against running on a post-merge tree).
2. Rewrite ``../site_libs`` -> ``site_libs`` and ``../assets`` -> ``assets`` so
   pages resolve against the VENDORED frozen bundles in this folder (NOT the
   live site_libs, whose hashes differ on the unified branch).
3. Rewrite intra-archive nav ``../python/<x>.html`` -> ``<x>.html`` so chapter
   links stay inside the archive instead of bouncing through #256 redirects.
4. Repoint remaining live-site links (``../r/...``, ``../index.html``,
   ``../blog.html`` ...) to absolute https://www.tidy-finance.org/ URLs.
5. Inject a sticky "archived & unmaintained" banner after <body>.
6. Strip Google Analytics (gtag) and the cookie-consent wall.
7. Add <meta name="robots" content="noindex"> and fix the og:image URL.
8. Best-effort removal of the in-page search box (it would query the new
   /chapters/ index).
9. Drop the .llms.md companions (dead clutter, never linked).

Idempotent: re-running is safe (path rewrites are no-ops the second time; the
banner and noindex tag are guarded by markers).

Usage
-----
    python scripts/build-legacy-archive.py            # process in place
    python scripts/build-legacy-archive.py --dry-run  # report only, no writes

Open decisions still pending owner sign-off (see issue #258):
  * exact banner wording / freeze date
  * which chapters to include (currently: all 31)
  * whether to strip vs keep the search box
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

ARCHIVE = Path(__file__).resolve().parent.parent / "legacy-python-pandas"
SITE = "https://www.tidy-finance.org"
ARCHIVE_PATH = "/legacy-python-pandas"
FREEZE_LABEL = "June 2026"  # TODO: confirm with owner
BANNER_MARKER = "legacy-archive-banner"

BANNER = (
    f'<div id="{BANNER_MARKER}" role="note" '
    'style="position:sticky;top:0;z-index:3000;background:#b45309;color:#fff;'
    'padding:.6rem 1rem;font-size:.9rem;line-height:1.4;text-align:center;'
    'box-shadow:0 1px 4px rgba(0,0,0,.25);">'
    '⚠️ <strong>Archived &amp; unmaintained.</strong> '
    'This is the original <em>pandas</em> edition of Tidy Finance with Python, '
    f'frozen as of {FREEZE_LABEL}. It is no longer updated and may not run with '
    'current package versions. The maintained edition (R and Python/polars) '
    'lives at <a href="/chapters/" style="color:#fff;text-decoration:underline;">'
    'tidy-finance.org/chapters</a>.'
    "</div>"
)

NOINDEX = '<meta name="robots" content="noindex, nofollow">'


SENTINEL = "beta-estimation.html"  # a known large content page


def is_redirect_stub(html: str) -> bool:
    """A Quarto redirect stub: tiny and built around window.location.replace."""
    return len(html) < 2000 and "window.location.replace" in html


def assert_correct_tree(archive: Path) -> None:
    """Guard against running on the wrong/post-merge tree.

    Pre-merge, beta-estimation.html is a full ~138 KB chapter; post-merge it is
    an 18-line redirect stub to ../chapters/. A couple of legitimate pre-merge
    stubs (e.g. introduction-to-tidy-finance -> working-with-stock-returns) are
    expected and handled per file; the sentinel just confirms real content was
    captured.
    """
    sentinel = archive / SENTINEL
    if not sentinel.is_file() or is_redirect_stub(sentinel.read_text(encoding="utf-8")):
        sys.exit(
            f"ABORT: {SENTINEL} is missing or a redirect stub. Re-capture from "
            "the 'legacy-pandas-snapshot' tag (main), not the post-merge tree."
        )


def rewrite_links(html: str) -> str:
    # 2. vendored bundles (must run before the generic ../ catch-all)
    html = html.replace("../site_libs/", "site_libs/")
    html = html.replace("../assets/", "assets/")
    # 3. intra-archive chapter nav: ../python/foo.html -> foo.html
    html = html.replace("../python/", "")
    # 4a. R language toggle -> root-relative (redirects to /chapters post-merge).
    #     Root-relative (not absolute) so links stay same-origin: clickable in
    #     local preview AND correct in production.
    html = html.replace("../r/", "/r/")
    # 4b. remaining live-site links (index/blog/contribute/...) -> root-relative
    html = re.sub(r'(href|src)="\.\./', r'\1="/', html)
    # 7. og:image: keep ABSOLUTE (Open Graph requires absolute URLs); just
    #    repoint the dead /python/ path to the archive path.
    html = html.replace(f"{SITE}/python/", f"{SITE}{ARCHIVE_PATH}/")
    return html


def inject_head_and_banner(html: str) -> str:
    # 7. noindex (guarded)
    if "robots" not in html:
        html = re.sub(r"(<head[^>]*>)", r"\1\n" + NOINDEX, html, count=1)
    # 5. banner right after <body> (guarded)
    if BANNER_MARKER not in html:
        html = re.sub(r"(<body[^>]*>)", r"\1\n" + BANNER, html, count=1)
    return html


def strip_tracking(html: str) -> str:
    # 6a. GA async loader
    html = re.sub(
        r'<script[^>]*googletagmanager[^>]*>\s*</script>\s*', "", html, flags=re.I
    )
    # 6b. gtag init block (gated under cookie-consent type="text/plain")
    html = re.sub(
        r'<script[^>]*cookie-consent="tracking"[^>]*>.*?</script>\s*',
        "",
        html,
        flags=re.I | re.S,
    )
    # 6c. inline gtag() config block, if any survives standalone
    html = re.sub(
        r"<script[^>]*>\s*window\.dataLayer.*?gtag\('config'.*?</script>\s*",
        "",
        html,
        flags=re.I | re.S,
    )
    # 6d. cookie-consent includes + init
    html = re.sub(r'<script[^>]*cookie-consent/cookie-consent\.js[^>]*>\s*</script>\s*', "", html, flags=re.I)
    html = re.sub(r'<link[^>]*cookie-consent/cookie-consent\.css[^>]*>\s*', "", html, flags=re.I)
    html = re.sub(r"<script[^>]*>\s*[^<]*cookieconsent\.run.*?</script>\s*", "", html, flags=re.I | re.S)
    return html


def strip_search(html: str) -> str:
    # 8. best-effort: drop search JS, options blob, and the search widgets
    html = re.sub(r'<script[^>]*quarto-search/[^>]*>\s*</script>\s*', "", html, flags=re.I)
    html = re.sub(r'<script[^>]*id="quarto-search-options"[^>]*>.*?</script>\s*', "", html, flags=re.I | re.S)
    html = re.sub(r'<div[^>]*id="quarto-search-results"[^>]*>\s*</div>\s*', "", html, flags=re.I)
    html = re.sub(r'<div[^>]*id="quarto-search"[^>]*>\s*</div>\s*', "", html, flags=re.I)
    html = re.sub(r'<button[^>]*quarto-search-button[^>]*>.*?</button>\s*', "", html, flags=re.I | re.S)
    return html


def process_file(path: Path, dry_run: bool) -> None:
    html = path.read_text(encoding="utf-8")
    if is_redirect_stub(html):
        # Legitimate pre-merge alias (e.g. a renamed chapter). Its redirect
        # target is already a sibling .html, so it works inside the archive
        # as-is; nothing to rewrite, banner, or strip.
        print(f"  skipped (redirect stub): {path.name}")
        return
    out = html
    out = rewrite_links(out)
    out = strip_tracking(out)
    out = strip_search(out)
    out = inject_head_and_banner(out)
    if out != html and not dry_run:
        path.write_text(out, encoding="utf-8")
    status = "would update" if dry_run else "updated"
    print(f"  {status}: {path.name}" if out != html else f"  unchanged: {path.name}")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true", help="report only; do not write")
    args = ap.parse_args()

    if not ARCHIVE.is_dir():
        sys.exit(f"ABORT: archive folder not found: {ARCHIVE}")

    html_files = sorted(ARCHIVE.glob("*.html"))
    if not html_files:
        sys.exit("ABORT: no .html files in archive folder.")

    assert_correct_tree(ARCHIVE)
    print(f"Processing {len(html_files)} HTML files in {ARCHIVE} ...")
    for f in html_files:
        process_file(f, args.dry_run)

    # 9. drop .llms.md companions
    llms = sorted(ARCHIVE.glob("*.llms.md"))
    print(f"Dropping {len(llms)} .llms.md companion files ...")
    for f in llms:
        if not args.dry_run:
            f.unlink()
        print(f"  {'would remove' if args.dry_run else 'removed'}: {f.name}")

    print("Done. Next: wire `resources: [legacy-python-pandas]` into _quarto.yml,")
    print("render, and verify (CSS/highlighting/figures/banner, no GA, sibling nav).")


if __name__ == "__main__":
    main()
