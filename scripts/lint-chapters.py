#!/usr/bin/env python3
"""Hygiene linter for the book chapters in chapters/*.qmd.

Catches mechanical defects that slip through rendering unnoticed:

  artifact-tag        stray tool-call/XML residue from machine-assisted edits
                      (e.g., </content> or </invoke> committed at end of file)
  malformed-option    Quarto cell options written as '# |' instead of '#|'
  ascii-math          ASCII '>=' / '<=' inside math (use \\geq / \\leq)
  figure-prefix       'Figure @fig-...' double prefix (Quarto adds 'Figure')
  heading-case        'Key takeaways' headings not matching '## Key Takeaways'
  empty-tab           a '### R' / '### Python' tab inside a panel-tabset with
                      no content before the next tab or the end of the tabset
  missing-fig-alt     a code cell that sets fig-cap but no fig-alt

Exits non-zero if any issue is found. Run from the repository root:

    python scripts/lint-chapters.py [files...]
"""

import re
import sys
from pathlib import Path

ARTIFACT_RE = re.compile(
    r"</?(?:content|invoke|parameter|function_calls|function_results)>"
    r"|<invoke\s|<parameter\s|antml"
)
MALFORMED_OPTION_RE = re.compile(r"^#\s+\|\s*\w")
INLINE_MATH_RE = re.compile(r"\$[^$]*(?:>=|<=)[^$]*\$")
FIGURE_PREFIX_RE = re.compile(r"[Ff]igure\s+@fig-")
HEADING_CASE_RE = re.compile(r"^#{1,6}\s+Key\s+[Tt]akeaways\s*$")
HEADING_OK = re.compile(r"^#{1,6}\s+Key Takeaways\s*$")
CELL_FENCE_RE = re.compile(r"^```\{[a-zA-Z]")
FENCE_RE = re.compile(r"^```")
TABSET_OPEN_RE = re.compile(r"^:::+\s*\{\.panel-tabset")
DIV_OPEN_RE = re.compile(r"^:::+\s*\{")
DIV_CLOSE_RE = re.compile(r"^:::+\s*$")
TAB_HEADING_RE = re.compile(r"^###\s+\S")


def lint_file(path: Path) -> list[tuple[int, str, str]]:
    issues = []
    lines = path.read_text(encoding="utf-8").splitlines()

    in_cell = False          # inside a ```{r} / ```{python} code cell
    in_display_math = False  # inside a $$ ... $$ block
    tabset_depth = 0         # nesting depth of panel-tabset divs
    div_stack = []           # True for panel-tabset divs, False otherwise
    tab_heading_line = None  # line number of the current tab heading
    tab_has_content = False
    cell_opts: dict[str, int] = {}  # option name -> line, for current cell
    cell_start = 0

    def close_tab(lineno):
        nonlocal tab_heading_line, tab_has_content
        if tab_heading_line is not None and not tab_has_content:
            issues.append(
                (tab_heading_line, "empty-tab",
                 f"tab '{lines[tab_heading_line - 1].strip()}' has no content")
            )
        tab_heading_line = None
        tab_has_content = False

    for i, raw in enumerate(lines, start=1):
        line = raw.rstrip("\n")
        stripped = line.strip()

        if ARTIFACT_RE.search(line):
            issues.append((i, "artifact-tag", stripped[:80]))

        if in_cell:
            if FENCE_RE.match(stripped):
                if "fig-cap" in cell_opts and "fig-alt" not in cell_opts:
                    issues.append(
                        (cell_start, "missing-fig-alt",
                         "cell sets fig-cap but no fig-alt")
                    )
                in_cell = False
                cell_opts = {}
                if tab_heading_line is not None:
                    tab_has_content = True
                continue
            if MALFORMED_OPTION_RE.match(stripped):
                issues.append((i, "malformed-option", stripped[:80]))
            m = re.match(r"#\|\s*([\w-]+)\s*:", stripped)
            if m:
                cell_opts[m.group(1)] = i
            continue

        if CELL_FENCE_RE.match(stripped):
            in_cell = True
            cell_start = i
            cell_opts = {}
            continue

        dollar_count = stripped.count("$$")
        if dollar_count:
            # an odd number of $$ delimiters opens or closes a display block;
            # an even number (e.g., '$$ x $$ where ...') is self-contained
            if ">=" in stripped or "<=" in stripped:
                issues.append((i, "ascii-math", stripped[:80]))
            if dollar_count % 2 == 1:
                in_display_math = not in_display_math
            if tab_heading_line is not None and stripped:
                tab_has_content = True
            continue

        if in_display_math:
            if ">=" in stripped or "<=" in stripped:
                issues.append((i, "ascii-math", stripped[:80]))
            if tab_heading_line is not None and stripped:
                tab_has_content = True
            continue

        if INLINE_MATH_RE.search(line):
            issues.append((i, "ascii-math", stripped[:80]))

        if FIGURE_PREFIX_RE.search(line):
            issues.append((i, "figure-prefix", stripped[:80]))

        if HEADING_CASE_RE.match(line) and not HEADING_OK.match(line):
            issues.append((i, "heading-case", stripped[:80]))

        if TABSET_OPEN_RE.match(stripped):
            div_stack.append(True)
            tabset_depth += 1
            continue
        if DIV_OPEN_RE.match(stripped):
            div_stack.append(False)
            continue
        if DIV_CLOSE_RE.match(stripped):
            if div_stack:
                was_tabset = div_stack.pop()
                if was_tabset:
                    close_tab(i)
                    tabset_depth -= 1
            continue

        if tabset_depth > 0 and TAB_HEADING_RE.match(stripped):
            close_tab(i)
            tab_heading_line = i
            tab_has_content = False
            continue

        if tab_heading_line is not None and stripped:
            tab_has_content = True

    # unterminated leftovers at EOF
    close_tab(len(lines))

    return issues


def main(argv: list[str]) -> int:
    if len(argv) > 1:
        files = [Path(p) for p in argv[1:]]
    else:
        files = sorted(Path("chapters").glob("*.qmd"))
    if not files:
        print("lint-chapters: no .qmd files found", file=sys.stderr)
        return 2

    total = 0
    for path in files:
        for lineno, code, detail in lint_file(path):
            print(f"{path}:{lineno}: [{code}] {detail}")
            total += 1

    if total:
        print(f"\nlint-chapters: {total} issue(s) found", file=sys.stderr)
        return 1
    print(f"lint-chapters: {len(files)} file(s) clean")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
