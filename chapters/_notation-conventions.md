# Notation Conventions

Internal reference for authors and reviewers. This file is not rendered
(Quarto skips files starting with `_`). When you write or edit a chapter,
make every equation conform to the conventions below; when you deviate on
purpose, say so explicitly in the chapter text.

## Subscripts and indices

- Entity first, time second: $r_{i,t}$, $x_{i,t-1}$, $\beta_{i,t}$ —
  never $r_{t,i}$.
- $i$ (and $j$ for a second entity) index assets/firms, $t$ indexes time,
  $k$ indexes factors or regressors. A summation index must appear in the
  summand (write $\sum_{j=1}^{N}\omega_{j,t}$, not
  $\sum_{j=1}^{N}\omega_{i,t}$).
- Date each term consistently: in a regression of $r_{i,t+1}$, the error
  is $\epsilon_{i,t+1}$; in a period-by-period cross-sectional regression,
  the intercept is $\alpha_t$ (common per period), while an asset-specific
  pricing error is $\alpha_i$.

## Canonical symbols

| Object | Symbol | Notes |
|---|---|---|
| Return, excess return | $r_{i,t}$ | excess unless stated otherwise |
| Risk-free rate | $r_f$ | never plain $r$ in the same display |
| Market return | $r_{m,t}$ | |
| Portfolio weights | $\omega$ | never $w$; pre-rebalancing: $\omega_{t^+}$ |
| Vector of ones | $\iota$ | |
| Expected returns, covariance | $\mu$, $\Sigma$ | sample versions $\hat\mu$, $\hat\Sigma$ |
| Factor loadings | $\beta$ | |
| Risk premia (Fama-MacBeth) | $\lambda_t$ | |
| Lagrange multiplier (efficient portfolio) | $\tilde\lambda$ | matches `lambda_tilde` in the code and the Proofs appendix |
| Risk aversion | $\gamma$ | |
| Penalty/shrinkage parameter | $\lambda$ | see elastic net below |
| Parametric policy parameters | $\theta$ | |
| Transaction-cost parameter | $\beta$ (scalar), $B$ (matrix) | the function signature must list every argument it uses, e.g. $\nu_t(\omega, B)$ |

## Elastic net

We parameterize the elastic net as
$\lambda\rho\sum_k|\beta_k| + \tfrac{1}{2}\lambda(1-\rho)\sum_k\beta_k^2$,
so $\rho = 1$ is the Lasso and $\rho = 0$ is Ridge. This matches the
mixing-parameter convention of every backend we use: `mixture` (tidymodels),
`alpha` (glmnet), and `l1_ratio` (scikit-learn) all equal $\rho$. Note that
scikit-learn calls the *shrinkage* parameter `alpha` (our $\lambda$) —
flag this in prose wherever both appear.

## Operators and formatting

- Use `\max`, `\min`, `\log`, `\arg\min` — never bare `max(x, 0)` in math.
- Use `\geq` / `\leq` — never ASCII `>=` / `<=` inside math (the lint in
  `scripts/lint-chapters.py` enforces this).
- Transposes: $\omega'\mu$ with a plain prime; do not double-prime or
  attach primes to closing arguments ($\omega'\Sigma\omega$, not
  $\omega'\Sigma\omega'$).
- Hadamard product: $\circ$; Kronecker product: $\otimes$.
- Cross-references: write `@fig-xxx` / `@eq-xxx` bare — Quarto adds the
  "Figure"/"Equation" prefix (the lint flags `Figure @fig-`).

## Keeping chapters, code, and appendix in sync

Every displayed equation has up to three counterparts: the implementing
code chunk, the Proofs appendix, and other chapters using the same object.
When you change one, check the others. Known conventions:

- The efficient-portfolio algebra ($C$, $D$, $E$, $\tilde\lambda$,
  $\omega_\text{mvp}$, $\omega_\text{efp}$) is defined in
  `proofs.qmd` — chapters must use identical symbols.
- Code variable names should transliterate the math where practical
  (`lambda_tilde`, `omega_mvp`, `w_prev` for $\omega_{t^+}$).
