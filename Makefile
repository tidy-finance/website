data_downloads:
	quarto render chapters/accessing-and-managing-financial-data.qmd
	quarto render chapters/wrds-crsp-and-compustat.qmd
	quarto render chapters/trace-and-fisd.qmd

intro_chapters:
	quarto render chapters/working-with-stock-returns.qmd
	quarto render chapters/modern-portfolio-theory.qmd
	quarto render chapters/capital-asset-pricing-model.qmd
	quarto render chapters/financial-statement-analysis.qmd
	quarto render chapters/discounted-cash-flow-analysis.qmd

other_chapters:
	quarto render chapters/beta-estimation.qmd
	quarto render chapters/univariate-portfolio-sorts.qmd
	quarto render chapters/size-sorts-and-p-hacking.qmd
	quarto render chapters/value-and-bivariate-sorts.qmd
	quarto render chapters/replicating-fama-and-french-factors.qmd
	quarto render chapters/fama-macbeth-regressions.qmd
	quarto render chapters/fixed-effects-and-clustered-standard-errors.qmd
	quarto render chapters/difference-in-differences.qmd
	quarto render chapters/factor-selection-via-machine-learning.qmd
	quarto render chapters/option-pricing-via-machine-learning.qmd
	quarto render chapters/parametric-portfolio-policies.qmd
	quarto render chapters/constrained-optimization-and-backtesting.qmd
