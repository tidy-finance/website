data_downloads:
	quarto render r/accessing-and-managing-financial-data.qmd
	quarto render r/wrds-crsp-and-compustat.qmd
	quarto render r/trace-and-fisd.qmd
	quarto render python/accessing-and-managing-financial-data.qmd
	quarto render python/wrds-crsp-and-compustat.qmd 
	quarto render python/trace-and-fisd.qmd

other_chapters: other_chapters_r other_chapters_python
	
other_chapters_r:
	quarto render r/working-with-stock-returns.qmd
	quarto render r/modern-portfolio-theory.qmd
	quarto render r/capital-asset-pricing-model.qmd
	quarto render r/financial-statement-analysis.qmd
	quarto render r/discounted-cash-flow-analysis.qmd
	quarto render r/beta-estimation.qmd
	quarto render r/univariate-portfolio-sorts.qmd
	quarto render r/size-sorts-and-p-hacking.qmd
	quarto render r/value-and-bivariate-sorts.qmd
	quarto render r/replicating-fama-and-french-factors.qmd
	quarto render r/fama-macbeth-regressions.qmd
	quarto render r/fixed-effects-and-clustered-standard-errors.qmd
	quarto render r/difference-in-differences.qmd
	quarto render r/factor-selection-via-machine-learning.qmd
	quarto render r/option-pricing-via-machine-learning.qmd
	quarto render r/parametric-portfolio-policies.qmd
	quarto render r/constrained-optimization-and-backtesting.qmd
	
other_chapters_python:
	quarto render python/introduction-to-tidy-finance.qmd
	quarto render python/beta-estimation.qmd
	quarto render python/univariate-portfolio-sorts.qmd
	quarto render python/size-sorts-and-p-hacking.qmd
	quarto render python/value-and-bivariate-sorts.qmd
	quarto render python/replicating-fama-and-french-factors.qmd
	quarto render python/fama-macbeth-regressions.qmd
	quarto render python/fixed-effects-and-clustered-standard-errors.qmd
	quarto render python/difference-in-differences.qmd
	quarto render python/factor-selection-via-machine-learning.qmd
	quarto render python/option-pricing-via-machine-learning.qmd
	quarto render python/parametric-portfolio-policies.qmd
	quarto render python/constrained-optimization-and-backtesting.qmd
