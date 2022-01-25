--- 
title: "Tidy Finance"
author: "Christoph Scheuch, Patrick Weiss and Stefan Voigt"
date: "2022-01-25"
site: bookdown::bookdown_site
output: bookdown::bs4_book
documentclass: book
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
github-repo: voigtstefan/tidy_finance
description: "Tidy Finance with R"
---

# Welcome

This is the online version of *Tidy Finance with R*, a book currently under development and intended for eventual print release. We are grateful for any kind of feedback on *every* aspect of the book. So please get in touch with us at [contact@tidy-finance.com](mailto:contact@tidy-finance.org) if you spot typos, discover any issues that deserve more attention, or even if you have suggestions for additional chapters and sections. 

## Motivation

Finance is an exciting area of economic research with a broad range of applied and academic empirical applications. As a undergrad student, you are typically exposed to different types of financial data, ranging from asset prices, accounting data, trading decisions to all kinds of other financial data. Despite the vast number of empirical studies of financial phenomenons, students quickly learn that the actual implementation of these studies is rather opaque. As graduate students, also we ourselves were surprised by the lack of public code for seminal papers or even textbooks on key insights of financial economics. The lack of transparent codes not only leads to numerous replication efforts (and their failures), but is also a waste of resources on problems that have already been solved by others. 

This book aims to lift this curtain on code in finance by providing a fully transparent code base for many common financial applications. Our target group comprises of students, data analysts and other programmers who want to gain the basic tools required for financial research and apply them to the challenges they face. The materials in this book can hence be used for courses in empirical finance, as well as to study empirical finance on your own. 

## Why R?

We believe that `R` is among best choices for a programming language in the area of finance. Some of our favorite features include:

- `R` is free and open source, so you can use it in academic and professional contexts
- A diverse and active online community working on a broad range of tools
- A massive set of actively maintained packages for all kinds of applications, e.g. data manipulation, visualization, machine learning, etc.
- Powerful tools for communication, e.g. Rmarkdown, shiny
- RStudio as one of the best development environments for interactive data analysis
- Strong foundation of functional programming
- Smooth integration with other programming languages, e.g., SQL, Python, C, C++, Fortran, etc.

For more information, we refer to  [@Wickham2019](https://adv-r.hadley.nz/introduction.html).

## Why tidy?

As you start working with data, you quickly realize that you spend a lot of time reading, cleaning and transforming your data. In fact, it is often said that more than 80% of data analysis is spent on preparing data. By **tidying data**, we want to structure datasets to facilitate further analyses. As [@Wickham2014] puts it, 

>[T]idy datasets are all alike but every messy dataset is messy in its own way. Tidy datasets provide a standardized way to link the structure of a dataset (its physical layout) with its semantics (its meaning). 

In its essence, tidy data follows these three principles:

1. Every column is a variable.
2. Every row is an observation.
3. Every cell is a single value.

Throughout this book, we try our best to follow these principles. If you want to learn more about tidy data principles in an informal manner, we refer you to [this vignette](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html).

In addition to the data layer, there are also tidy coding principles outlined by [INSERT CITATION](https://tidyverse.tidyverse.org/articles/manifesto.html) that we try to follow: 

1. Reuse existing data structures.
2. Compose simple functions with the pipe.
3. Embrace functional programming.
4. Design for humans.

In particular, we heavily draw on a set of packages called the [`tidyverse`](https://tidyverse.tidyverse.org/index.html). The `tidyverse` is a consistent set of packages for all data analysis tasks, ranging from importing, wrangling to visualizing and modeling data with the same grammar. In addition to explicit tidy principles, the `tidyverse` has further benefits: (i) if you master one package, it is easier to master others and (ii) the core packages are developed and maintained by the Public Benefit Company RStudio, Inc. 

## License

This book is licensed to you under [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).

The code samples in this book are licensed under [Creative Commons CC0 1.0 Universal (CC0 1.0), i.e. public domain](https://creativecommons.org/publicdomain/zero/1.0/).

## Prerequisites

Before you work through the chapters, we recommend to perform the following couple of steps:

- [Install R and RStudio](https://rstudio-education.github.io/hopr/starting.html#starting). To get a walk-through of the installation (for every major operating system), follow the steps outlined [in this summary](https://rstudio-education.github.io/hopr/starting.html#starthng). The whole process should be done in few clicks. If you wonder about the difference: R is an open-source language and environment for statistical computing and graphics, free to download and use. While R runs the computations, RStudio is an integrated development environment (IDE) that provides an interface by adding many convenient features and tools. We hence suggest to do all the coding in RStudio.
- Open RStudio and [install the `tidyverse`](https://tidyverse.tidyverse.org/). Not sure how it works? You find helpful information on how to install packages in this [brief summary](https://rstudio-education.github.io/hopr/packages2.html). 

If you are new to `R`, we recommend to start with the following sources:
- A very gentle and good introduction into the workings of R can be found [here](https://rstudio-education.github.io/hopr/project-1-weighted-dice.html). Once you are done with setting up R on your machine, try to follow the "weighted dice project".
- The main book on the `tidyverse` is available online and for free: [R for Data Science](https://r4ds.had.co.nz/introduction.html) by Hadley Wickham and Garrett Grolemund explains the majority of the tools we use in this book. 
