--- 
title: "Tidy Finance"
author: "Christoph Scheuch, Patrick Weiss and Stefan Voigt"
date: "2022-01-18"
site: bookdown::bookdown_site
output: bookdown::bs4_book
documentclass: book
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
github-repo: voigtstefan/tidy_finance
description: "Tidy Finance with R"
---

# Prerequisites

You are very welcome to give us feedback on *every* aspect of the book such that we can improve the codes, explanations and general structure. Please contact us at [contact@tidy-finance.com](mailto:contact@tidy-finance.org) if you spot typos, mistakes or other issues that deserve more attention. 

## Things to get done before the first lecture

To dive right into it, there are a couple of prerequisites you should get done before the first lecture:

- [Install R and RStudio](https://rstudio-education.github.io/hopr/starting.html#starting). To get a walk-through of the installation (for every major operating system), follow the steps outlined [in this summary](https://rstudio-education.github.io/hopr/starting.html#starting). Should be done in few clicks. If you wonder about the difference: R is an open-source  language and environment for statistical computing and graphics, free to download and use. While R runs computations, RStudio is an integrated development environment (IDE) that provides an interface by adding many convenient features and tools. I suggest you do all coding exclusively with RStudio during the course (and beyond).
- Open RStudio and  run  the  following  line  of  code: `install.packages("tidyverse")`. Not sure how it works? You find helpful information on how to install packages in this [brief summary](https://rstudio-education.github.io/hopr/packages2.html). The tidyverse is “a framework for managing data that aims at making the cleaning and preparing steps much easier”. We are going to work almost exclusively with tidyverse packages.
- Check if everything works: Open Rstudio and type `library(tidyverse)`. You should then get a message like in the picture below.  Works? Done with the software setup!  

## What is R?

In this course we work with R. Together with Python, R is nowadays the de facto standard tool in finance and is unparalleled when it comes to handle data science applications.

*But so far I learned to code with Python, Ox, Matlab, …*

Excellent if you have prior knowledge in another coding language. Your start with R will be way easier. Also, as a Python user you can actually run your Python code in R (and vice versa) - so if you have already written some analysis for a prior project, no need to translate everything! If you are interested to do this at some point in time, check out this resource .

*I have no experience whatsoever with programming*

Great, you nevertheless make the bar: We will start entirely from scratch (but with a really steep learning curve). Besides the applications in Finance, we will focus on state-of-the-art data science concepts that cover the entire life cycle of a successful empirical analysis. No matter if we consider a research project, a thesis, seminar paper or a thorough portfolio back testing procedure at an investment bank: the challenges are always similar: How do we get R connected to our datasets? How can we clean the data such that our analysis makes sense? How to visualize or change variables in order to estimate parameters or make financial decisions? And, finally, how can we communicate our results such that we are sure that our analysis is reliable, correct and flexible enough to quickly find out what happens if we have to change some input parameters.

*This sounds like a lot of work on the data science front. How do I get started?*

There are a couple of ways for you to get started, below a list with relevant material and suggestions.

1. Only way to learn how to code is by getting your hands dirty. Make sure your system is ready to go by completing all technical requirements.
1. A very gentle and good introduction into the workings of R can be found [here](https://rstudio-education.github.io/hopr/project-1-weighted-dice.html). Once you are done with setting up R on your machine, try to follow the "weighted dice project".
1. The main book on the tidyverse is available online and for free: [R for Data Science](https://r4ds.had.co.nz/introduction.html) by Hadley Wickham and Garrett Grolemund explains everything we need. 
