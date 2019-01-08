Speed-Acc-Novel CogSci 2019
-----------------

There is one key RMarkdown files in the paper repository for "Children integrate social and statistical information during language processing"

* `san-cogsci-2019.Rmd`. It contains the paper writeup along with the R code for generating plots of the model-based analyses.

All figures in the paper can be reproduced using these RMarkdown files:

* E1: [`speed_acc_gaze_viz.Rmd`](https://github.com/kemacdonald/speed-acc-novel/blob/master/writing/figures/speed_acc_gaze_viz.Rmd)
* E2: [`speed_acc_novel_viz.Rmd`](https://github.com/kemacdonald/speed-acc-novel/blob/master/writing/figures/speed_acc_novel_viz.Rmd)

All statisical models reported in the paper can be found here:

* Experiment 1 (familiar words):
  - [Bayesian data analysis](https://github.com/kemacdonald/speed-acc-novel/blob/master/code/analysis/speed-acc-fam-bda.Rmd)
  - [Cluster-based permutation](https://github.com/kemacdonald/speed-acc-novel/blob/master/code/analysis/speed-acc-fam-permutation.Rmd)

Here is the session info for building the paper. You can use this information to create the necessary R environment (i.e., install packages) for knitting the manuscript. 

**R version 3.5.1 (2018-07-02)**

**Platform:** x86_64-apple-darwin17.6.0 (64-bit) 

**locale:**
en_US.UTF-8||en_US.UTF-8||en_US.UTF-8||C||en_US.UTF-8||en_US.UTF-8

**attached base packages:** 

* stats 
* graphics 
* grDevices 
* utils 
* datasets 
* methods 
* base 


**other attached packages:** 

* forcats(v.0.3.0) 
* stringr(v.1.3.1) 
* dplyr(v.0.7.6) 
* purrr(v.0.2.5) 
* readr(v.1.3.1) 
* tidyr(v.0.8.1) 
* tibble(v.2.0.0) 
* ggplot2(v.3.1.0) 
* tidyverse(v.1.2.1) 