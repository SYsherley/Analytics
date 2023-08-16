Behavioral Data Analysis
================
SYsherley
latest: 2023-08-16

### 

This is an R Markdown file. It includes a simplified procedure of
behavioral data analysis.<br/> The dataset shown in this file includes
naming latencies extracted from speech voices recorded in a language
production experiment.<br/> The dataset has been cleaned, removed
outliers. <br/> We need to calculate the mean naming latencies for each
condition (4 in total), make violin plots, and develop GLM.

### Analysis procedure:

1.  Import libraries
2.  Load dataset
3.  Average values
4.  Visualization
5.  Generalized Linear Mixed effects Model

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.1     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)     
library(Hmisc) 
```

    ## 
    ## Attaching package: 'Hmisc'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(tibble) 
library(ggplot2)   
library(ggpubr)    # enhance ggplot2 visualizations
library(hrbrthemes)# additional themes and styling options for ggplot2 
```

    ## NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use these themes.
    ##       Please use hrbrthemes::import_roboto_condensed() to install Roboto Condensed and
    ##       if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow

``` r
library(ggsignif)  # significance brackets to ggplot2 plots
library(languageR) # statistical analysis of linguistic data
library(readxl)    # read Microsoft Excel files (.xls and .xlsx) into R as data frames
library(ez)        # analyze and visualize data from factorial experiments 
library(forcats)   # reorder factor levels 
library(lme4)      # linear mixed-effects models
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(QuantPsyc) # quantitative psychological analyses
```

    ## Loading required package: boot
    ## Loading required package: MASS
    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select
    ## 
    ## 
    ## Attaching package: 'QuantPsyc'
    ## 
    ## The following object is masked from 'package:Matrix':
    ## 
    ##     norm
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     norm

``` r
library(sjmisc)    # missing values
```

    ## 
    ## Attaching package: 'sjmisc'
    ## 
    ## The following object is masked from 'package:Hmisc':
    ## 
    ##     %nin%
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_empty
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     replace_na
    ## 
    ## The following object is masked from 'package:tibble':
    ## 
    ##     add_case

``` r
library(kableExtra)# pack_rows
```

    ## 
    ## Attaching package: 'kableExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(car)       # regression analysis and model diagnostics
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:boot':
    ## 
    ##     logit
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(palmerpenguins) # visual
library(viridis)   # color scales
```

    ## Loading required package: viridisLite

``` r
library(cowplot)   # save figures in .png file
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:ggpubr':
    ## 
    ##     get_legend
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(sjPlot)    # tab_model()
```

    ## 
    ## Attaching package: 'sjPlot'
    ## 
    ## The following objects are masked from 'package:cowplot':
    ## 
    ##     plot_grid, save_plot

``` r
library(sjlabelled)
```

    ## 
    ## Attaching package: 'sjlabelled'
    ## 
    ## The following object is masked from 'package:forcats':
    ## 
    ##     as_factor
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     as_label
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     as_label

#### Load dataset

``` r
RTdata<-read_excel("RT.xls") 

head(RTdata, 3) # show 3 rows
```

    ## # A tibble: 3 × 18
    ##      Nr Determiner Item   G     S     Condition Marker Picture     RT SubjectNr.
    ##   <dbl> <chr>      <chr>  <chr> <chr> <chr>      <dbl> <chr>    <dbl> <chr>     
    ## 1    33 De         tafel  Inc   Rel   c2            52 table.j…  1.26 P01       
    ## 2    12 De         rivier Con   Rel   c1           121 river.j…  1.1  P01       
    ## 3    17 Het        oog    Con   Rel   c1           171 eye.jpg   1.10 P01       
    ## # ℹ 8 more variables: LexTALE <dbl>, AOA <dbl>, UseYear <dbl>, SelfRate <dbl>,
    ## #   AcqRoman <dbl>, AcqGerm <dbl>, AcqAsian <dbl>, OrderofLearning <dbl>

#### Average value

``` r
# Average of the 4 conditions
tapply(RTdata$RT,RTdata$Condition, mean)*1000
```

    ##       c1       c2       c3       c4 
    ## 1065.393 1079.983 1025.531 1035.525

``` r
tapply(RTdata$RT,RTdata$Condition, sd)*1000  # SD
```

    ##       c1       c2       c3       c4 
    ## 364.7251 369.5279 335.5612 353.8405

``` r
# Average of the G condition (with 2 levels)
tapply(RTdata$RT,RTdata$G, mean)*1000
```

    ##      Con      Inc 
    ## 1045.369 1057.393

``` r
tapply(RTdata$RT,RTdata$G, sd)*1000 # SD
```

    ##      Con      Inc 
    ## 350.7415 362.1150

We found that naming latencies are faster in the Con condition compared
to the Inc condition. The estimated difference is approximately 12 ms.

``` r
# Average of the S condition (with 2 levels)
tapply(RTdata$RT,RTdata$S, mean)*1000
```

    ##      Rel       Un 
    ## 1072.663 1030.568

``` r
tapply(RTdata$RT,RTdata$S, sd)*1000 # SD
```

    ##      Rel       Un 
    ## 366.9824 344.7333

Here, naming latencies in the Rel condition were longer compared to the
Un condition. The exitmated difference is around 52 ms.

#### Plot

``` r
# prepare
RTdata_2<-RTdata # create a new dataframe to plot
RTdata_2$RT<-RTdata_2$RT*1000 # ms
RTdata_2$Condition <- factor(RTdata_2$Condition, levels = c("c1","c2","c3","c4"))

RTdata_2$S_status<-RTdata_2$S
RTdata_2$S_status<-as.character(RTdata_2$S_status)
RTdata_2$S_status[RTdata_2$S_status =="Rel"] <-"S+" # rename
RTdata_2$S_status[RTdata_2$S_status =="Un"] <-"S-"
RTdata_2$S_status<-as.factor(RTdata_2$S_status) 

RTdata_2$G_status<-RTdata_2$G
RTdata_2$G_status<-as.character(RTdata_2$G_status)
RTdata_2$G_status[RTdata_2$G_status =="Con"] <-"G+"
RTdata_2$G_status[RTdata_2$G_status =="Inc"] <-"G-"
RTdata_2$G_status<-as.factor(RTdata_2$G_status) 
```

``` r
levels(RTdata_2$S_status) # should be 2 levels
```

    ## [1] "S-" "S+"

``` r
levels(RTdata_2$G_status)
```

    ## [1] "G-" "G+"

``` r
RTdata_2<-RTdata_2 %>%
  mutate(day = fct_reorder(G_status, RT)) %>%
  mutate(day = factor(G_status, levels=c("G+", "G-"))) 

RTdata_2<-RTdata_2 %>%
  mutate(day = fct_reorder(S_status, RT)) %>%
  mutate(day = factor(S_status, levels=c("S+", "S-"))) 
```

## Including Plots

You can also embed plots, for example:

![](R_behavioral_project_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
