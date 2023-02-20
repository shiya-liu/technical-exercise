# Set up

```r
rm(list =ls())
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.1     ✔ purrr   1.0.1
## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
## ✔ readr   2.1.3     ✔ forcats 1.0.0
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(dm)
```

```
## 
## Attaching package: 'dm'
## 
## The following object is masked from 'package:stats':
## 
##     filter
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
## 
## The following object is masked from 'package:purrr':
## 
##     transpose
```

```r
library(mice)
```

```
## 
## Attaching package: 'mice'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
```


# Load data

```r
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/data_230213_2316.Rdata")
```

# Add succss variables to evaluate student success
+ C or better is Y, otherwise is N
+ a factorial variable





```r
mydf <- df %>%
 mutate(success = case_when(
   official_grade %in% c("A","A-","B+","B","B-","C") ~ "Y",
   TRUE ~ "N"))
```

# Convert characterial variables
+ 

```r
str(mydf)
```

```
## 'data.frame':	106154 obs. of  23 variables:
##  $ student_id_number   : int  150 150 150 150 150 83679 306 89384 198 90433 ...
##  $ term_code           : int  1 3 3 4 4 4 2 6 3 5 ...
##  $ class_nbr           : int  9785 5167 5062 4981 4979 10902 9445 14391 15293 7545 ...
##  $ completed_flag      : int  1 0 1 0 1 0 0 1 0 1 ...
##  $ official_grade      : chr  "C" "DROP" "D" "DROP" ...
##  $ acad_plan           : chr  "BS0411" "BS0411" "BS0411" "BS0411" ...
##  $ hours_carried       : num  11 9 9 8 8 14 15 9 9 7 ...
##  $ ACT_score           : int  NA NA NA NA NA 26 17 28 15 31 ...
##  $ hs_gpa_entry        : num  NA NA NA NA NA ...
##  $ hardship_score      : int  2 2 2 2 2 3 3 0 0 0 ...
##  $ instructor_id_number: int  940 2199 486 486 486 2562 2473 119 548 3481 ...
##  $ semester            : chr  "Fall" "Fall" "Fall" "Spring" ...
##  $ subject             : chr  "PHYS" "BIOS" "BIOS" "BIOS" ...
##  $ catalog_nbr         : int  101 101 101 101 101 101 101 101 101 101 ...
##  $ class_description   : chr  "General Physics" "Human Anatomy" "Human Anatomy" "Human Anatomy" ...
##  $ dept_code           : int  22 21 21 21 21 22 16 16 16 16 ...
##  $ acad_plan_desc      : chr  NA "Microbiology" "Microbiology" "Microbiology" ...
##  $ acad_career         : chr  NA "UGRD" "UGRD" "UGRD" ...
##  $ degree              : chr  NA "BS" "BS" "BS" ...
##  $ college             : chr  NA "A&S" "A&S" "A&S" ...
##  $ acad_plan_type      : chr  NA "MAJ" "MAJ" "MAJ" ...
##  $ eff_status          : chr  NA "A" "A" "A" ...
##  $ success             : chr  "Y" "N" "N" "N" ...
```

```r
cols_factor <- c("acad_plan","semester","subject","acad_career","degree","college","acad_plan_type","eff_status","success")
mydf <- as.data.table(mydf)
mydf[,c(cols_factor) := lapply(.SD,as.factor), .SDcols = cols_factor]
str(mydf)
```

```
## Classes 'data.table' and 'data.frame':	106154 obs. of  23 variables:
##  $ student_id_number   : int  150 150 150 150 150 83679 306 89384 198 90433 ...
##  $ term_code           : int  1 3 3 4 4 4 2 6 3 5 ...
##  $ class_nbr           : int  9785 5167 5062 4981 4979 10902 9445 14391 15293 7545 ...
##  $ completed_flag      : int  1 0 1 0 1 0 0 1 0 1 ...
##  $ official_grade      : chr  "C" "DROP" "D" "DROP" ...
##  $ acad_plan           : Factor w/ 2536 levels "AA1100","AA1101",..: 522 522 522 522 522 1772 822 1778 2181 826 ...
##  $ hours_carried       : num  11 9 9 8 8 14 15 9 9 7 ...
##  $ ACT_score           : int  NA NA NA NA NA 26 17 28 15 31 ...
##  $ hs_gpa_entry        : num  NA NA NA NA NA ...
##  $ hardship_score      : int  2 2 2 2 2 3 3 0 0 0 ...
##  $ instructor_id_number: int  940 2199 486 486 486 2562 2473 119 548 3481 ...
##  $ semester            : Factor w/ 2 levels "Fall","Spring": 1 1 1 2 2 2 2 2 1 1 ...
##  $ subject             : Factor w/ 3 levels "BIOS","PHYS",..: 2 1 1 1 1 2 3 3 3 3 ...
##  $ catalog_nbr         : int  101 101 101 101 101 101 101 101 101 101 ...
##  $ class_description   : chr  "General Physics" "Human Anatomy" "Human Anatomy" "Human Anatomy" ...
##  $ dept_code           : int  22 21 21 21 21 22 16 16 16 16 ...
##  $ acad_plan_desc      : chr  NA "Microbiology" "Microbiology" "Microbiology" ...
##  $ acad_career         : Factor w/ 3 levels "GRAD","MED","UGRD": NA 3 3 3 3 NA NA NA NA NA ...
##  $ degree              : Factor w/ 169 levels "","AA","AAB",..: NA 48 48 48 48 NA NA NA NA NA ...
##  $ college             : Factor w/ 16 levels "","A&S","COB",..: NA 2 2 2 2 NA NA NA NA NA ...
##  $ acad_plan_type      : Factor w/ 8 levels "CON","CRT","END",..: NA 5 5 5 5 NA NA NA NA NA ...
##  $ eff_status          : Factor w/ 2 levels "A","I": NA 1 1 1 1 NA NA NA NA NA ...
##  $ success             : Factor w/ 2 levels "N","Y": 2 1 1 1 1 1 1 2 1 2 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

# Deal with extreme cases

```r
student_details <- student_details %>%
  mutate(
    hs_gpa_entry = case_when(
      hs_gpa_entry <= 5 ~hs_gpa_entry, 
      TRUE ~ NA
    )
  )
```


# Imputate some variables

```r
vars <- c("student_id_number",
          "term_code",
          "class_nbr",
          "acad_plan",
          "hours_carried",
          "ACT_score",
          "hs_gpa_entry",
          "hardship_score",
          "instructor_id_number",
          "subject",
          "dept_code",
          "acad_career",
          "degree",
          "college",
          "acad_plan_type",
          "eff_status",
          "success")
DF <- mydf[,..vars]
```


# Save data

```r
dt <- format(Sys.time(),"%y%m%d_%H%M")
fileName <- paste0("data_", dt, ".Rdata",sep="")
fileName <- "data_230214_1659.Rdata"
save.image(fileName)
```

