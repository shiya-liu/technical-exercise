# What are our overall success rates (C or better)?

offical_grade in course_erollments data set provides information of each student success, which can be used to evaluate overall success rates.


## Set up and load data

```r
rm(list =ls())
library(tidyverse)
load("D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data_230213_1514.Rdata")
```

## Calculate overall success rate
I transform the students' official_grade variable to the success variable. If students obtained C or better grade, the success variable was recorded as Y. Otherwise, this variable is N.

```r
course_enrollments %>%
 mutate(success = case_when(
   official_grade %in% c("A","A-","B+","B","B-","C") ~ "Y",
   TRUE ~ "N")) %>%
  count(success) %>%
  mutate(percent = 100*n/sum(n)) %>%
  select(-n)
```

```
  success  percent
1       N 40.16468
2       Y 59.83532
```
The overall success rate is 59.84%.
