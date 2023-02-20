# Introduction
The data are about ‘Smart Start’ Program  in the university to review entry level courses. First I review research questions, import and clean data, as well as describe the data and variables.




```r
rm(list =ls())
library(tidyverse)
library(dm)
```


# Research questions

1.What are our overall success rates (C or better)?

2.What student information helps predict success in their courses?

3.Are success rates improving or getting worse?

4.Are some of their instructors better than others? 

5.Does teaching frequently the course improve instructor performance over time?

6.How do their students, in their academic plans, perform compared to students from other departments/colleges?


# Description

## Data

It includes five tables of data: acad_plan, class_instructors, class_inventory, course_enrollments, and student_details. Those are relational data since they have relations so that they can connect with others via one or several variables. Before joining those tables into one data frame, I explore those tables first and keys.

### Load data



```r
acad_plan <- read.csv(file = "D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data/acad_plan.csv")
class_instructors <- read.csv(file = "D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data/class_instructors.csv")
class_inventory <- read.csv(file = "D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data/class_inventory.csv")
course_enrollments <- read.csv(file = "D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data/course_enrollments.csv")
student_details <- read.csv(file = "D:/R/data analysis/Institutional research/technical-exercise/content.en/docs/data/student_details.csv")
```

+  **acad_plan** describes detail information related to academic plan

```
  acad_plan              acad_plan_desc acad_career degree college dept_code
1    MS8894         Textiles & Clothing        GRAD  MSHCS     HSP        36
2    CTCBIG        Conservation Biology        GRAD            A&S        21
3    CTDIAG                    Diabetes        GRAD            HSP        81
4    CTNEDG Post-Masters Nurse Educator        GRAD            HSP        32
5    DP8140            Physical Therapy        GRAD    DPT     HSP        79
6    MA4221                   Economics        GRAD     MA     A&S        26
  acad_plan_type eff_status
1            MAJ          I
2            CRT          A
3            CRT          A
4            CRT          A
5            MAJ          A
6            MAJ          A
```

+ **class_instructors** contains instructor id, class number, and term code

```
  term_code class_nbr instructor_id_number
1         1      5241                  486
2         1      5242                 3234
3         1      5243                  486
4         1      5244                 2199
5         1      5245                 2199
6         1      5246                 2199
```

+ **class_inventory** includes detail information relate to class, such as term code, class number, etc.

```
  term_code semester subject catalog_nbr class_nbr class_description dept_code
1         1     Fall    BIOS         101      5350     Human Anatomy        21
2         1     Fall    SPAN         101      6171  Beginner Spanish        16
3         1     Fall    PHYS         101     11991   General Physics        22
4         1     Fall    PHYS         101      8405   General Physics        22
5         1     Fall    SPAN         101      6155  Beginner Spanish        16
6         1     Fall    SPAN         101      6188  Beginner Spanish        16
```

+ **course_enrollments** indicates student performance and course details

```
  student_id_number term_code class_nbr completed_flag official_grade acad_plan
1               150         1      9785              1              C    BS0411
2               150         3      5167              0           DROP    BS0411
3               150         3      5062              1              D    BS0411
4               150         4      4981              0           DROP    BS0411
5               150         4      4979              1             C-    BS0411
6             83679         4     10902              0             WP    ND1105
  hours_carried
1            11
2             9
3             9
4             8
5             8
6            14
```

+ **student_details** represents characteristics of students

```
  student_id_number ACT_score hs_gpa_entry hardship_score
1                 1        NA           NA              0
2                 2        NA           NA              0
3                 3        NA         1.03              0
4                 4        NA           NA              0
5                 5        NA           NA              0
6                 6        NA           NA              0
```







```r
df <- course_enrollments %>% 
  full_join(student_details, c("student_id_number")) %>%
  full_join(class_instructors, c("term_code", "class_nbr")) %>%
  full_join(class_inventory,c("term_code","class_nbr")) %>%
  full_join(acad_plan, c("acad_plan", "dept_code")) 
```



