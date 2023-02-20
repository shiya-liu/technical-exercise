# How do their students, in their academic plans, perform compared to students from other departments/colleges?

## Set up and load data




```r
rm(list =ls())
library(pacman)
p_load(tidyverse, ggplot2, broom, mice, kableExtra, data.table, ggrepel)
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/questions/data_230214_1659.Rdata")
```

## Research question
**How do their students, in their academic plans, perform compared to students from other departments/colleges?**

To compare students in those three academic plans (Physics, Biology (Human Anatomy), and Spanish) with others, I combine all data sets.
Then, I select academic plan from **acad_plan** data set and put student into two groups in the overall data set. 
Next, I compare those groups based on plot and welch test.


## Data cleaning
1. Combine all data sets and drop observations without official grades
2. Select academic plan from **acad_plan** data set
3. Put student into two groups based on their academic plan

```r
DF <- course_enrollments %>% 
  left_join(class_inventory, by = c("term_code","class_nbr")) %>%
  full_join(class_instructors, by = c("term_code", "class_nbr")) %>%
  full_join(acad_plan, c("acad_plan","dept_code")) %>%
  full_join(student_details, by = "student_id_number")
```


```r
cols_factor <- c("acad_plan","subject","acad_career","degree","college","eff_status","official_grade")
DF <- as.data.table(DF)
DF[,c(cols_factor) := lapply(.SD,as.factor), .SDcols = cols_factor]
```


```r
summary(DF)
```

```
 student_id_number   term_code       class_nbr     completed_flag 
 Min.   :    1     Min.   :1.00    Min.   : 4645   Min.   :0.00   
 1st Qu.:25821     1st Qu.:2.00    1st Qu.: 5249   1st Qu.:0.00   
 Median :50064     Median :3.00    Median : 6129   Median :1.00   
 Mean   :48958     Mean   :3.31    Mean   : 6661   Mean   :0.74   
 3rd Qu.:71402     3rd Qu.:5.00    3rd Qu.: 7517   3rd Qu.:1.00   
 Max.   :97112     Max.   :6.00    Max.   :15657   Max.   :1.00   
 NA's   :2719      NA's   :89714   NA's   :89714   NA's   :89759  
 official_grade    acad_plan     hours_carried   success     
 A      : 3483   ND1206 : 1359   Min.   : 3.00   N   : 6585  
 DROP   : 2713   ND1201 :  975   1st Qu.:15.00   Y   : 9810  
 B      : 1622   BS2127 :  929   Median :16.00   NA's:89759  
 A-     : 1566   BS2121 :  686   Mean   :15.63               
 B+     : 1394   BS7257 :  611   3rd Qu.:17.00               
 (Other): 5617   (Other):14509   Max.   :27.00               
 NA's   :89759   NA's   :87085   NA's   :89759               
   semester         subject       catalog_nbr    class_description 
 Length:106154      BIOS: 5370   Min.   :101     Length:106154     
 Class :character   PHYS: 3535   1st Qu.:101     Class :character  
 Mode  :character   SPAN: 7490   Median :101     Mode  :character  
                    NA's:89759   Mean   :101                       
                                 3rd Qu.:101                       
                                 Max.   :101                       
                                 NA's   :89759                     
   dept_code     instructor_id_number acad_plan_desc     acad_career  
 Min.   : 0.00   Min.   :  25         Length:106154      GRAD:  1160  
 1st Qu.:16.00   1st Qu.: 596         Class :character   MED :     5  
 Median :21.00   Median :2165         Mode  :character   UGRD:  3403  
 Mean   :21.47   Mean   :1815                            NA's:101586  
 3rd Qu.:22.00   3rd Qu.:2696                                         
 Max.   :92.00   Max.   :3497                                         
 NA's   :87336   NA's   :89714                                        
     degree          college       acad_plan_type     eff_status   
 BS     :  1863   A&S    :  2381   Length:106154      A   :  3041  
        :   919   EHS    :   507   Class :character   I   :  1527  
 BA     :   201   HSP    :   475   Mode  :character   NA's:101586  
 BSED   :   159   FAR    :   263                                   
 MED    :   117   COM    :   237                                   
 (Other):  1309   (Other):   705                                   
 NA's   :101586   NA's   :101586                                   
   ACT_score      hs_gpa_entry   hardship_score  
 Min.   : 0.00   Min.   :0.21    Min.   :0.0000  
 1st Qu.:20.00   1st Qu.:3.07    1st Qu.:0.0000  
 Median :23.00   Median :3.43    Median :0.0000  
 Mean   :23.05   Mean   :3.39    Mean   :0.6612  
 3rd Qu.:26.00   3rd Qu.:3.76    3rd Qu.:1.0000  
 Max.   :36.00   Max.   :5.00    Max.   :3.0000  
 NA's   :42584   NA's   :37393   NA's   :2719    
```

```r
DF <- DF%>%
  filter(!is.na(official_grade))
```
Notice that there are some rows in the data set with NA values for acad_career, degree, college, etc. However, since we want to focus on comparing success rates between three academic plans and others, we can ignore those missing values.


```r
plan <- acad_plan %>% 
  filter(acad_plan_desc %in% c("Bios Human Biology","Physics","Spanish")) 
DF <- DF %>%
  mutate(
    treat = case_when(
      acad_plan %in% plan$acad_plan~ "Y",
      is.na(acad_plan) ~ NA_character_,
      TRUE ~ "N"
    )
  )
DF$treat <- as.factor(DF$treat)
```

## Visualization

<details><summary>Visualization code</summary>

```r
DF %>%
  select(c("treat","success")) %>%
  group_by(treat) %>%
  count(success) %>%
  mutate(percent = round(n/sum(n),3)) %>%
  filter(success == "Y") %>%
  ggplot(aes(x = treat, y = percent, fill = treat)) +
  geom_bar(width = 0.5, position = position_dodge(0.5), stat = "identity") +
  geom_text(aes(label = percent), size = 6, vjust = -.8) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
  scale_x_discrete(
    limits =c("Y","N"),
    labels = c("Physics, Biology, and Spanish", "Others")) +
  labs(
    y = "Success rate",
    x = "Department",
    title = "Do students in Physics, Biology, and Spanish has higher success rate?"
  ) +
  theme_classic() +
  theme(
        legend.position = "none",
        axis.title.y = element_text(face = "bold", vjust = 0.9, size = 16),
        axis.title.x = element_text(face = "bold", vjust = 0.9, size = 16),
        axis.text = element_text(colour = "black", size = 14),
        plot.title = element_text(face = "bold", size = 20)
  ) 
```
</details>



![](/images/p3.png)
The plot shows that success rates of student in those three academic plans (Physics, Biology (Human Anatomy), and Spanish) are different with others. To have strong evidence, I conduct Welch test to compare those groups.

## Welch test


```r
compare <- DF %>%
  select(c("success", "treat")) %>%
  drop_na()
compare$success <- as.numeric(compare$success)
oneway.test(success~treat, data = compare,var.equal=FALSE)
```

```

	One-way analysis of means (not assuming equal variances)

data:  success and treat
F = 3.9056, num df = 1.000, denom df = 90.957, p-value = 0.05116
```

## Conclusion
While the plot indicates success rates of those two groups are different, the Welch test shows that those difference are not statistically significant.

