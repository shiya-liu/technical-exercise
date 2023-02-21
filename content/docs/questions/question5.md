# Does teaching frequently the course improve instructor performance over time?

## Set up and load data




```r
rm(list =ls())
library(pacman)
p_load(tidyverse, ggplot2, broom, kableExtra)
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/questions/data_230214_1659.Rdata")
```

## Research question

**Does teaching frequently the course improve instructor performance over time?**

This question requires to explore if the instructor teaches more, their performance will be better. In other words, it examines relationships between number of terms they teach and success rates. 

The data set **class_instructors** has information related to instructor and courses they teach. The data set **course_enrollments** has information about students' success rates. 

Next, I combine those two data sets and conduct further analysis.

## Data  cleaning
1. Join two data sets: **class_instructors** and **course_enrollments** via term_code and class_nbr variables (drop the observations without grades)
2. Calculate how many terms per instructor teach
3. Calculate how many courses they teach 
4. Drop observations who teach more than six terms


```r
term <- course_enrollments %>%
  left_join(
    class_instructors, 
    by = c("term_code","class_nbr")
    ) %>%
  group_by(instructor_id_number) %>%
  summarise(
    total_student = n(),
    freq = sum(success == "Y"),
    percent = freq/total_student,
    total_term = n_distinct(term_code),
    total_course = n_distinct(class_nbr)
  ) %>%
  filter(total_term <= 6)
```


```
 instructor_id_number total_student          freq           percent      
 Min.   :  25.0       Min.   :   8.00   Min.   :  0.00   Min.   :0.0000  
 1st Qu.: 596.2       1st Qu.:  39.00   1st Qu.: 28.00   1st Qu.:0.6264  
 Median :1741.5       Median :  59.00   Median : 43.00   Median :0.7801  
 Mean   :1719.8       Mean   :  94.22   Mean   : 56.38   Mean   :0.7406  
 3rd Qu.:2678.5       3rd Qu.:  86.25   3rd Qu.: 63.75   3rd Qu.:0.8750  
 Max.   :3497.0       Max.   :2461.00   Max.   :852.00   Max.   :1.0000  
   total_term     total_course   
 Min.   :1.000   Min.   : 1.000  
 1st Qu.:1.000   1st Qu.: 2.000  
 Median :2.000   Median : 3.000  
 Mean   :2.385   Mean   : 3.661  
 3rd Qu.:3.000   3rd Qu.: 4.000  
 Max.   :6.000   Max.   :68.000  
```

## Visualization

<details><summary>Visualization code</summary>


```r
term %>%
  ggplot(aes(
    x = as.factor(total_term), 
    y = percent, 
    fill = as.factor(total_term)
    )) +
  geom_boxplot() +
  labs(
    x = "Number of terms",
    y = "Success rates",
    title = "Does teaching frequently the course improve instructor performance over term?"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(face = "bold", vjust = 0.9, size = 14),
    axis.title.x = element_text(face = "bold", vjust = 0.9, size = 14),
    axis.text = element_text(colour = "black", size = 10,),
    plot.title = element_text(face = "bold", size = 22),
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 10),
    plot.caption = element_text(size = 8)
  ) 
```

</details>



![](/images/p51.png)

The relationship between frequency of teaching and performance does not show the linear trend. That is to say, teaching frequently does not guarantee better performance. Instead, there is tipping point, which is five terms. From the box plot, if the instructor teaches less than six terms, the performance seems improve. However, for instructors who teach six terms, their performance lower than others.

## Statistical test
I first use welch test to see whether performance over those six groups has difference.

```r
oneway.test(percent~total_term, data = term,var.equal=FALSE)
```

```

	One-way analysis of means (not assuming equal variances)

data:  percent and total_term
F = 1.1229, num df = 5.00, denom df = 16.01, p-value = 0.3876
```
Overall, Welch test does not show there is statistically significant difference in this relationship. It might caused by a decrease in the performance of those who teach six terms. 

I conduct a simple linear regression to see whether the total terms are associated with performance.



```r
reg <- lm(percent~total_term + total_student + total_course, data = term)
broom::tidy(reg) %>%
  kable() %>% 
  kable_styling("basic", bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.6898860 </td>
   <td style="text-align:right;"> 0.0273790 </td>
   <td style="text-align:right;"> 25.1976361 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_term </td>
   <td style="text-align:right;"> 0.0428097 </td>
   <td style="text-align:right;"> 0.0129097 </td>
   <td style="text-align:right;"> 3.3160990 </td>
   <td style="text-align:right;"> 0.0011161 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_student </td>
   <td style="text-align:right;"> -0.0000454 </td>
   <td style="text-align:right;"> 0.0001457 </td>
   <td style="text-align:right;"> -0.3114527 </td>
   <td style="text-align:right;"> 0.7558381 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total_course </td>
   <td style="text-align:right;"> -0.0128816 </td>
   <td style="text-align:right;"> 0.0063804 </td>
   <td style="text-align:right;"> -2.0189424 </td>
   <td style="text-align:right;"> 0.0450645 </td>
  </tr>
</tbody>
</table>
P value of total_term is less than 0.05. That is to say, the number of terms instructors teach is related to their performance.

Therefore, based on the previous findings, I select instructors who teach less than six terms and to see whether there is difference.

Moreover, the number of courses they teach is negatively related to their performance.

### Further exploration


```r
term5 <- term %>%
  filter(total_term < 6)
```


```r
oneway.test(percent~total_term, data = term5,var.equal=FALSE)
```

```

	One-way analysis of means (not assuming equal variances)

data:  percent and total_term
F = 1.2719, num df = 4.000, denom df = 34.656, p-value = 0.2998
```
Welch test still does not significant. 


## Conclusion
While the box plot show a slight difference among instructors who teach different number of terms, the statistic tests indicate that those difference are not significant.

