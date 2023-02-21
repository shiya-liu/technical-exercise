# What student information helps predict success in their courses?



## Set up and load data


```r
rm(list =ls())
library(pacman)
p_load(tidyverse, broom, ggplot2, kableExtra, interactions)
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/data_230220_2216.Rdata")
```

## Research question
**What student information helps predict success in their courses?**

Success in course can be found in the **course_enrollments** data set and be defined as C or better score in their official grade.

The student information can be obtained from **student_details** data set, which include information like **student_id_number**, **ACT_score**, **hs_gpa_entry**, and **hardship_score**. 

## Data cleaning
I conduct following steps to clean data:
1. Combine **student_details** and **course_enrollments** data set via student_id_number variable.
2. Deal with missing values and extreme values



```r
DF <- student_details %>%
  full_join(course_enrollments, by="student_id_number") %>%
  select(c("student_id_number","ACT_score","hs_gpa_entry","hardship_score","success"))
```


```
 student_id_number   ACT_score      hs_gpa_entry    hardship_score  
 Min.   :    1     Min.   : 0.00   Min.   :  0.21   Min.   :0.0000  
 1st Qu.:25821     1st Qu.:20.00   1st Qu.:  3.07   1st Qu.:0.0000  
 Median :50064     Median :23.00   Median :  3.43   Median :0.0000  
 Mean   :48958     Mean   :23.05   Mean   :  3.40   Mean   :0.6612  
 3rd Qu.:71402     3rd Qu.:26.00   3rd Qu.:  3.76   3rd Qu.:1.0000  
 Max.   :97112     Max.   :36.00   Max.   :553.00   Max.   :3.0000  
                   NA's   :39865   NA's   :34588                    
 success     
 N   : 6585  
 Y   : 9810  
 NA's:87040  
             
             
             
             
```
### Deal with missing values and extreme values
After summarizing the **DF** data set, we can find there are extreme values in **hs_gpa_entry** (e.g., it has 553), and missing values in several variables (**ACT_score**, **hs_gpa_entry**, and **success**).

1. Extreme values

For extreme values in **hs_gpa_entry**, I record them as NA since I do not have information about how the data are collected.

```r
DF <- DF %>%
  mutate(
    hs_gpa_entry = case_when(
      hs_gpa_entry <= 5 ~hs_gpa_entry, 
      TRUE ~ NA
    )
  )
```

2. Missing values

After combining two data sets, the new data set: **DF** has several variables (**ACT_score**, **hs_gpa_entry**, and **success**) with missing values, which might caused by joining two data sets. **student_details** data set has some students that are not included in the **course_enrollments** data set. 
I first examine the **course_enrollments** data set to see whether it has missing value.




```r
summary(course_enrollments)
```

```
 student_id_number   term_code       class_nbr     completed_flag  
 Min.   :  150     Min.   :1.000   Min.   : 4645   Min.   :0.0000  
 1st Qu.:50065     1st Qu.:2.000   1st Qu.: 5249   1st Qu.:0.0000  
 Median :55538     Median :3.000   Median : 6129   Median :1.0000  
 Mean   :55939     Mean   :3.311   Mean   : 6639   Mean   :0.7428  
 3rd Qu.:60401     3rd Qu.:5.000   3rd Qu.: 7515   3rd Qu.:1.0000  
 Max.   :95213     Max.   :6.000   Max.   :15323   Max.   :1.0000  
                                                                   
 official_grade  acad_plan         hours_carried   success 
 A      :3483   Length:16395       Min.   : 3.00   N:6585  
 DROP   :2713   Class :character   1st Qu.:15.00   Y:9810  
 B      :1622   Mode  :character   Median :16.00           
 A-     :1566                      Mean   :15.63           
 B+     :1394                      3rd Qu.:17.00           
 B-     : 923                      Max.   :27.00           
 (Other):4694                                              
```
All students in this data set has official grades. So, in the joined data set **DF**, I drop those observations without official grades (i.e. NA in **success** variable).


```r
DF <- DF %>%
  filter(!is.na(success))
```

## Visualization
Through visualization, I plan to examine difference on ACT_score, hs_gpa_entry, and hardship_score between the groups of students who have C or better and others.
-   ACT_score


```r
DF %>%
  ggplot(aes(x = success, y = ACT_score)) +
  geom_boxplot()+
  labs(
    x = "Success",
    y = "ACT score",
    title = ""
  ) +
  theme_classic()
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-9-1.png" width="672" />

-   hs_gpa_entry


```r
DF %>%
  ggplot(aes(x = success, y = hs_gpa_entry)) +
  geom_boxplot()+
  labs(
    x = "Success",
    y = "HS GPA entry",
    title = ""
  ) +
  theme_classic()
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-10-1.png" width="672" />

-   hardship_score


```r
DF %>%
  ggplot(aes(x = success, y = hardship_score)) +
  geom_boxplot()+
  labs(
    x = "Success",
    y = "Hardship score",
    title = ""
  ) +
  theme_classic()
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-11-1.png" width="672" />

Three box plots do not show huge differences on ACT_score, hs_gpa_entry, and hardship_score between students succeed and others.

Next, I conduct logistic regression to examine how those three variables predict student success rates.

## Logistic regression
1. Run basic logistic regression model
2. Evaluate potential interactions
3. Run model with interactions
4. Compare two models



### Basic Model
First, I run the basic logistic regression model with three independent variables (**ACT_score**, **hs_gpa_entry**,**hardship_score**) without considering interaction.


```r
DF$success <- relevel((DF$success), ref = "N")
```


```r
formula <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score)
lr <- glm(formula, data = DF, family = binomial(link = "logit"))
```

When we interpret the results, we look at the odds ratio.

-   Odds ratio is an measure of association between dependent variable and independent variables in the logistic regression. It estimates the change in the odd of membership in the target group for one unit increase in the predictor.

-  Odds ratio > 1 indicates the condition or event is more likely to occur in the first group. In our case, it means positive relationship between the odds of succeed and the independent variable. 

-  Odds ratio < 1 indicates that the condition or event is less likely to occur in the first group, which refers negative relationship between the odds of succeed and the independent variable. 


```r
broom::tidy(lr, exp = TRUE) %>%
  kable(
    col.names = c("Term","Odds ratio","Standard error","Z value", "P value")
  ) %>% 
  kable_styling("basic", bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Odds ratio </th>
   <th style="text-align:right;"> Standard error </th>
   <th style="text-align:right;"> Z value </th>
   <th style="text-align:right;"> P value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 1.7383189 </td>
   <td style="text-align:right;"> 0.1472242 </td>
   <td style="text-align:right;"> 3.755623 </td>
   <td style="text-align:right;"> 0.0001729 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9352319 </td>
   <td style="text-align:right;"> 0.0050686 </td>
   <td style="text-align:right;"> -13.210965 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.5638112 </td>
   <td style="text-align:right;"> 0.0412348 </td>
   <td style="text-align:right;"> 10.843423 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.8615343 </td>
   <td style="text-align:right;"> 0.0147317 </td>
   <td style="text-align:right;"> -10.116963 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
</tbody>
</table>

The results show that all three independent variables are statistically significant associated with the dependent variable (**success**).

-   ACT_score: After controlling all other variables, the odds of succeed **decrease** as ACT score increases.

-   hs_gpa_entry: After controlling all other variables, the odds of succeed **increase** as hs_gpa_entry increases.

-   hardship_score: After controlling all other variables, the odds of succeed **decrease** as hardship score increases.

### Add Interactions

Intuitively, those three variables might interact with each other when predicting **success** variable. To verify it, I first examine the interactions. Next, based on the results, I choose the interactions to add in the model.

<details><summary>Evaluate interactions</summary>

-   ACT_score*hardship_score


```r
formulaAH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score)
lrAH <- glm(formulaAH, data = DF, family = binomial(link = "logit"))
sim_slopes(lrAH, pred = ACT_score, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is INSIDE the interval [-55.45, 5.21], the slope of
ACT_score is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-15-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of ACT_score when hardship_score = -0.2902732 (- 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.07   0.01   -11.00   0.00

Slope of ACT_score when hardship_score =  0.8411562 (Mean): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.07   0.01   -13.16   0.00

Slope of ACT_score when hardship_score =  1.9725856 (+ 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.06   0.01    -8.45   0.00
```

From the plot, we can see that for ACT_score, the slope of hardship_score is significantly different from zero and in this case positive.

-   hs_gpa_entry*hardship_score


```r
formulaHH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + hs_gpa_entry*hardship_score)
lrHH <- glm(formulaHH, data = DF, family = binomial(link = "logit"))
sim_slopes(lrHH, pred = hs_gpa_entry, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is INSIDE the interval [-3.28, 18.44], the slope of
hs_gpa_entry is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-16-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of hs_gpa_entry when hardship_score = -0.2902732 (- 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.40   0.06     7.06   0.00

Slope of hs_gpa_entry when hardship_score =  0.8411562 (Mean): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.45   0.04    10.79   0.00

Slope of hs_gpa_entry when hardship_score =  1.9725856 (+ 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.49   0.05     8.96   0.00
```

For hs_gpa_entry, the slope of hardship_score is also significantly different from zero and positive. 

Overall, those two interactions are significant. So, I include these interactions and compare the new model with the previous basic model.
</details>


<details><summary>Run model with interactions</summary>


```r
formulaFull<- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score + hs_gpa_entry*hardship_score)
lrFull <- glm(formulaFull, data = DF, family = binomial(link = "logit"))
```



```r
broom::tidy(lrFull, exp = TRUE) %>%
  kable(
    col.names = c("Term","Odds ratio","Standard error","Z value", "P value")
  ) %>% 
  kable_styling("basic", bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Odds ratio </th>
   <th style="text-align:right;"> Standard error </th>
   <th style="text-align:right;"> Z value </th>
   <th style="text-align:right;"> P value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 2.0576230 </td>
   <td style="text-align:right;"> 0.1806552 </td>
   <td style="text-align:right;"> 3.9940811 </td>
   <td style="text-align:right;"> 0.0000649 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9310526 </td>
   <td style="text-align:right;"> 0.0062740 </td>
   <td style="text-align:right;"> -11.3866553 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.5367251 </td>
   <td style="text-align:right;"> 0.0524251 </td>
   <td style="text-align:right;"> 8.1955671 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.6991754 </td>
   <td style="text-align:right;"> 0.1280593 </td>
   <td style="text-align:right;"> -2.7944369 </td>
   <td style="text-align:right;"> 0.0051990 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score:hardship_score </td>
   <td style="text-align:right;"> 1.0058442 </td>
   <td style="text-align:right;"> 0.0044407 </td>
   <td style="text-align:right;"> 1.3122139 </td>
   <td style="text-align:right;"> 0.1894480 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry:hardship_score </td>
   <td style="text-align:right;"> 1.0217839 </td>
   <td style="text-align:right;"> 0.0357726 </td>
   <td style="text-align:right;"> 0.6024177 </td>
   <td style="text-align:right;"> 0.5468961 </td>
  </tr>
</tbody>
</table>
</details>

All interaction terms do not statistically significant, but I still want to compare those two models and see which one performs better.


### Compare two models

To choose from those two models, I compare several criteria of them. One is AIC, which is a commonly used measurement to compare models; lower values are considered better fitting than those with larger values.

Another is pseudo R-square, which provides model fit information. 

-   AIC


```r
glance(lr)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik    AIC    BIC deviance df.residual  nobs
          <dbl>   <int>   <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
1        21020.   15637 -10370. 20749. 20780.   20741.       15634 15638
```

```r
glance(lrFull)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik    AIC    BIC deviance df.residual  nobs
          <dbl>   <int>   <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
1        21020.   15637 -10369. 20750. 20796.   20738.       15632 15638
```

Those two tables show that the basic model has smaller AIC (20748.94) than the model with interactions (AIC = 20749.79).

-   Pseudo R-Square


```r
DescTools::PseudoR2(lr, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   1.328718e-02    1.290660e-02    1.770177e-02    2.394572e-02    1.754693e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   3.060096e-02    1.793824e-02    2.251562e-02    1.788166e-02    2.074894e+04 
            BIC          logLik         logLik0              G2 
   2.077957e+04   -1.037047e+04   -1.051012e+04    2.792998e+02 
```

```r
DescTools::PseudoR2(lrFull, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   1.343701e-02    1.286613e-02    1.789958e-02    2.421330e-02    1.774127e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   3.093989e-02    1.815369e-02    2.280072e-02    1.808023e-02    2.074979e+04 
            BIC          logLik         logLik0              G2 
   2.079573e+04   -1.036889e+04   -1.051012e+04    2.824491e+02 
```

Results of pseudo R-square tables show that the basic model accounts for approximately 23.95% of the total variance; the model with interactions accounts for approximately 24.21% of the total variance, slightly higher than basic model.

## Conclusion

In summary, while the interaction models have slightly higher pseudo R-square, considering significance of coefficients and AIC, I choose the basic model.

Variables (**ACT_score**, **hs_gpa_entry**, **hardship_score**) significantly predict success. 

Higher ACT score and higher hardship score are associated with lower success rates; Higher high school GPA entry are associated with higher success rates.
