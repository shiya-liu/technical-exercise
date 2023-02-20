# What student information helps predict success in their courses?



## Set up and load data



## Research question
**What student information helps predict success in their courses?**

Success in course can be found in the **course_enrollments** data set and be defined as C or better score in their official grade.

The student information can be obtained from **student_details** data set, which include information like **student_id_number**, **ACT_score**, **hs_gpa_entry**, and **hardship_score**. 

## Data cleaning
1. combine **student_details** and **course_enrollments** data set via student_id_number variable.
2. deal with missing values


```r
DF <- student_details %>%
  full_join(course_enrollments, by="student_id_number") %>%
  select(c("student_id_number","ACT_score","hs_gpa_entry","hardship_score","success"))
```


```
 student_id_number   ACT_score      hs_gpa_entry   hardship_score  
 Min.   :    1     Min.   : 0.00   Min.   :0.21    Min.   :0.0000  
 1st Qu.:25821     1st Qu.:20.00   1st Qu.:3.07    1st Qu.:0.0000  
 Median :50064     Median :23.00   Median :3.43    Median :0.0000  
 Mean   :48958     Mean   :23.05   Mean   :3.39    Mean   :0.6612  
 3rd Qu.:71402     3rd Qu.:26.00   3rd Qu.:3.76    3rd Qu.:1.0000  
 Max.   :97112     Max.   :36.00   Max.   :5.00    Max.   :3.0000  
                   NA's   :39865   NA's   :34674                   
 success     
 N   : 6585  
 Y   : 9810  
 NA's:87040  
             
             
             
             
```

After combining two data sets, the new data set: **DF** has several variables (**ACT_score**, **hs_gpa_entry**, and **success**) with missing values. To have a better model, I first impute those variables.



## Visualization

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

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-5-1.png" width="672" />

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

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-6-1.png" width="672" />

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

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Three boxplots do not show huge differences on ACT_score, hs_gpa_entry, and hardship_score between students succeed and others.

## Logistic regression
1. Run basic logistic regression model
2. Evaluate interactions
3. Run model with interactions
4. Compare two models

First, I run the basic logistic regression model with three independent variables (**ACT_score**, **hs_gpa_entry**,**hardship_score**) without considering interaction.

### Basic Model


```r
DF$success <- relevel((DF$success), ref = "N")
```


```r
formula <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score)
lr <- glm(formula, data = DF, family = binomial(link = "logit"))
```

Next, I explore coefficients by calculate odds ratio.

-   Odds ratio is an measure of association between dependent variable and independent variables in the logistic regression. It estimates the change in the odd of membership in the target group for one unit increase in the predictor.


```r
broom::tidy(lr, exp = TRUE) %>%
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
   <td style="text-align:right;"> 1.4601350 </td>
   <td style="text-align:right;"> 0.0449586 </td>
   <td style="text-align:right;"> 8.419505 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9294064 </td>
   <td style="text-align:right;"> 0.0018543 </td>
   <td style="text-align:right;"> -39.481169 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.6506766 </td>
   <td style="text-align:right;"> 0.0138676 </td>
   <td style="text-align:right;"> 36.140837 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.8450598 </td>
   <td style="text-align:right;"> 0.0059242 </td>
   <td style="text-align:right;"> -28.417118 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

The results show that all three independent variables are statistically significant associated with the dependent variable (**success**).

-   ACT_score: After controlling all other variables, the odds of success **increase** as ACT score increases.

-   hs_gpa_entry: After controlling all other variables, the odds of success **increase** as hs_gpa_entry increases.

-   hardship_score: After controlling all other variables, the odds of success **decrease** as hardship score increases.

### Add Interactions

Intuitively, those three variables might interact with each other when predicting **success** variable. To verify it, I first examine the interaction. Next, based on the results, I choose the interactions to add in the model.

1. Evaluate interactions

-   ACT_score*hardship_score


```r
formulaAH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score)
lrAH <- glm(formulaAH, data = DF, family = binomial(link = "logit"))
sim_slopes(lrAH, pred = ACT_score, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is OUTSIDE the interval [-26.74, -8.01], the slope of
ACT_score is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-11-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of ACT_score when hardship_score = -0.4180933 (- 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.07   0.00   -27.99   0.00

Slope of ACT_score when hardship_score =  0.6612365 (Mean): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.07   0.00   -39.55   0.00

Slope of ACT_score when hardship_score =  1.7405663 (+ 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.08   0.00   -32.03   0.00
```

From the plot, we can see that for ACT_score, the slope of hardship_score is significantly different from zero and in this case negative.

-   hs_gpa_entry*hardship_score


```r
formulaHH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + hs_gpa_entry*hardship_score)
lrHH <- glm(formulaHH, data = DF, family = binomial(link = "logit"))
sim_slopes(lrHH, pred = hs_gpa_entry, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is OUTSIDE the interval [-38.80, -8.44], the slope of
hs_gpa_entry is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="/docs/questions/question2_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of hs_gpa_entry when hardship_score = -0.4180933 (- 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.46   0.02    25.43   0.00

Slope of hs_gpa_entry when hardship_score =  0.6612365 (Mean): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.50   0.01    36.04   0.00

Slope of hs_gpa_entry when hardship_score =  1.7405663 (+ 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.54   0.02    30.01   0.00
```

For hs_gpa_entry, the slope of hardship_score is also significantly different from zero and positive. Overall, those two interactions are significant. So, I model these interactions and compare the new model with the previous basic model.


2. Run model with interactions


```r
formulaFull<- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score + hs_gpa_entry*hardship_score)
lrFull <- glm(formulaFull, data = DF, family = binomial(link = "logit"))
```

Odds ratio estimates the change in the odd of membership in the target group for one unit increase in the predictor.


```r
broom::tidy(lrFull, exp = TRUE) %>%
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
   <td style="text-align:right;"> 1.4726068 </td>
   <td style="text-align:right;"> 0.0519907 </td>
   <td style="text-align:right;"> 7.444300 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9361319 </td>
   <td style="text-align:right;"> 0.0021565 </td>
   <td style="text-align:right;"> -30.604093 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.5674512 </td>
   <td style="text-align:right;"> 0.0162978 </td>
   <td style="text-align:right;"> 27.577427 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.8380645 </td>
   <td style="text-align:right;"> 0.0397342 </td>
   <td style="text-align:right;"> -4.446048 </td>
   <td style="text-align:right;"> 8.7e-06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score:hardship_score </td>
   <td style="text-align:right;"> 0.9890067 </td>
   <td style="text-align:right;"> 0.0017406 </td>
   <td style="text-align:right;"> -6.350799 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry:hardship_score </td>
   <td style="text-align:right;"> 1.0788345 </td>
   <td style="text-align:right;"> 0.0126484 </td>
   <td style="text-align:right;"> 5.999279 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
</tbody>
</table>

All variables, including interactions are statistically significant associated with the dependent variable


### Compare two models

To choose from those two models, I compare several criteria of them. One is AIC, which is a commonly used measurement to compare models; lower values are considered better fitting than those with larger values.

Another is pseudo R-square, which provides model fit information. 

-   AIC


```r
glance(lr)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik     AIC     BIC deviance df.residual   nobs
          <dbl>   <int>   <dbl>   <dbl>   <dbl>    <dbl>       <int>  <int>
1       141319.  103434 -69377. 138762. 138800.  138754.      103431 103435
```

```r
glance(lrFull)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik     AIC     BIC deviance df.residual   nobs
          <dbl>   <int>   <dbl>   <dbl>   <dbl>    <dbl>       <int>  <int>
1       141319.  103434 -69351. 138715. 138772.  138703.      103429 103435
```

Those two tables show that the model with interactions has smaller AIC (138714.9) than the basic model (AIC = 138761.6).

-   Pseudo R-Square


```r
DescTools::PseudoR2(lr, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   1.815623e-02    1.809962e-02    2.450104e-02    3.288988e-02    2.420573e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   4.192248e-02    2.498798e-02    3.073081e-02    2.480682e-02    1.387616e+05 
            BIC          logLik         logLik0              G2 
   1.387997e+05   -6.937678e+04   -7.065969e+04    2.565828e+03 
```

```r
DescTools::PseudoR2(lrFull, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   1.851434e-02    1.842943e-02    2.497821e-02    3.353042e-02    2.467138e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   4.272895e-02    2.543636e-02    3.134814e-02    2.529393e-02    1.387149e+05 
            BIC          logLik         logLik0              G2 
   1.387722e+05   -6.935147e+04   -7.065969e+04    2.616436e+03 
```

Results of pseudo R-square tables show that the basic model accounts for approximately 32.9% of the total variance; the model with interactions accounts for approximately 33.53% of the total variance.

## Conclusion

In summary, the model with interactions has better performance. Variables (**ACT_score**, **hs_gpa_entry**, **hardship_score**) and interactions ( **ACT_score x hardship_score**, **hs_gpa_entry x hardship_score**) significantly predict success.
