# What student information helps predict success in their courses?
Success in course can find in the course_enrollments data set and be defined as C or better score in their official grade.

In a narrow sense, the student information can be obtained from student_details data set, which include information like student_id_number, ACT_score, hs_gpa_entry, and hardship_score.
Based on those definition, I connect those two data sets via student_id_number, conduct logistic regression. 

More broadly, student information can include more information like courses they take, academic plan, college, their instructors, etc. In that sense, I connect student_details, acad_plan, class_instrutors, and course_enrollments data sets. Then I run the logistic regression with more independent variables.






## Set up and load data



## Logistic regression
### Basic Model


```r
DF1 <- student_details %>%
  full_join(course_enrollments, by="student_id_number") %>%
  select(c("student_id_number","ACT_score","hs_gpa_entry","hardship_score","success"))
```


```r
formula <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score)
lr <- glm(formula, data = DF1, family = binomial(link = "logit"))
```

1. calculate odds ratio

Odds ratio estimates the change in the odd of membership in the target group for one unit increase in the predictor.

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
   <td style="text-align:right;"> 1.3899599 </td>
   <td style="text-align:right;"> 0.0469639 </td>
   <td style="text-align:right;"> 7.011239 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9258930 </td>
   <td style="text-align:right;"> 0.0017925 </td>
   <td style="text-align:right;"> -42.955551 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.7199525 </td>
   <td style="text-align:right;"> 0.0131361 </td>
   <td style="text-align:right;"> 41.282927 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.8541707 </td>
   <td style="text-align:right;"> 0.0059563 </td>
   <td style="text-align:right;"> -26.463574 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>


2. Plot the dependent variable and key independent variables
+ ACT_score

+ hs_gpa_entry

+hardship_score





### Add Interactions
1. Evaluate interactions
+ ACT_score*hardship_score

```r
formulaAH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score)
lrAH <- glm(formulaAH, data = DF1, family = binomial(link = "logit"))
sim_slopes(lrAH, pred = ACT_score, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is INSIDE the interval [-12.97, 356.64], the slope of
ACT_score is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="content.en/docs/questions/question2_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of ACT_score when hardship_score = -0.4180933 (- 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.07   0.00   -31.13   0.00

Slope of ACT_score when hardship_score =  0.6612365 (Mean): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.08   0.00   -42.98   0.00

Slope of ACT_score when hardship_score =  1.7405663 (+ 1 SD): 

   Est.   S.E.   z val.      p
------- ------ -------- ------
  -0.08   0.00   -32.72   0.00
```
+ hs_gpa_entry*hardship_score

```r
formulaHH <- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + hs_gpa_entry*hardship_score)
lrHH <- glm(formulaHH, data = DF1, family = binomial(link = "logit"))
sim_slopes(lrHH, pred = hs_gpa_entry, modx = hardship_score, johnson_neyman = TRUE, jnplot = TRUE)
```

```
JOHNSON-NEYMAN INTERVAL 

When hardship_score is OUTSIDE the interval [-257.02, -11.50], the slope of
hs_gpa_entry is p < .05.

Note: The range of observed values of hardship_score is [0.00, 3.00]
```

<img src="content.en/docs/questions/question2_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```
SIMPLE SLOPES ANALYSIS 

Slope of hs_gpa_entry when hardship_score = -0.4180933 (- 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.52   0.02    29.41   0.00

Slope of hs_gpa_entry when hardship_score =  0.6612365 (Mean): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.54   0.01    41.26   0.00

Slope of hs_gpa_entry when hardship_score =  1.7405663 (+ 1 SD): 

  Est.   S.E.   z val.      p
------ ------ -------- ------
  0.57   0.02    32.32   0.00
```


2. Run model

```r
formulaFull<- as.formula(success~ ACT_score+ hs_gpa_entry+ hardship_score + ACT_score*hardship_score + hs_gpa_entry*hardship_score)
lrFull <- glm(formulaFull, data = DF1, family = binomial(link = "logit"))
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
   <td style="text-align:right;"> 1.4055738 </td>
   <td style="text-align:right;"> 0.0543578 </td>
   <td style="text-align:right;"> 6.263052 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score </td>
   <td style="text-align:right;"> 0.9289358 </td>
   <td style="text-align:right;"> 0.0020777 </td>
   <td style="text-align:right;"> -35.478859 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry </td>
   <td style="text-align:right;"> 1.6758928 </td>
   <td style="text-align:right;"> 0.0153249 </td>
   <td style="text-align:right;"> 33.693204 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hardship_score </td>
   <td style="text-align:right;"> 0.8393121 </td>
   <td style="text-align:right;"> 0.0414230 </td>
   <td style="text-align:right;"> -4.228878 </td>
   <td style="text-align:right;"> 0.0000235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ACT_score:hardship_score </td>
   <td style="text-align:right;"> 0.9948731 </td>
   <td style="text-align:right;"> 0.0016729 </td>
   <td style="text-align:right;"> -3.072508 </td>
   <td style="text-align:right;"> 0.0021227 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hs_gpa_entry:hardship_score </td>
   <td style="text-align:right;"> 1.0404389 </td>
   <td style="text-align:right;"> 0.0120785 </td>
   <td style="text-align:right;"> 3.282076 </td>
   <td style="text-align:right;"> 0.0010305 </td>
  </tr>
</tbody>
</table>
### Compare two models
To choose from those two models, I compare several criteria of them. One is AIC, which is… ; Another is pesudo-R square, which measures….
+ AIC

```r
glance(lr)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik     AIC     BIC deviance df.residual   nobs
          <dbl>   <int>   <dbl>   <dbl>   <dbl>    <dbl>       <int>  <int>
1       141412.  103434 -69161. 138330. 138368.  138322.      103431 103435
```

```r
glance(lrFull)
```

```
# A tibble: 1 × 8
  null.deviance df.null  logLik     AIC     BIC deviance df.residual   nobs
          <dbl>   <int>   <dbl>   <dbl>   <dbl>    <dbl>       <int>  <int>
1       141412.  103434 -69154. 138319. 138377.  138307.      103429 103435
```
+ Pseudo R-Square

```r
DescTools::PseudoR2(lr, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   2.185193e-02    2.179536e-02    2.943312e-02    3.949859e-02    2.900835e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   5.022639e-02    2.968696e-02    2.534363e-01    2.962055e-02    1.383295e+05 
            BIC          logLik         logLik0              G2 
   1.383677e+05   -6.916076e+04   -7.070582e+04    3.090117e+03 
```

```r
DescTools::PseudoR2(lrFull, which =  "all")
```

```
       McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson 
   2.195187e-02    2.186701e-02    2.956573e-02    3.967654e-02    2.913715e-02 
VeallZimmermann           Efron McKelveyZavoina            Tjur             AIC 
   5.044941e-02    2.982034e-02    2.402328e-01    2.976343e-02    1.383194e+05 
            BIC          logLik         logLik0              G2 
   1.383767e+05   -6.915369e+04   -7.070582e+04    3.104250e+03 
```




## Conclusion

