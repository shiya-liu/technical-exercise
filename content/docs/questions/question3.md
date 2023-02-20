
# Are success rates improving or getting worse?




## Set up and load data


```r
rm(list =ls())
library(pacman)
p_load(tidyverse, ggplot2, ggrepel,cowplot, ggtext, grid, gridExtra, kableExtra)
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/questions/data_230214_1659.Rdata")
```

## Research question
**Are success rates improving or getting worse?**

For this question, I examine success rates over terms (**term_code**) from **course_enrollments* data set via robust Welch test to determine whether success rates improve or not. 
Welch test is proved to be robust under all conditions.

## Data cleaning



## Visualization
<details><summary>Visualization code</summary>

```r
course_enrollments %>%
  group_by(term_code) %>%
  count(success) %>%
  mutate(
    percent = round(100*n/sum(n),2)
    ) %>%
  filter(success == "Y") %>%
  ungroup() %>%
  ggplot(
    aes(x= term_code, y = percent)
    ) +

  geom_line(size = 1, color = "black") +
  geom_point(
    aes(fill = term_code), shape = 21, size = 4.5, show.legend = F) +
  expand_limits(y = 0) +
  geom_text_repel(
    aes(label = precent), 
    size = 6, point.padding = .2) +
  scale_x_continuous(breaks = seq(1,6, by = 1)) +
  scale_y_continuous(
    limits = c(50,70),
    breaks = seq(50,70, by= 5),
    labels = c(0,seq(55, 70, by =5)),
    expand = c(0,0,0.05,0)
  ) +
  labs(
    x = "Term",
    y = "Success rate(%)\n",
    title = "Change in success rate over time"
  ) +
  scale_fill_viridis_c() +
  scale_alpha_discrete('Success rate') +
  theme_classic() +
  theme(
        legend.position = "none",
        axis.title.y = element_text(face = "bold", vjust = 0.9, size = 18),
        axis.title.x = element_text(face = "bold", vjust = 0.9, size = 18),
        axis.text = element_text(colour = "black", size = 16),
        plot.title = element_text(face = "bold", size = 28)
  ) 

gt <- ggplotGrob(p)
is_yaxis <- which(gt$layout$name == "axis-l")
yaxis <- gt$grobs[[is_yaxis]]
# You should grab the polyline child
yline <- yaxis$children[[1]]
yline$x <- unit(rep(1, 4), "npc")
yline$y <- unit(c(0, 0.1, 1, 0.15), "npc")
yline$id <- c(1, 1, 2, 2)
yline$arrow <- arrow(angle = 90)
yaxis$children[[1]] <- yline
gt$grobs[[is_yaxis]] <- yaxis
# grid plotting syntax
p <- grid.arrange(gt)      
```
</details>





![](/images/p1.png)
The plot indicates that success rates did change over terms, which first decreased from 62.66% to 55.93%, and then increased to 60.71%. The overall success rate shows a downward trend. Next, the Welch test determines if there is statistically significant difference between success rates over terms.


## Statistical test
+ Robust Welch test

```r
course_enrollments$success <- as.numeric(course_enrollments$success)
oneway.test(success~term_code, data = course_enrollments,var.equal=FALSE)
```

```

	One-way analysis of means (not assuming equal variances)

data:  success and term_code
F = 6.3989, num df = 5.0, denom df = 7464.7, p-value = 6.127e-06
```
With p value less than 0.05, Welch tests indicates that there is statistically significant difference between success rates over terms. But this test only shows that there is difference. We cannot know whether the performance becomes better or worse. Therefore, I further examine the relationship between term and success by regression model.


```r
course_enrollments$success <- as.factor(course_enrollments$success)
lr <- glm(success ~ term_code, data = course_enrollments, family = binomial(link = "logit"))
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
   <td style="text-align:right;"> 1.5392212 </td>
   <td style="text-align:right;"> 0.0346160 </td>
   <td style="text-align:right;"> 12.458870 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> term_code </td>
   <td style="text-align:right;"> 0.9901899 </td>
   <td style="text-align:right;"> 0.0092661 </td>
   <td style="text-align:right;"> -1.063934 </td>
   <td style="text-align:right;"> 0.2873585 </td>
  </tr>
</tbody>
</table>

With p value larger than 0.05, we cannot conclude that whether performance change over term. It might because that the relationship is not linear. Therefore, logistic regression does not apply to this situation. But the coefficient of term_code did show there is negative relationship even it is not significant. 


## Conclusion
The overall trend of performance decreases. 
