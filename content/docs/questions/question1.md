# What are our overall success rates (C or better)?

## Set up and load data



```r
rm(list =ls())
library(tidyverse)
load("D:/R/data analysis/Institutional research/technical-exercise/content/docs/questions/data_230213_2317.Rdata")
```

## Research question

**What are our overall success rates (C or better)?**

**offical_grade** in course_erollments data set provides information of each student success, which can be used to evaluate overall success rates.


## Data cleaning

To analyze overall success rates, I first transform the students' **official_grade** variable to the **success** variable. If students obtained C or better grade, the **success** variable is recorded as Y. Otherwise, this variable is N.


```r
success <- course_enrollments %>%
 mutate(success = case_when(
   official_grade %in% c("A","A-","B+","B","B-","C") ~ "Y",
   TRUE ~ "N")) %>%
  count(success) %>%
  mutate(percent = round(100*n/sum(n),2)) %>%
  select(-n)
```

## Visualization
Let's visualize overall success rate.


```r
p <- success %>% 
  ggplot(aes(x = success, y = percent, fill = success)) +
  geom_bar(width = 0.5, position = position_dodge(0.5), stat = "identity") +
  geom_text(aes(label = percent), size = 6, vjust = -.8) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
  labs(
    x = "Success",
    y = "Percent (%)",
    title = "What are our overall success rates (C or better)?"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(face = "bold", vjust = 0.9, size = 18),
    axis.title.x = element_text(face = "bold", vjust = 0.9, size = 18),
    axis.text = element_text(colour = "black", size = 16),
    plot.title = element_text(face = "bold", size = 28)
  ) 
```



![](/images/p.png)
## Conclusion

The overall success rate is 59.84%.
