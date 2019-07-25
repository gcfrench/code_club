``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(viridis)
library(janitor)
```

> This script creates creates a ggridge plot of age of nobel prize winners for each category

``` r
nobel_winners_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
```

Firstly we are only interested in individuals who have won a prize along with the year they won and their date of birth, so that we work out how old they were when they won. We can exclude organisation winners at this stage.

``` r
nobel_winners <- nobel_winners_raw %>% 
  filter(laureate_type == "Individual") %>% 
  select(prize_year, category, birth_date) 
```

Lets work out the age each individual was when they were awarded a Nobel prize. We only have the year of the prize and so won't worry too much if a person had their birthday before the date in the year, ignoring that we may overestimate their age by a year. Also the few individuals with missing birth dates are removed.

``` r
nobel_winners <- nobel_winners %>% 
  filter(!is.na(birth_date)) %>% 
  mutate(age = prize_year - year(birth_date)) 
```

The range of years for each Nobel proze category can be displayed using a ggridge plot. First group the categories so that the science categories are next to each other

``` r
categories <- c("Physics", "Chemistry", "Medicine", "Peace", "Literature", "Economics")
nobel_winners <- nobel_winners %>% 
  mutate(category = factor(category, levels = categories))
```

Now create the plot using ggplot2 with ggridges geometry, theme from ggthemes package and using a colour palette from the viridis package.

``` r
nobel_plot <- ggplot(nobel_winners, aes(x = age, y = category, fill = category)) +
  geom_density_ridges2(rel_min_height = 0.01,
                       scale = 2.4,
                       center_axis_labels = TRUE,
                       colour = 'white',
                       size = 1) +
  scale_y_discrete(expand = expand_scale(add = c(0.2, 0.5))) +
  scale_x_continuous(expand = expand_scale(add = c(0, 5)),
                     breaks = seq(0, 100, 10)) +
  theme_economist() +
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 2),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 20, colour = 'white', face = 'bold'),
        axis.text.x = element_text(vjust = 2),
        axis.text.y = element_text(hjust = 1),
        plot.title = element_text(size = 24, colour = 'white', face = 'italic',
                                  hjust = -0.4, vjust = -10),
        plot.subtitle = element_text(size = 16, colour = 'white', face = 'italic',
                                     hjust = -0.2, vjust = -16),
        plot.caption = element_text(size = 12, colour = 'white', face = 'italic'),
        legend.position = "none") +
  labs(title = "Age ranges of Nobel Laureates",
       subtitle = "TidyTuesday 2019-05-14",
       caption = "Kaggle: Nobel Laureates, 1901-Present") +
  scale_fill_viridis(discrete = TRUE)
```

    ## Warning: Ignoring unknown parameters: center_axis_labels

``` r
nobel_plot
```

    ## Picking joint bandwidth of 3.71

![](nobel_prize_winners_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggsave(filename = "nobel_winners_age.png",
       plot = nobel_plot,
       width = 10)
```

    ## Saving 10 x 5 in image

    ## Picking joint bandwidth of 3.71

It looks like the winners of the three science categories tend to be younger than the winners of the Peace, Literature or Economics categories.
