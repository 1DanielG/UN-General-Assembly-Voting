UN General Assembly Voting
================
Ali Gharaee
December 3, 2017

Summery
=======

This data provides the voting history of countries in the United Nations General Assembly, along with information such as date, description, and topics for each vote. In this exploratoy analysis I will explore the historical voting of the United Nations General Assembly, including analyzing differences in voting between countries, across time, and among international issue.

Orginal dataset come from the **kaggle** *UN General Assembly Votes, 1946-2015*. see [Kaggle UN dataset](https://www.kaggle.com/unitednations/general-assembly) for value's detail explanation.

This dataset originaly come form *Erik Voeten Dataverse* see [United Nations General Assembly Voting Data](https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379)

Packages instalation
--------------------

``` r
library(tidyverse)
library(readr)
library(countrycode)
```

Data Import
-----------

`Votes` dataset contains the history of each country's vote. The `resolutions` dataset shows relationships betwen each vote and 6 UN resolutions.

``` r
votes <- read_csv("votes.csv")
resolutions <- read_csv("resolutions.csv")
```

### Section 1: Data Exploration

Start with`votes` data. In this dataset : The vote column has 5 numbers that represents that country's vote: + **temp** 1 = yes 2 = Abstain 3 = No 8 = Not present 9 = Not a member

use `dplyr::filter` to remove rows that I am not interested in (8,9)

First session of the UN was held in 1946 and and holds one session per year.\* use `dplyr::mutate` to convert session coloum to a new coloum to make them more informative and easier to work with.

The country codes in the ccode column are what is called [Correlates of War codes](http://cow.dss.ucdavis.edu/data-sets/cow-country-codes). I used the *countrycode* package to translate ccode to country names.

Use `dplyr::summarize` to calculate *% of votes that are yes* . It shows the country tends to agree with international consensus(vote = 1 means yes).

Finally use `dplyr::arrange` to find the countries that voted "yes" least often over time

``` r
votes_tidy <- votes %>% 
  filter(vote <= 3) %>% 
  mutate(year = session + 1945 ,
         country = countrycode(ccode,"cown", "country.name" )) %>%
  select(-c(session,ccode)) %>%
  group_by(year , country) %>% 
  summarize( total = n() , percent_yes = mean(vote == 1)) %>% 
  filter(total > 100) %>% # remove countries that have fewer than 100 votes.
  arrange(percent_yes) %>% 
  ungroup()
```

Apprently , America first.

Next, I like to know how much the average "agreeableness" of 6 countries changed from year to year(1946-2015) so I filter out 6 countries:

+USA +Denmark +Norway +Sweden +France +Israel

Use the *ggplot2* package to turn your results into a visualization of the percentage of "yes" votes over time for 6 countries:

``` r
subject_country <- votes_tidy %>% 
  filter(country %in% c("United States of America" , "Denmark" ,
                        "Norway", "Sweden" , "Israel" , "France"))

subject_country %>% 
  ggplot(aes( x = year , y = percent_yes)) +
  geom_line(color = "blue") +
  scale_x_continuous("Year") +
  scale_y_continuous("Yes-vote") +
  facet_wrap(~ country)
```

![](html2_files/figure-markdown_github/unnamed-chunk-4-1.png)

Similarly, we have agreeableness relationship between Scandinavian countries (Norway, Sweden, Denmark), USA and Israel on the same graph, using color to distinguish each country: ![](html2_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Section 2: Data Modelling

A linear regression is a model that examine how one variable changes with respect to another by fitting a best fit line.

Model the trend with liner regression, finding the best fit line Tidying model with *broom* package.

``` r
library(broom)
```

At this point, analyses will involve joining the votes and resolutions datasets which includes topic information about each country, to be able to analyze votes within particular topics. Then use `tidyr::gather` to tidy the new dataset for further analysis.

### Data preparation

``` r
votes_tidy2 <- votes %>%
  filter(vote <= 3) %>% 
  mutate(year = session + 1945 ,
         country = countrycode(ccode,"cown", "country.name" )) %>% 
  inner_join(resolutions, by = c("rcid", "session")) %>% 
  gather(topic, has_topic , me:ec) %>% 
  filter(has_topic == 1) %>% 
  select(country, year, vote, topic, has_topic) %>% 
  #recode the data to replace these codes with their full name
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))
```

Data modelling
--------------

Summarize the votes for each combination of country, year, and topic

``` r
(country_year_topic <- votes_tidy2 %>% 
  group_by(country, year, topic) %>% 
  summarize( total = n() , percent_yes = mean(vote == 1)) %>% 
  ungroup()  )
```

    ## # A tibble: 26,968 x 5
    ##        country  year                        topic total percent_yes
    ##          <chr> <dbl>                        <chr> <int>       <dbl>
    ##  1 Afghanistan  1947                  Colonialism     8   0.5000000
    ##  2 Afghanistan  1947         Economic development     1   0.0000000
    ##  3 Afghanistan  1947                 Human rights     1   0.0000000
    ##  4 Afghanistan  1947         Palestinian conflict     6   0.0000000
    ##  5 Afghanistan  1949 Arms control and disarmament     3   0.0000000
    ##  6 Afghanistan  1949                  Colonialism    22   0.8636364
    ##  7 Afghanistan  1949         Economic development     1   1.0000000
    ##  8 Afghanistan  1949                 Human rights     3   0.0000000
    ##  9 Afghanistan  1949              Nuclear weapons     3   0.0000000
    ## 10 Afghanistan  1949         Palestinian conflict    11   0.8181818
    ## # ... with 26,958 more rows

Modelling for each country has 4 steps: +Divided the data for each country into a separate dataset in the data column. `tidyr::nest` all columns except country and topic, which will result in a data frame with one row per country-topic. + Use `purrr::map` by applying liner regression to each item. + Use `purrr::map` again to tidy `broom::tidy` each model in the dataset. + Use `tidyr::unnest`to combine all of those into a large data frame.

Then filter the slope of each model (term = year) to see how each is changing over timeand. Also filter out random noise in p.value to extract only cases that are statistically significant.

Finnaly use `dplyr::arrange` to find the countries with the highest and lowest slopes (the estimate column)

Countries has the steepest downward trend in Colonialism (goes agnasit UN resolution):

``` r
(country_coef_model <- country_year_topic %>% 
  nest(-country, -topic) %>% 
  mutate( model = map(data, ~lm(percent_yes ~ year , .))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>% 
  filter ( term == "year" , p.adjust(p.value) < 0.05 ,
           topic == "Colonialism") %>%
  arrange (estimate))
```

    ## # A tibble: 26 x 7
    ##                       country       topic  term     estimate    std.error
    ##                         <chr>       <chr> <chr>        <dbl>        <dbl>
    ##  1                     Israel Colonialism  year -0.009529337 0.0017706645
    ##  2   United States of America Colonialism  year -0.007473819 0.0009222886
    ##  3                   Pakistan Colonialism  year  0.004277785 0.0008367695
    ##  4                      Egypt Colonialism  year  0.004941763 0.0009449754
    ##  5                Afghanistan Colonialism  year  0.005106200 0.0009885245
    ##  6                   Malaysia Colonialism  year  0.005597429 0.0010631137
    ##  7                   Thailand Colonialism  year  0.005754159 0.0011343229
    ##  8                      India Colonialism  year  0.005858268 0.0010665963
    ##  9 Iran (Islamic Republic of) Colonialism  year  0.006048075 0.0009691037
    ## 10                       Cuba Colonialism  year  0.006337732 0.0011236394
    ## # ... with 16 more rows, and 2 more variables: statistic <dbl>,
    ## #   p.value <dbl>

Countries has the steepest upward trend in Colonialism:

``` r
(country_coef_model <- country_year_topic %>% 
  nest(-country, -topic) %>% 
  mutate( model = map(data, ~lm(percent_yes ~ year , .))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>% 
  filter( term == "year" , p.adjust(p.value) < 0.05 ,
           topic == "Colonialism") %>%
  arrange(desc(estimate) )  )
```

    ## # A tibble: 26 x 7
    ##                             country       topic  term    estimate
    ##                               <chr>       <chr> <chr>       <dbl>
    ##  1                     South Africa Colonialism  year 0.016574451
    ##  2                      New Zealand Colonialism  year 0.010865002
    ##  3 Saint Vincent and the Grenadines Colonialism  year 0.010811672
    ##  4                            Spain Colonialism  year 0.009479393
    ##  5                        Nicaragua Colonialism  year 0.009464870
    ##  6                            Chile Colonialism  year 0.009323302
    ##  7 Bolivia (Plurinational State of) Colonialism  year 0.008524197
    ##  8               Dominican Republic Colonialism  year 0.008422279
    ##  9                          Austria Colonialism  year 0.008353305
    ## 10                           Brazil Colonialism  year 0.008271821
    ## # ... with 16 more rows, and 3 more variables: std.error <dbl>,
    ## #   statistic <dbl>, p.value <dbl>

Visualizing trends in topics for Norway over time:

``` r
country_year_topic %>% 
  filter (country == "Norway") %>% 
  ggplot(aes(year, percent_yes ))+
    geom_line(color = "blue")+
    facet_wrap(~ topic)+
    scale_x_continuous("Year") +
    scale_y_continuous("Yes-vote")
```

![](html2_files/figure-markdown_github/unnamed-chunk-11-1.png)
