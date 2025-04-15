# Lab 6: Childcare Costs in California
Catalina Dorado
2025-04-14

## The Data

In this lab we’re going look at the median weekly cost of childcare in
California. A detailed description of the data can be found
[here](https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-09/readme.md).

The data come to us from
[TidyTuesday](https://github.com/rfordatascience/tidytuesday).

**0. Load the appropriate libraries and the data.**

``` r
library(tidyverse)
library(dplyr)
```

``` r
childcare_costs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')

counties <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

childcare_costs
```

    # A tibble: 34,567 × 61
       county_fips_code study_year unr_16 funr_16 munr_16 unr_20to64 funr_20to64
                  <dbl>      <dbl>  <dbl>   <dbl>   <dbl>      <dbl>       <dbl>
     1             1001       2008   5.42    4.41    6.32        4.6         3.5
     2             1001       2009   5.93    5.72    6.11        4.8         4.6
     3             1001       2010   6.21    5.57    6.78        5.1         4.6
     4             1001       2011   7.55    8.13    7.03        6.2         6.3
     5             1001       2012   8.6     8.88    8.29        6.7         6.4
     6             1001       2013   9.39   10.3     8.56        7.3         7.6
     7             1001       2014   8.5     9.18    7.95        6.8         6.8
     8             1001       2015   7.64    8.45    6.91        5.9         6.1
     9             1001       2016   5.59    6.27    4.99        4.4         4.6
    10             1001       2017   5.21    5.84    4.64        4.6         4.9
    # ℹ 34,557 more rows
    # ℹ 54 more variables: munr_20to64 <dbl>, flfpr_20to64 <dbl>,
    #   flfpr_20to64_under6 <dbl>, flfpr_20to64_6to17 <dbl>,
    #   flfpr_20to64_under6_6to17 <dbl>, mlfpr_20to64 <dbl>, pr_f <dbl>,
    #   pr_p <dbl>, mhi_2018 <dbl>, me_2018 <dbl>, fme_2018 <dbl>, mme_2018 <dbl>,
    #   total_pop <dbl>, one_race <dbl>, one_race_w <dbl>, one_race_b <dbl>,
    #   one_race_i <dbl>, one_race_a <dbl>, one_race_h <dbl>, …

``` r
counties
```

    # A tibble: 3,144 × 4
       county_fips_code county_name     state_name state_abbreviation
                  <dbl> <chr>           <chr>      <chr>             
     1             1001 Autauga County  Alabama    AL                
     2             1003 Baldwin County  Alabama    AL                
     3             1005 Barbour County  Alabama    AL                
     4             1007 Bibb County     Alabama    AL                
     5             1009 Blount County   Alabama    AL                
     6             1011 Bullock County  Alabama    AL                
     7             1013 Butler County   Alabama    AL                
     8             1015 Calhoun County  Alabama    AL                
     9             1017 Chambers County Alabama    AL                
    10             1019 Cherokee County Alabama    AL                
    # ℹ 3,134 more rows

**1. Briefly describe the dataset (~ 4 sentences). What information does
it contain?**

The first dataset, `childcare_costs`, contains numerical values
representing various explanatory variables involved in childcare costs.
Every variable has a data type as doubles. Each row is identified by
their `county_fips_code`, which uniquely identifies the county in a
state, and `study_year`, which is the year the data collection is
representative of. The unique identity of each county in a state can be
referred to the `counties` dataset, where it contains the FIPS code of
the state to which the county belongs to.

## California Childcare Costs

Let’s start by focusing only on California.

**2. Create a `ca_childcare` dataset of childcare costs in California,
containing (1) county information and (2) just the year and childcare
cost variable information from the `childcare_costs` dataset.**

*Hint: There are 58 counties in CA and 11 years in the dataset.
Therefore, your new dataset should have 53 x 11 = 638 observations. The
final data set should have study year, median household income expressed
in 2018 dollars, all the variables associated with full-time median
price charged for Center-based Care, and California county names*

``` r
childcare <- childcare_costs %>%
  mutate(county_fips_code = as.character(county_fips_code))

ca_counties <- counties %>%
  mutate(county_fips_code = as.character(county_fips_code)) 

ca_childcare <- childcare %>%
  left_join(ca_counties, by = "county_fips_code") %>% 
  filter(state_name == "California")

childcare
```

    # A tibble: 34,567 × 61
       county_fips_code study_year unr_16 funr_16 munr_16 unr_20to64 funr_20to64
       <chr>                 <dbl>  <dbl>   <dbl>   <dbl>      <dbl>       <dbl>
     1 1001                   2008   5.42    4.41    6.32        4.6         3.5
     2 1001                   2009   5.93    5.72    6.11        4.8         4.6
     3 1001                   2010   6.21    5.57    6.78        5.1         4.6
     4 1001                   2011   7.55    8.13    7.03        6.2         6.3
     5 1001                   2012   8.6     8.88    8.29        6.7         6.4
     6 1001                   2013   9.39   10.3     8.56        7.3         7.6
     7 1001                   2014   8.5     9.18    7.95        6.8         6.8
     8 1001                   2015   7.64    8.45    6.91        5.9         6.1
     9 1001                   2016   5.59    6.27    4.99        4.4         4.6
    10 1001                   2017   5.21    5.84    4.64        4.6         4.9
    # ℹ 34,557 more rows
    # ℹ 54 more variables: munr_20to64 <dbl>, flfpr_20to64 <dbl>,
    #   flfpr_20to64_under6 <dbl>, flfpr_20to64_6to17 <dbl>,
    #   flfpr_20to64_under6_6to17 <dbl>, mlfpr_20to64 <dbl>, pr_f <dbl>,
    #   pr_p <dbl>, mhi_2018 <dbl>, me_2018 <dbl>, fme_2018 <dbl>, mme_2018 <dbl>,
    #   total_pop <dbl>, one_race <dbl>, one_race_w <dbl>, one_race_b <dbl>,
    #   one_race_i <dbl>, one_race_a <dbl>, one_race_h <dbl>, …

``` r
ca_counties
```

    # A tibble: 3,144 × 4
       county_fips_code county_name     state_name state_abbreviation
       <chr>            <chr>           <chr>      <chr>             
     1 1001             Autauga County  Alabama    AL                
     2 1003             Baldwin County  Alabama    AL                
     3 1005             Barbour County  Alabama    AL                
     4 1007             Bibb County     Alabama    AL                
     5 1009             Blount County   Alabama    AL                
     6 1011             Bullock County  Alabama    AL                
     7 1013             Butler County   Alabama    AL                
     8 1015             Calhoun County  Alabama    AL                
     9 1017             Chambers County Alabama    AL                
    10 1019             Cherokee County Alabama    AL                
    # ℹ 3,134 more rows

``` r
ca_childcare
```

    # A tibble: 638 × 64
       county_fips_code study_year unr_16 funr_16 munr_16 unr_20to64 funr_20to64
       <chr>                 <dbl>  <dbl>   <dbl>   <dbl>      <dbl>       <dbl>
     1 6001                   2008   6.6     6.57    6.63        6.1         6.1
     2 6001                   2009   7.71    7.53    7.86        7.2         7.1
     3 6001                   2010   8.5     8.24    8.73        7.9         7.7
     4 6001                   2011   9.21    8.87    9.51        8.7         8.4
     5 6001                   2012   9.91    9.68   10.1         9.3         9.2
     6 6001                   2013  10.3    10.2    10.5         9.7         9.6
     7 6001                   2014   9.6     9.59    9.58        9           9.1
     8 6001                   2015   8.26    8.44    8.11        7.8         8  
     9 6001                   2016   7.11    7.34    6.91        6.7         7  
    10 6001                   2017   6.05    6.06    6.03        5.7         5.7
    # ℹ 628 more rows
    # ℹ 57 more variables: munr_20to64 <dbl>, flfpr_20to64 <dbl>,
    #   flfpr_20to64_under6 <dbl>, flfpr_20to64_6to17 <dbl>,
    #   flfpr_20to64_under6_6to17 <dbl>, mlfpr_20to64 <dbl>, pr_f <dbl>,
    #   pr_p <dbl>, mhi_2018 <dbl>, me_2018 <dbl>, fme_2018 <dbl>, mme_2018 <dbl>,
    #   total_pop <dbl>, one_race <dbl>, one_race_w <dbl>, one_race_b <dbl>,
    #   one_race_i <dbl>, one_race_a <dbl>, one_race_h <dbl>, …

**3. Using a function from the `forcats` package, complete the code
below to create a new variable where each county is categorized into one
of the [10 Census regions](https://census.ca.gov/regions/) in
California. Use the Region description (from the plot), not the Region
number. An example region has been started for you.**

*Hint: This is probably a good place to use ChatGPT to reduce on tedious
work. But you do need to know how to prompt ChatGPT to make it useful!*

``` r
# Initial code eliminates the word “County” from each of the county names in the `ca_childcare` dataset. Keep and pipe into the rest of your data manipulations.

ca_childcare <- ca_childcare |> 
  mutate(county_name = str_remove(county_name, " County")) |> 
  mutate(region = fct_collapse(county_name, 
                               'Superior California' = c("Butte", "Colusa", "El Dorado", "Glenn", "Lassen", "Modoc", "Nevada", "Placer", "Plumas", "Sacramento", "Shasta", "Sierra", "Siskiyou", "Sutter", "Tehama", "Yolo", "Yuba"), 
                               'North Coast' = c("Del Norte", "Humboldt", "Lake", "Mendocino", "Napa", "Sonoma", "Trinity"), 
                               'San Francisco Bay Area' = c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Solano"), 
                               'Northern San Joaquin Valley' = c("Alpine", "Amador", "Calaveras", "Madera", "Mariposa", "Merced", "Mono", "San Joaquin", "Stanislaus", "Tuolumne"), 
                               'Central Coast' = c("Monterey", "San Benito", "San Luis Obispo", "Santa Barbara", "Santa Cruz", "Ventura"), 
                               'Southern San Joaquin Valley' = c("Fresno", "Inyo", "Kern", "Kings", "Tulare"), 
                               'Inland Empire' = c("Riverside", "San Bernardino"), 
                               'Los Angeles County' = "Los Angeles", 
                               'Orange County' = "Orange", 
                               'San Diego - Imperial' = c("Imperial", "San Diego")))

ca_childcare
```

    # A tibble: 638 × 65
       county_fips_code study_year unr_16 funr_16 munr_16 unr_20to64 funr_20to64
       <chr>                 <dbl>  <dbl>   <dbl>   <dbl>      <dbl>       <dbl>
     1 6001                   2008   6.6     6.57    6.63        6.1         6.1
     2 6001                   2009   7.71    7.53    7.86        7.2         7.1
     3 6001                   2010   8.5     8.24    8.73        7.9         7.7
     4 6001                   2011   9.21    8.87    9.51        8.7         8.4
     5 6001                   2012   9.91    9.68   10.1         9.3         9.2
     6 6001                   2013  10.3    10.2    10.5         9.7         9.6
     7 6001                   2014   9.6     9.59    9.58        9           9.1
     8 6001                   2015   8.26    8.44    8.11        7.8         8  
     9 6001                   2016   7.11    7.34    6.91        6.7         7  
    10 6001                   2017   6.05    6.06    6.03        5.7         5.7
    # ℹ 628 more rows
    # ℹ 58 more variables: munr_20to64 <dbl>, flfpr_20to64 <dbl>,
    #   flfpr_20to64_under6 <dbl>, flfpr_20to64_6to17 <dbl>,
    #   flfpr_20to64_under6_6to17 <dbl>, mlfpr_20to64 <dbl>, pr_f <dbl>,
    #   pr_p <dbl>, mhi_2018 <dbl>, me_2018 <dbl>, fme_2018 <dbl>, mme_2018 <dbl>,
    #   total_pop <dbl>, one_race <dbl>, one_race_w <dbl>, one_race_b <dbl>,
    #   one_race_i <dbl>, one_race_a <dbl>, one_race_h <dbl>, …

**4. Let’s consider the median household income of each region, and how
that income has changed over time. Create a table with ten rows, one for
each region, and two columns, one for 2008 and one for 2018. The cells
should contain the `median` of the median household income (expressed in
2018 dollars) of the `region` and the `study_year`. Arrange the rows by
2018 values.**

``` r
# Hint: You will want to calculate the median of median income before you make columns for 2008/2018 by transforming the data

# Median household income code = `mhi_2018`
ca_median_income <- ca_childcare |> 
  select(region, study_year, mhi_2018) |> 
  filter(study_year %in% c(2008, 2018)) |> 
  group_by(region, study_year) |> 
  summarise(mean_mhi = mean(mhi_2018), .groups = "drop") |> 
  pivot_wider(names_from = study_year, values_from = mean_mhi)
  
ca_median_income
```

    # A tibble: 10 × 3
       region                      `2008`  `2018`
       <fct>                        <dbl>   <dbl>
     1 San Francisco Bay Area      90247. 101231.
     2 Northern San Joaquin Valley 58472.  57629.
     3 Superior California         57490.  56531.
     4 North Coast                 53760.  54642.
     5 Southern San Joaquin Valley 52787.  51599.
     6 San Diego - Imperial        58201.  60344.
     7 Los Angeles County          63471.  64251 
     8 Central Coast               75187.  75511.
     9 Orange County               86452.  85398 
    10 Inland Empire               65977.  62056 

**5. Which California `region` had the lowest `median` full-time median
weekly price for center-based childcare for infants in 2018? Does this
`region` correspond to the `region` with the lowest `median` income in
2018 that you found in Q4?**

*Hint: The code should give me the EXACT answer. This means having the
code output the exact row(s) and variable(s) necessary for providing the
solution. Consider the `slice()` functions.*

``` r
# Median weekly price for center-based childcare for infants = mc_infant
ca_mc_infants <- ca_childcare |> 
  select(region, study_year, mc_infant) |>
  filter(study_year == "2018") |>
  group_by(region, study_year) |> 
  summarise(mean_mc_infant = mean(mc_infant), .groups = "drop") |> 
  pivot_wider(names_from = study_year, values_from = mean_mc_infant) |> 
  slice_min(`2018`, n = 1)

ca_median_income |> 
  slice_min(`2018`, n = 1)
```

    # A tibble: 1 × 3
      region                      `2008` `2018`
      <fct>                        <dbl>  <dbl>
    1 Southern San Joaquin Valley 52787. 51599.

``` r
ca_mc_infants
```

    # A tibble: 1 × 2
      region              `2018`
      <fct>                <dbl>
    1 Superior California   242.

> The California region that had the lowest median full-time median
> weekly price for center-based childcare for infants in 2018 was
> Superior California (241,7306). This region does not correspond to the
> region with the lowest median household income in 2018 (Southern San
> Joaquin Valley, 51599.4).

**6. Recreate the plot (look at online instructions) that shows, for all
ten regions, the change over time of the full-time median price for
center-based childcare for infants, toddlers, and preschoolers. Recreate
the plot. You do not have to replicate the exact colors or theme, but
your plot should have the same content, including the order of the
facets and legend, reader-friendly labels, axes breaks, and a smooth
function.**

``` r
# Data transformation 
ca_mc_all <- ca_childcare |> 
  select(study_year, region, mc_infant, mc_toddler, mc_preschool) |> 
  pivot_longer(cols = starts_with("mc"), 
               names_to = "Age_Group", 
               values_to = "Median_Cost") |>
  mutate(Age_Group = factor(Age_Group, levels = c("mc_infant", 
                                                  "mc_toddler", 
                                                  "mc_preschool")))

# Graph
ggplot(ca_mc_all, aes(x = study_year, y = Median_Cost, color = factor(region))) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ Age_Group, labeller = labeller(Age_Group = c(mc_infant = "Infant", 
                                                            mc_toddler = "Toddler", 
                                                            mc_preschool = "Preschool"))) + 
  labs(title = "Weekly Median Price for Center-Based Childcare ($)", 
       x = "Study Year", 
       y = NULL, 
       color = "California Region") + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) + 
  scale_y_continuous(limits = c(100, 500)) + 
  theme_bw() 
```

![](lab-6-student_files/figure-commonmark/recreate-plot-1.png)

``` r
ca_mc_all
```

    # A tibble: 1,914 × 4
       study_year region                 Age_Group    Median_Cost
            <dbl> <fct>                  <fct>              <dbl>
     1       2008 San Francisco Bay Area mc_infant           302.
     2       2008 San Francisco Bay Area mc_toddler          214.
     3       2008 San Francisco Bay Area mc_preschool        214.
     4       2009 San Francisco Bay Area mc_infant           313.
     5       2009 San Francisco Bay Area mc_toddler          234.
     6       2009 San Francisco Bay Area mc_preschool        234.
     7       2010 San Francisco Bay Area mc_infant           313.
     8       2010 San Francisco Bay Area mc_toddler          235.
     9       2010 San Francisco Bay Area mc_preschool        235.
    10       2011 San Francisco Bay Area mc_infant           314.
    # ℹ 1,904 more rows
