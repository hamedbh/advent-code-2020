Advent of Code 2020
================

  - [Day 1](#day-1)

<!-- badges: start -->

[![R build
status](https://github.com/hamedbh/advent-code-2020/workflows/R-CMD-check/badge.svg)](https://github.com/hamedbh/advent-code-2020/actions)
<!-- badges: end -->

Here’s the work on Advent of Code 2020. Let’s see if I do more than my
usual thing of getting halfway and then not finding time for the rest\!

# Day 1

## Part 1

First we need the product of the two numbers in the expense report that
add to 2020.

``` r
day01_expense_report <- readLines(here::here("data/day01.txt")) %>% 
    as.double()

day01_expense_report %>% 
    {
        expand_grid(
            num1 = ., 
            num2 = .
        )
    } %>% 
    filter(
        # Get rid of rows where a number is paired with itself, and dedupe, by 
        # requiring that num1 < num2
        num1 < num2, 
        # Keep just the row with the right sum
        near(num1 + num2, 2020)
    ) %>% 
    pivot_longer(cols = everything()) %>% 
    pull(value) %>% 
    reduce(`*`) %>% 
    {
        sprintf("Part 1 answer is %s", .)
    }
```

    ## [1] "Part 1 answer is 889779"

## Part 2

Now do the same but for the **three** numbers that add to 2020.

``` r
day01_expense_report %>% 
    {
        expand_grid(
            num1 = ., 
            num2 = ., 
            num3 = .
        )
    } %>% 
    filter(
        # Get rid of rows where a number is paired with itself, and dedupe, by 
        # requiring that num1 < num2
        num1 < num2, 
        num2 < num3, 
        # Keep just the row with the right sum
        near(num1 + num2 + num3, 2020)
    ) %>% 
    pivot_longer(cols = everything()) %>% 
    pull(value) %>% 
    reduce(`*`) %>% 
    {
        sprintf("Part 2 answer is %s", .)
    }
```

    ## [1] "Part 2 answer is 76110336"
