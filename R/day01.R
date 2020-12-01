# Part 2 ------------------------------------------------------------------

# Find the two entries that sum to 2020 and return their product
library(tidyverse)
readLines(here::here("data/day01.txt")) %>% 
    as.integer() %>% 
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


# Part 2 ------------------------------------------------------------------


readLines(here::here("data/day01.txt")) %>% 
    as.double() %>% 
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
