Advent of Code 2020
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)

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

# Day 2

## Part 1

Count how many of the passwords are valid. Created a function to compare
a password and policy and test if it’s valid.

``` r
test_password
```

    ## function (policy, letter, password) 
    ## {
    ##     policy_num <- str_split(policy, pattern = "-") %>% pluck(1) %>% 
    ##         as.integer()
    ##     letter_count <- str_count(password, letter)
    ##     letter_count >= policy_num[[1]] & letter_count <= policy_num[[2]]
    ## }

``` r
day02_passwords <- read_delim(
    here::here("data/day02.txt"), 
    delim = " ", 
    col_names = c("policy", "letter", "password"), 
    col_types = "ccc"
) %>% 
    # get rid of the trailing colon on the letter
    mutate(
        letter = str_remove(letter, ":")
    )
day02_passwords %>% 
    mutate(valid = pmap_lgl(., test_password)) %>% 
    filter(valid) %>% 
    {
        sprintf("Part 1 answer is %s", nrow(.))
    }
```

    ## [1] "Part 1 answer is 625"

## Part 2

Now we need a different version of the password testing function that
will comply with the Official Toboggan Corporate Policy (OTCP).

``` r
test_password_OTCP
```

    ## function (policy, letter, password) 
    ## {
    ##     policy_num <- str_split(policy, pattern = "-") %>% pluck(1) %>% 
    ##         as.integer()
    ##     first <- str_sub(password, start = policy_num[[1]], end = policy_num[[1]])
    ##     second <- str_sub(password, start = policy_num[[2]], end = policy_num[[2]])
    ##     xor(first == letter, second == letter)
    ## }

``` r
day02_passwords %>% 
    mutate(valid = pmap_lgl(., test_password_OTCP)) %>% 
    filter(valid) %>% 
    {
        sprintf("Part 2 answer is %s", nrow(.))
    }
```

    ## [1] "Part 2 answer is 391"
