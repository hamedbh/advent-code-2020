Advent of Code 2020
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)
  - [Day 3](#day-3)
  - [Day 4](#day-4)

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

# Day 3

## Part 1

Need to navigate the map and count the number of trees we hit travelling
**down 1, across 3** at every step. The same map repeats infinitely to
the right.

``` r
# Save the 'raw' input: partly because its dimensions will be useful when
# creating the map, partly because I'm bound to need it again for Part 2
day03_input <- readLines(here::here("data/day03.txt"))

day03_map <- day03_input %>% 
    map(
        ~ .x %>% 
            str_split("") %>% 
            pluck(1)
    ) %>% 
    unlist() %>% 
    matrix(nrow = length(day03_input), byrow = TRUE)
```

``` r
count_trees <- function(map_matrix, step_down, step_right) {
    
    # Easier to get the indices we will need separately
    rows <- seq(1, nrow(map_matrix), by = step_down)
    columns <- 
        seq(1, by = step_right, length.out = length(rows)) %% ncol(map_matrix)
    # Replacing the zeroes is maybe a bit hacky but this is the price we pay for
    # indexing from 1 and being unwilling to spend time on figuring out a more
    # elegant solution with modular arithmetic
    columns[columns == 0L] <- ncol(day03_map)
    
    map2_chr(rows, columns, ~ map_matrix[.x, .y]) %>% 
        str_c(collapse = "") %>% 
        str_count("#")
}
```

``` r
sprintf("Part 1 answer is %s trees", count_trees(day03_map, 1, 3))
```

    ## [1] "Part 1 answer is 198 trees"

## Part 2

Now need to check a number of trajectories and then return the product
of the result.

``` r
down <-  c(1, 1, 1, 1, 2)
right <- c(1, 3, 5, 7, 1)

map2_int(
    down, 
    right, 
    ~ count_trees(day03_map, .x, .y)
) %>% 
    prod() %>% 
    {
        sprintf("Part 2 answer is %s", .)
    }
```

    ## [1] "Part 2 answer is 5140884672"

# Day 4

## Part 1

Data validation. This is a bit of a busman’s holiday but off we go.

``` r
# get the required fields
req <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
day04_input <- tibble(x = read_lines(here::here("data/day04.txt")))

day04_passports <- day04_input %>% 
    mutate(id = cumsum(x == "")) %>% 
    mutate(pairs = str_match_all(x, "(...)\\:([^ ]+)")) %>% 
    mutate(
        key = map(pairs, ~ .x[, 2]), 
        value = map(pairs, ~ .x[, 3])
    ) %>% 
    select(-c(x, pairs)) %>% 
    unnest(c(key, value))
```

Now we have the passports cleaned up it’s just grouping and counting.

``` r
day04_valid <- day04_passports %>% 
    group_by(id) %>% 
    filter(all(req %in% c(key)))

sprintf("Part 1 answer is %s valid passports", 
        length(unique(day04_valid$id)))
```

    ## [1] "Part 1 answer is 196 valid passports"

## Part 2

Now need to validate the values.

``` r
day04_valid %>% 
    # make sure we don't have any stray spaces messing things up in the value 
    # field
    mutate(across(where(is.character), str_trim)) %>% 
    pivot_wider(names_from = "key") %>% 
    # now filter the records
    filter(
        between(as.integer(byr), 1920L, 2002L),
        between(as.integer(iyr), 2010L, 2020L), 
        between(as.integer(eyr), 2020L, 2030L),
        
        (str_detect(hgt, "cm") & between(parse_number(hgt), 150, 193)) | 
            (str_detect(hgt, "in") & between(parse_number(hgt), 59, 76)), 
        
        str_detect(hcl, "^\\#[a-f0-9]{6}$"), 
        
        ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"), 
        
        str_detect(pid, "^\\d{9}$")
    ) %>% 
    pull(id) %>% 
    length() %>% 
    {
        sprintf("Part 2 answer is %s", .)
    }
```

    ## [1] "Part 2 answer is 114"
