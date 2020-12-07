Advent of Code 2020
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)
  - [Day 3](#day-3)
  - [Day 4](#day-4)
  - [Day 5](#day-5)
  - [Day 6](#day-6)
  - [Day 7](#day-7)

Here’s my work on Advent of Code 2020. Let’s see if I do more than my
usual thing of getting halfway and then not finding time for the rest\!

# Day 1

## Part 1

First we need the product of the two numbers in the expense report that
add to 2020.

``` r
day1_expense_report <- readLines(here::here("data/day1.txt")) %>% 
    as.double()

day1_expense_report %>% 
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
    prod() %>% 
    {
        sprintf("Part 1 answer is %s", .)
    }
```

    ## [1] "Part 1 answer is 889779"

## Part 2

Now do the same but for the **three** numbers that add to 2020.

``` r
day1_expense_report %>% 
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
day2_passwords <- read_delim(
    here::here("data/day2.txt"), 
    delim = " ", 
    col_names = c("policy", "letter", "password"), 
    col_types = "ccc"
) %>% 
    # get rid of the trailing colon on the letter
    mutate(
        letter = str_remove(letter, ":")
    )
day2_passwords %>% 
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
day2_passwords %>% 
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
day3_input <- readLines(here::here("data/day3.txt"))

day3_map <- day3_input %>% 
    map(
        ~ .x %>% 
            str_split("") %>% 
            pluck(1)
    ) %>% 
    unlist() %>% 
    matrix(nrow = length(day3_input), byrow = TRUE)
```

``` r
count_trees
```

    ## function (map_matrix, step_down, step_right) 
    ## {
    ##     rows <- seq(1, nrow(map_matrix), by = step_down)
    ##     columns <- seq(1, by = step_right, length.out = length(rows))%%ncol(map_matrix)
    ##     columns[columns == 0L] <- ncol(map_matrix)
    ##     map2_chr(rows, columns, ~map_matrix[.x, .y]) %>% str_c(collapse = "") %>% 
    ##         str_count("#")
    ## }

``` r
sprintf("Part 1 answer is %s trees", count_trees(day3_map, 1, 3))
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
    ~ count_trees(day3_map, .x, .y)
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
day4_input <- tibble(x = read_lines(here::here("data/day4.txt")))

day4_passports <- day4_input %>% 
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
day4_valid <- day4_passports %>% 
    group_by(id) %>% 
    filter(all(req %in% c(key)))

sprintf("Part 1 answer is %s valid passports", 
        length(unique(day4_valid$id)))
```

    ## [1] "Part 1 answer is 196 valid passports"

## Part 2

Now need to validate the values.

``` r
day4_valid %>% 
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

# Day 5

## Part 1

The problem uses binary partitioning to find a seat on the plane. The
row numbers get higher as they go from the `F`ront to the `B`ack; column
numbers start low on the `L`eft and are higher on the `R`ight.

Can do this by converting to binary.

``` r
parse_seat_spec
```

    ## function (seat_spec) 
    ## {
    ##     seat_spec <- seat_spec %>% str_replace_all("B|R", "1") %>% 
    ##         str_replace_all("F|L", "0")
    ##     row <- strtoi(str_sub(seat_spec, 1, 7), base = 2)
    ##     col <- strtoi(str_sub(seat_spec, 8, 10), base = 2)
    ##     list(row = row, col = col)
    ## }

Now point this at today’s input.

``` r
day5_specs <- tibble(seat_spec = read_lines(here::here("data/day5.txt")))

day5_seats <- day5_specs %>% 
    mutate(combined = map(seat_spec, parse_seat_spec)) %>% 
    unnest_wider(combined) %>% 
    mutate(seat_id = map2_int(row, col, ~ (8L * .x) + .y))

sprintf("Part 1 answer: highest seat ID is %s", max(day5_seats$seat_id))
```

    ## [1] "Part 1 answer: highest seat ID is 919"

## Part 2

Need to find my seat, which is the only one missing. I know that mine
won’t be the lowest or highest seat ID.

``` r
seq(min(day5_seats$seat_id), max(day5_seats$seat_id)) %>% 
    setdiff(day5_seats$seat_id) %>% 
    {
        sprintf("Part 2 answer: my seat id is %s", .)
    }
```

    ## [1] "Part 2 answer: my seat id is 642"

# Day 6

## Part 1

Need counts for how many distinct questions were answered yes by anyone
in each group. Or: the union of all the sets of answers.

``` r
day6_answers <- tibble(x = read_lines(here::here("data/day6.txt"))) %>% 
    mutate(group_id = cumsum(x == "")) %>% 
    filter(x != "") %>% 
    add_count(group_id, name = "group_size") %>% 
    mutate(split_x = str_split(x, ""))

day6_answers %>% 
    group_by(group_id) %>% 
    summarise(all_yes = reduce(split_x, union), .groups = "drop") %>% 
    nrow() %>% 
    {
        sprintf("Part 1 answer is %s", .)
    }
```

    ## [1] "Part 1 answer is 6351"

## Part 2

Now need counts for how many questions were answered yes by *everyone*
in each group. Just replace `union()` with `intersect()`.

``` r
day6_answers %>% 
    group_by(group_id) %>% 
    summarise(all_yes = reduce(split_x, intersect), .groups = "drop") %>% 
    nrow() %>% 
    {
        sprintf("Part 2 answer is %s", .)
    }
```

    ## [1] "Part 2 answer is 3143"

# Day 7

## Part 1

``` r
day7_input <- read_lines(here::here("data/day7.txt"))
day7_tidy <- tibble(x = day7_input) %>%
    separate(x, into = c("outside", "inside"), sep = " contain ") %>% 
    separate_rows(inside, sep = ", ") %>% 
    filter(!(str_detect(inside, "no other bag"))) %>% 
    mutate(inside_bag_count = parse_number(inside)) %>% 
    mutate(
        outside = str_match(outside, "([a-z]+ [a-z]+) bag")[, 2], 
        inside  = str_match(inside, "^\\d+ ([a-z]+ [a-z]+) bag")[, 2]
    )

day7_tidy %>% 
    select(from = inside, to = outside) %>% 
    graph_from_data_frame() %>% 
    all_simple_paths(
        from = "shiny gold", 
        mode = "out"
    ) %>% 
    map(~ attr(.x, "names") %>% str_subset("shiny gold", negate = TRUE)) %>% 
    unlist() %>% 
    unique() %>% 
    length() %>% 
    {
        sprintf("There are %s possible containers", .)
    }
```

    ## [1] "There are 142 possible containers"

## Part 2

Now we need to count the total number of bags that the `shiny gold` must
contain. Use some recursion for this. Took me a while to get it right,
forgot to add the 1 at the end.

``` r
count_bags <- function(bag_desc) {
    d <- day7_tidy %>% 
        filter(outside == bag_desc)
    
    sum(
        d$inside_bag_count * 
            (map_dbl(d$inside, count_bags) + 1)
    )
}

sprintf(
    "Part 2 answer: the shiny gold bag must contain %s other bags", 
    count_bags("shiny gold")
)
```

    ## [1] "Part 2 answer: the shiny gold bag must contain 10219 other bags"
