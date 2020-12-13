Advent of Code 2020
================

  - [Day 1](#day-1)
  - [Day 2](#day-2)
  - [Day 3](#day-3)
  - [Day 4](#day-4)
  - [Day 5](#day-5)
  - [Day 6](#day-6)
  - [Day 7](#day-7)
  - [Day 8](#day-8)
  - [Day 9](#day-9)
  - [Day 10](#day-10)

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

This sort of problem lends itself to some sort of graph structure. Time
to learn a bit about {igraph}.

``` r
day7_input <- read_lines(here::here("data/day7.txt"))

day7_tidy <- tibble(x = day7_input) %>%
    separate(x, into = c("outside", "inside"), sep = " bags contain ") %>%
    separate_rows(inside, sep = ", ") %>%
    mutate(inside = str_remove(inside, " bags?.?")) %>%
    separate(inside, into = c("weight", "inside"), extra = "merge") %>%
    mutate(weight = str_replace(weight, "no", "0") %>% parse_number())

day7_nodes <- tibble(desc = union(day7_tidy$outside, day7_tidy$inside)) %>%
    rowid_to_column()

day7_edges <- day7_tidy %>%
    left_join(day7_nodes, by = c("outside" = "desc")) %>% 
    rename(to = rowid) %>% 
    left_join(day7_nodes, by = c("inside" = "desc")) %>% 
    rename(from = rowid) %>% 
    filter(!near(weight, 0)) %>% 
    select(from, to, weight)

day7_graph <- tbl_graph(
    nodes = day7_nodes, 
    edges = day7_edges, 
    directed = TRUE
)

shiny_gold_id <- day7_nodes %>% filter(desc == "shiny gold") %>% pull(rowid)
all_simple_paths(
    day7_graph, 
    from = shiny_gold_id, 
    mode = "out"
) %>% 
    map(~ .x %>% as.integer() %>% discard(~ .x == shiny_gold_id)) %>% 
    unlist() %>% 
    unique() %>% 
    length() %>% 
    {
        sprintf("There are %s possible containers", .)
    }
```

    ## [1] "There are 142 possible containers"

## Part 2

Previously I had done this with recursion because I hadn’t set up the
graph correctly, so my answers were off, which made me give up on the
graph thing. I’ve kept that method below for posterity. Switching to a
graph makes it easier and matches the intuition of the problem.

``` r
all_simple_paths(day7_graph, from = shiny_gold_id, mode = "in") %>% 
    map_dbl(
        function(x) {
            x <- unclass(x)
            tibble(to = x[seq_along(x) - 1], 
                   from = x[seq(2, length(x))]) %>% 
                left_join(day7_edges, by = c("to", "from")) %>% 
                pull(weight) %>% 
                prod()
        }
    ) %>% 
    sum() %>% 
    {
        sprintf(
            "Part 2 answer: the shiny gold bag must contain %s other bags", 
            .
        )
    }
```

    ## [1] "Part 2 answer: the shiny gold bag must contain 10219 other bags"

Now we need to count the total number of bags that the `shiny gold` must
contain. Use some recursion for this. Took me a while to get it right,
forgot to add the 1 at the end.

``` r
count_bags <- function(bag_desc) {
    d <- day7_tidy %>% 
        filter(outside == bag_desc)
    
    sum(
        d$weight * 
            (map_dbl(d$inside, count_bags) + 1)
    )
}

sprintf(
    "Part 2 answer: the shiny gold bag must contain %s other bags", 
    count_bags("shiny gold")
)
```

    ## [1] "Part 2 answer: the shiny gold bag must contain 10219 other bags"

# Day 8

## Part 1

Parsing a boot code. Similar to the intcode thing from last year. This
looks like a job for a `while` loop with some bookkeeping. Wrap it in a
function as it’s likely we’ll need it again.

``` r
parse_boot_code
```

    ## function (boot_code) 
    ## {
    ##     i <- 1L
    ##     visited <- i
    ##     total <- length(visited)
    ##     accumulator <- 0L
    ##     while (!any(duplicated(visited)) & i <= nrow(boot_code)) {
    ##         current <- boot_code[i, ]
    ##         cmd <- current$cmd
    ##         value <- current$value
    ##         if (cmd == "acc") {
    ##             accumulator <- accumulator + value
    ##             i <- i + 1L
    ##         }
    ##         else if (cmd == "jmp") {
    ##             i <- i + value
    ##         }
    ##         else {
    ##             i <- i + 1L
    ##         }
    ##         visited <- c(visited, i)
    ##     }
    ##     list(accumulator = accumulator, i = i, visited = visited, 
    ##         total = total)
    ## }

``` r
day8_input <- tibble(x = read_lines(here::here("data/day8.txt"))) %>% 
    separate(x, into = c("cmd", "value"), sep = " ", convert = TRUE)

parse_boot_code(day8_input) %>% 
    pluck("accumulator") %>% 
    {
        sprintf("Part 1 answer: the accumulator is %s", .)
    }
```

    ## [1] "Part 1 answer: the accumulator is 1671"

## Part 2

Now we need to fix the corrupted boot code. In other words: for each of
the `jmp` and `nop` commands in the boot code we need to figure out if
switching it will cause the programme to terminate because it’s gone
past the end of the code (rather than because it’s looped round to
something it already visited).

``` r
to_test <- day8_input %>% 
    rowid_to_column() %>% 
    filter(cmd %in% c("jmp", "nop")) %>% 
    mutate(
        new_cmd = map_chr(cmd, ~ switch(.x, jmp = "nop", nop = "jmp"))
    )

to_test %>% 
    mutate(
        res = map2(
            rowid, 
            new_cmd, 
            function(rowid, new_cmd) {
                tmp <- day8_input
                tmp[rowid, "cmd"] <- new_cmd
                parse_boot_code(tmp)
            }
        )
    ) %>% 
    unnest_wider(res) %>% 
    filter(i > nrow(day8_input)) %>% 
    pull(accumulator) %>% 
    {
        sprintf("Part 2 answer: the accumulator is %s", .)
    }
```

    ## [1] "Part 2 answer: the accumulator is 892"

# Day 9

## Part 1

Finding all combinations of the previous 25 values is nice and quick
with matrices.

``` r
day9_input <- read_lines(here::here("data/day9.txt")) %>% 
    as.double()

preamble <- 25L
found_error <- FALSE
i <- preamble + 1L
while(isFALSE(found_error)) {
    prev <- day9_input[seq(i - preamble, i - 1L)]
    summed <- outer(prev, prev, "+")
    if (day9_input[i] %in% summed) {
        i <- i + 1L
    } else {
        found_error <- TRUE
    }
}
day9_part1_ans <- day9_input[i]
sprintf("Part 1 answer is %s", day9_part1_ans)
```

    ## [1] "Part 1 answer is 1492208709"

## Part 2

We get told that the range is *contiguous*, so we can loop over the
vector just once and keep a running total. If it’s too small just raise
the upper bound for the range; if it’s too large raise the lower bound.

``` r
lower <- 1
upper <- 1
total <- sum(day9_input[seq(from = lower, to = upper)])
while (total != day9_part1_ans) {
    if (total < day9_part1_ans) {
        upper <- upper + 1
    } else {
        lower <- lower + 1
    }
    total <- sum(day9_input[seq(from = lower, to = upper)])
}
sprintf(
    "Part 2 answer is %s", 
    sum(range(day9_input[lower:upper]))
)
```

    ## [1] "Part 2 answer is 238243506"

# Day 10

## Part 1

This first part is just about counting how many of the jolts have a
`diff()` of 1 or 3. Can quickly check the table of all the diffs:

``` r
day10_input <- read_lines(here::here("data/day10.txt")) %>% 
    as.integer() %>% 
    {
        c(., 0L, max(.) + 3L)
    } %>% 
    sort()

table(diff(day10_input))
```

    ## 
    ##  1  3 
    ## 64 32

There are only diffs of 1 and 3, which simplifies things.

``` r
sprintf("Part 1 answer is %s", prod(table(diff(day10_input))))
```

    ## [1] "Part 1 answer is 2048"

## Part 2

Counting all of the combinations is a non-starter. We know from Part 1
though that there are only diffs of 1 and 3 in the data. And what is the
longest run of 1’s in the data?

``` r
runs <- rle(diff(day10_input))
max(runs$lengths[runs$values == 1])
```

    ## [1] 4

So then we just multiply each run of ones by the number of ways to
rearrange them to make up the length of the run. So if there’s a length
3 run of 1’s you could rearrange them in 4 ways. There’s 1 way to make 1
from 1’s; 2 ways to make 2 (1 + 1 or 2); 4 ways to make 3; and 7 ways to
make 7.

``` r
runs$lengths[runs$values == 1] %>% 
    as.integer() %>% 
    {
        c(1, 2, 4, 7)[.]
    } %>% 
    prod() %>% 
    as.character() %>% 
    {
        sprintf("Part 2 answer is %s", .)
    }
```

    ## [1] "Part 2 answer is 1322306994176"
