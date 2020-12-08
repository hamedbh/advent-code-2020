parse_boot_code <- function(boot_code) {
    # Need to keep track of several things: 
    # - the index of the current instruction
    # - which instructions we've visited (and whether there are any dupes)
    # - how many instructions we've visited
    # - the accumulator
    i <- 1L
    visited <- i
    total <- length(visited)
    accumulator <- 0L
    
    while (!any(duplicated(visited)) & i <= nrow(boot_code)) {
        current <- boot_code[i, ]
        cmd <- current$cmd
        value <- current$value
        if (cmd == "acc") {
            accumulator <- accumulator + value
            i <- i + 1L
        } else if (cmd == "jmp") {
            i <- i + value
        } else {
            i <-  i + 1L
        }
        visited <- c(visited, i)
    }
    list(
        accumulator = accumulator, 
        i = i, 
        visited = visited, 
        total = total
    )
}
