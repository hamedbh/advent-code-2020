count_trees <- function(map_matrix, step_down, step_right) {
    
    # Easier to get the indices we will need separately
    rows <- seq(1, nrow(map_matrix), by = step_down)
    columns <- 
        seq(1, by = step_right, length.out = length(rows)) %% ncol(map_matrix)
    # Replacing the zeroes is maybe a bit hacky but this is the price we pay for
    # indexing from 1 and being unwilling to spend time on figuring out a more
    # elegant solution with modular arithmetic
    columns[columns == 0L] <- ncol(map_matrix)
    
    map2_chr(rows, columns, ~ map_matrix[.x, .y]) %>% 
        str_c(collapse = "") %>% 
        str_count("#")
}
