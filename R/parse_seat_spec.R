parse_seat_spec <- function(seat_spec) {
    
    seat_spec <- seat_spec %>% 
        str_replace_all("B|R", "1") %>% 
        str_replace_all("F|L", "0")
    
    row <- strtoi(str_sub(seat_spec, 1, 7), base = 2)
    col <- strtoi(str_sub(seat_spec, 8, 10), base = 2)
    
    list(
        row = row, 
        col = col
    )
}
