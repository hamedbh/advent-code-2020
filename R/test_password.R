test_password <- function(policy, letter, password) {
    policy_num <- str_split(policy, pattern = "-") %>% 
        pluck(1) %>% 
        as.integer()
    
    letter_count <- str_count(password, letter)
    
    letter_count >= policy_num[[1]] & letter_count <= policy_num[[2]]
}
