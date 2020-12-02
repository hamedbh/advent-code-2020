test_password_OTCP <- function(policy, letter, password) {
    policy_num <- str_split(policy, pattern = "-") %>% 
        pluck(1) %>% 
        as.integer()
    
    first <- str_sub(password, start = policy_num[[1]], end = policy_num[[1]])
    second <- str_sub(password, start = policy_num[[2]], end = policy_num[[2]])
    
    xor(
        first == letter, 
        second == letter
    )
}
