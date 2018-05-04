library(here)
library(tidyverse)

## randomize stuff within a learning block

## global variables
set.seed(10)
n_blocks <- 4
n_targets <- 8
n_side_screen <- n_targets / 2
n_orders <- 4

# create a counterbalanced block
create_block <- function(block_length, strings) {
  n_half <- block_length / length(strings)
  block <- c(replicate(n_half, strings[1]), replicate(n_half, strings[2]))
  sample(block)
}

# randomize thing relative to experiment
create_randomized_exp <- function(n_blocks, block_length, strings) {
  rep(create_block(block_length = block_length, strings = strings), n_blocks)
}


## side of screen for the target
replicate(create_randomized_exp(n_blocks = 4, block_length = 8, strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_target_side_rand.csv"))

## side of screen for the target
replicate(create_randomized_exp(n_blocks = 2, block_length = 4, strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_gaze_side_rand.csv"))


## distractor image name

## carrier phrase

## speaker