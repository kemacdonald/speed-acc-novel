library(here)
library(stringr)
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
replicate(create_randomized_exp(n_blocks = n_blocks, 
                                block_length = n_targets, 
                                strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_target_side_rand.csv"))

## side of screen for the target
replicate(create_randomized_exp(n_blocks = 2, 
                                block_length = 4, 
                                strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_gaze_side_rand.csv"))

## novel names to objects
# create two lexicons that we will vary across the 
# four orders
novel_obj_imgs <- list.files(here::here("experiment/novel_objects/"))
novel_obj_names <- c("bosa", "pifo", "modi", "toma")

lex_1 <- data_frame(name = novel_obj_names, 
                    image = sample(novel_obj_imgs, size = 4, 
                                   replace = F))

lex_2 <- data_frame(name = novel_obj_names, 
                    image = sample(novel_obj_imgs, size = 4, 
                                   replace = F))

## distractor images 
# need to sample images randomly without replacement
# removing images in the lexicon
distractor_set_lex1 <- setdiff(novel_obj_imgs, lex_1$image)
distractor_set_lex2 <- setdiff(novel_obj_imgs, lex_2$image)
n_distractors_block <- 16

data_frame(
  order_lex1_1 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex1_2 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex2_1 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex2_2 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), sample(distractor_set_lex1, size = n_distractors_block, replace = F))
) %>% 
  write_csv(here::here("experiment/orders/speed_acc_novel_distractor_imgs_rand.csv"))

## carrier phrases
carriers <- c("hey", "look")
replicate(create_randomized_exp(n_blocks = 4, 
                                block_length = 8, 
                                strings = carriers), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_carriers_rand.csv"))