library(here)
library(stringr)
library(tidyverse)

## This code hnadles randomization for the Speed-Acc-Novel experiment

## global variables
set.seed(10)
n_blocks <- 4
n_targets <- 8
n_side_screen <- n_targets / 2
n_orders <- 4
n_lexicons <- 4
n_distractors_block <- 16

# function to create a counterbalanced block
create_block <- function(block_length, strings) {
  n_half <- block_length / length(strings)
  block <- c(replicate(n_half, strings[1]), replicate(n_half, strings[2]))
  sample(block)
}

# function to randomize thing relative to experiment
create_randomized_exp <- function(n_blocks, block_length, strings) {
  rep(create_block(block_length = block_length, strings = strings), n_blocks)
}

## randomize side of screen for the target image
replicate(create_randomized_exp(n_blocks = n_blocks, 
                                block_length = n_targets, 
                                strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_target_side_rand.csv"))

## randomize side of screen for the gaze cue
replicate(create_randomized_exp(n_blocks = 2, 
                                block_length = 4, 
                                strings = c("left", "right")), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_gaze_side_rand.csv"))

## generate the mapping of novel names to novel objects ("lexicons")
novel_obj_imgs <- list.files(here::here("experiment/novel_objects/"))
novel_obj_names <- c("bosa", "pifo", "modi", "toma")
lexicons <- list("lex1", "lex2", "lex3", "lex4")

create_lexicon <- function(lex_name, novel_names, novel_images) {
  data_frame(lex_name = lex_name,
             name = novel_names, 
             image = sample(novel_images, 
                            size = 4, 
                            replace = F))
}

df_lexicons <- purrr::map_dfr(lexicons, create_lexicon, 
                              novel_names = novel_obj_names, 
                              novel_images = novel_obj_imgs)

write_csv(df_lexicons, here::here("experiment/orders/speed_acc_novel_lexicons.csv"))

## randomly sample distractor images 
# sample images without replacement
# removing images that are in the lexicon
distractor_set_lex1 <- setdiff(novel_obj_imgs, 
                               df_lexicons %>% filter(lex_name == "lex1") %>% pull(image))
distractor_set_lex2 <- setdiff(novel_obj_imgs, 
                               df_lexicons %>% filter(lex_name == "lex2") %>% pull(image))
distractor_set_lex3 <- setdiff(novel_obj_imgs, 
                               df_lexicons %>% filter(lex_name == "lex3") %>% pull(image))
distractor_set_lex4 <- setdiff(novel_obj_imgs, 
                               df_lexicons %>% filter(lex_name == "lex4") %>% pull(image))

df_distractors <- data_frame(
  order_lex1 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), 
                 sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex2 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), 
                 sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex3 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), 
                 sample(distractor_set_lex1, size = n_distractors_block, replace = F)),
  order_lex4 = c(sample(distractor_set_lex1, size = n_distractors_block, replace = F), 
                 sample(distractor_set_lex1, size = n_distractors_block, replace = F))
) 

df_distractors %>% write_csv(here::here("experiment/orders/speed_acc_novel_distractor_imgs_rand.csv"))

## carrier phrases
carriers <- c("hey", "look")
replicate(create_randomized_exp(n_blocks = 4, 
                                block_length = 8, 
                                strings = carriers), 
          n = n_orders) %>% 
  as_data_frame() %>% 
  write_csv(., here::here("experiment/orders/speed_acc_novel_carriers_rand.csv"))