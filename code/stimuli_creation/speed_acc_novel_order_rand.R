## randomize stuff within a learning block
n_blocks <- 4
n_targets <- 8
n_side_screen <- n_targets / 2

# create side of screen vector for each black
side_vect_block <- c(replicate(n_side_screen, "l"), replicate(n_side_screen, "r"))
side_vect_exp <- rep(sample(side_vect_block), n_blocks)
