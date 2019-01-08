# generate random order list for speed-acc-novel
set.seed(7)
orders <- c("gaze-1", "gaze-2", "straight-ahead-1", "straight-ahead-2")
orders_all <- rep(orders, 13)
randomized <- sample(orders_all, size = length(orders_all), replace = F)
randomized %>% as_data_frame() %>% write_csv(path = "~/Desktop/speed-acc-novel0v2-order-list.csv")
