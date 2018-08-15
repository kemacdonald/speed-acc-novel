## Plots to sanity check output of eye tracking data processing pipeline
source(here::here("code/helper_functions/libraries_and_functions.R"))
processed_data_path <- "data/03_processed_data/"
file_name <- "speed_acc_novel_timecourse.feather"
df <- read_feather(here::here(processed_data_path, file_name))

# plot to check that fixation locations look reasonable
df %>% 
  sample_frac(size = 0.05) %>% 
  ggplot(aes(x = x, y = y, color = aoi_looking_type), data = .) +
  geom_point(alpha = 0.3, size = 3) +
  xlim(0, 1980) +
  ylim(0, 1080) +
  facet_wrap(~subid) +
  ggthemes::scale_color_ptol() +
  ggthemes::theme_base() 

## plot fixation tiemcourses to see that time processing looks reasonable
all_trials <- df %>% 
  pull(stimulus) %>% 
  unique()

trials_to_plot <- sample(all_trials, size = 1)

df %>%
  filter(stimulus %in% trials_to_plot, 
         t_rel_noun >= 0, t_rel_noun <= 4) %>% 
  ggplot(aes(x = t_rel_noun, y = x, color = target_looking)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  labs(x = "time (ms)", y = "x coordinate") +
  facet_wrap(stimulus~subid, ncol = 4) +
  ggthemes::scale_color_ptol() +
  ggthemes::theme_base() +
  theme(legend.position = "top")
