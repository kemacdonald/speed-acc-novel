## Plots to sanity check output of eye tracking data processing pipeline
source(here::here("code/helper_functions/libraries_and_functions.R"))
processed_data_path <- "data/03_processed_data/"
df <- read_feather(here::here(processed_data_path, "speed_acc_novel_timecourse.feather"))

# plot to check that fixation locations look reasonable
df %>% 
  sample_frac(size = 0.10) %>% 
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
         t.stim >= 0, t.stim <= 5) %>% 
  ggplot(aes(x = t.stim, y = x, color = aoi_looking_type)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  labs(x = "time (ms)", y = "x coordinate") +
  facet_grid(stimulus~subid) +
  ggthemes::scale_color_ptol() +
  ggthemes::theme_base()
