library(here)
source(here::here("code/helper_functions/libraries_and_functions.R"))
processed_data_path <- "data/03_processed_data/"

all_data <- read_csv(here::here(processed_data_path, "speed_acc_novel_et.csv.gz"))

# plot to check that fixation locations look reasonable
all_data %>% 
  sample_frac(size = 0.20) %>% 
  ggplot(aes(x = x, y = y), data = .) +
  geom_point(alpha = 0.3, size = 3) +
  xlim(0, 1980) +
  ylim(0, 1080) +
  facet_wrap(~subid) +
  ggthemes::theme_base()

## plot fixation tiemcourses to see that time processing looks reasonable
all_trials <- all_data %>% 
  filter(str_detect(stimulus, pattern = ".avi")) %>% 
  pull(stimulus) %>% 
  unique()

trials_to_plot <- sample(all_trials, size = 1)

all_data %>%
  filter(stimulus %in% trials_to_plot, 
         t.stim >= 0, t.stim <= 5,
         subid == "SAN-071218-02") %>% 
  ggplot(aes(x = t.stim, y = x)) +
  geom_point() +
  labs(x = "time (ms)", y = "x coordinate") +
  facet_grid(stimulus~subid) +
  ggthemes::theme_base()
