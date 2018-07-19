################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC-NOVEL EXPERIMENT
## read in eye tracking data text files and consolidate them into a single .csv
################################################################################

## PRELIMINARIES
library(here)
source(here::here("code/helper_functions/libraries_and_functions.R"))
raw_data_path <- "data/02_raw_data/pilot_data/"
processed_data_path <- "data/03_processed_data/"

## Map read and preprocess functions over the files 
files <- dir(raw_data_path, pattern="*.txt")
all_data <- files %>% purrr::map_dfr(process_et_file, 
                                        file_path = raw_data_path,
                                        x_max = 1920, 
                                        y_max = 1080, 
                                        avg_eyes = TRUE,
                                        sample_rate = 30) %>% 
  mutate(subid = str_trim(subid))

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

## WRITE DATA OUT TO ZIPPED CSV FOR EASY ACCESS AND SMALL FILE SIZE
write_csv(all_data, path=here::here(processed_data_path, "speed_acc_novel_et.csv.gz"))
