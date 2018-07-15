# This script takes in raw .xml files that have trial level information for the 
# speed_acc_novel experiment.
# saves a .csv with ROIs and timing information for each trial

##Load libraries
library(here)
source(here::here("code/helper_functions/libraries_and_functions.R"))

### Define path to stimulus log files
read_path <- "data/0b_stimuli_information/stimuli_rois_xml/" 

## Read in stimulus log .xml files
stim_log_files <- dir(here::here(read_path), pattern = "*.xml")

stim_log_df <- xmlParse(here::here(read_path, stim_log_files[1])) %>% 
  xmlToList() %>% 
  flatten() %>% 
  purrr::map_dfr(make_stim_key_value_pairs)

## Iterate over trial-level .xml files and extract relevant information using ROIs
files <- dir(here::here(read_path, "kids/kid_xml_rois/"), 
             pattern="*.xml")

rois_df <- files %>% purrr::map_dfr(make_roi_key_value_pairs)

####### Read in timing information for each stimulus item and add to final trial info df
library(googlesheets)

trial_timing_gs <- gs_title("speed_acc_child_adult_ng_info_measurements")
trial_timing_grace <- trial_timing_gs %>% gs_read(ws = "grace")
trial_timing_olivia <- trial_timing_gs %>% gs_read(ws = "olivia")

trial_timing_df <- bind_rows(trial_timing_grace, trial_timing_olivia)

# join timing information with the ROI information
rois_df %<>% mutate(stimulus_name = str_remove(stimulus_name, ".mov|.jpg|.png"))
rois_df %<>% left_join(trial_timing_df, by = "stimulus_name") 

####### Write to .csv
write_csv(rois_df, path = here::here("data/0b_stimuli_information/kids/speed-acc-novel-kid-trial-info.csv"))
