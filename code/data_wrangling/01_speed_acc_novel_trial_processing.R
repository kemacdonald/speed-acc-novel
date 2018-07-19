# This script takes in raw .xml files that have trial level information for the 
# speed_acc_novel experiment.
# saves a .csv with ROIs and timing information for each trial

## Load libraries
library(here)
library(googlesheets)
source(here::here("code/helper_functions/libraries_and_functions.R"))

## Define path to stimulus log files
read_path <- "data/00_stimuli_information/stimuli_rois_xml" 

## Iterate over trial-level .xml files and extract relevant information using AOIs
files <- dir(here::here(read_path), pattern="*.xml")
rois_df <- files %>% purrr::map_dfr(make_roi_key_value_pairs)
rois_df %<>% mutate(stimulus_name = str_remove(stimulus_name, ".mov|.jpg|.png")) %>% 
  mutate(stimulus_name = str_remove(stimulus_name, "_fixed"))

## extract and save AOI bounding values for left image, right image, and center video
image_aois <- rois_df %>% 
  select(-stimulus_name) %>% 
  distinct() %>% 
  filter(aoi_type == "image")

center_video_aoi <- rois_df %>% 
  filter(aoi_type == "movie") %>% 
  select(-stimulus_name) %>% 
  distinct() %>% 
  filter(aoi_x_min == min(aoi_x_min))

## Read in order sheets
order <- read_csv(here::here("data/00_stimuli_information/experiment_order_sheets/speed_acc_novel_kid_orders - nogaze_gaze_1_gr.csv"))

## Read in timing information for each stimulus item and add to final trial info df
trial_timing_gs <- gs_title("speed_acc_child_adult_ng_info_measurements")
trial_timing_df <- trial_timing_gs %>% gs_read(ws = "novel_nonoise")

# add timing and ROIs to the order sheet
stimuli_info_df <- left_join(order, rois_df, by = "stimulus_name") %>% distinct()

## Write to .csv
write_csv(stimuli_info_df, path = here::here("data/00_stimuli_information/analysis_order_sheets/speed-acc-novel-trial-info.csv"))
