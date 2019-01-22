# This script takes in raw .xml files that have trial level information for the 
# speed_acc_novel experiment.
# saves a .csv with ROIs and timing information for each trial

## Load libraries
library(googlesheets)
source(here::here("code/helper_functions/libraries_and_functions.R"))

## Which version of experiment? 
exp_version <- "v1"

## Define path to stimulus log files and write path for analysis metadata
stimuli_logs_path <- file.path("data/00_stimuli_information/stimulus_logs", paste0(exp_version, "_logs"))  
read_path <- file.path("data/00_stimuli_information/stimuli_rois_xml", paste0(exp_version, "_rois"))
exp_order_sheet_path <- "data/00_stimuli_information/experiment_order_sheets/"
write_path <- "data/00_stimuli_information/analysis_order_sheets/"

## Make stimuli key
df_stimulus_key <- process_log_files(stimuli_logs_path, exp_version = exp_version) %>% 
  filter(stimulus_type == "Composite",
         !(is.na(trial_num_exp))) %>% 
  mutate(trial_num_exp = as.integer(trial_num_exp)) %>% 
  filter(trial_num_exp > 0)    # handles the case of zero-numbered familiarization trials

## check how many trials we have for each order
df_stimulus_key %>% count(order_age, order_name) %>% kable()

## save stimulus key
write_csv(df_stimulus_key, path = here::here(write_path, paste0("speed-acc-novel-stimulus-key-", exp_version, ".csv")))

## Make AOI key
# Iterate over trial-level .xml files and extract relevant information using AOIs
files <- dir(here::here(read_path), pattern="*.xml")
rois_df <- files %>% purrr::map_dfr(make_roi_key_value_pairs)
rois_df %<>% mutate(stimulus_name = str_remove(stimulus_name, ".mov|.jpg|.png")) %>% 
  mutate(stimulus_name = str_remove(stimulus_name, "_fixed")) 

## extract and save AOI bounding values for left image, right image, and center video
image_aois <- rois_df %>% 
  select(-stimulus_name, -stimulus) %>% 
  distinct() %>% 
  filter(aoi_type == "image")

center_video_aoi <- rois_df %>% 
  filter(aoi_type == "movie") %>% 
  select(-stimulus_name, -stimulus) %>% 
  distinct() %>% 
  filter(aoi_x_min == min(aoi_x_min))

## save aoi information 
final_aois <- bind_rows(image_aois, center_video_aoi)
write_csv(final_aois, here::here(write_path, paste0("speed-acc-novel-aois-", exp_version, ".csv")))

## Read in experiment order sheet from google sheets
## Tidies up and stores order information in local project repo
download_orders_gs(sheet_name = "speed_acc_novel_kid_orders",exp_version = "v1", exp_order_sheet_path)
download_orders_gs(sheet_name = "speed_acc_novel_kid_orders_version_2", exp_version = "v2", exp_order_sheet_path)

## Read and combine all order sheets
order_files <- list.files(file.path(exp_order_sheet_path, paste0(exp_version, "_orders")), full.names = T)

df_orders <- order_files[1:4] %>% 
  purrr::map_dfr(read_csv) %>% 
  mutate(trial_num_exp = as.character(trial_num_exp)) %>% 
  mutate(stimulus_name = str_remove(stimulus_name, "_audio"))

## Read in timing information for each stimulus item from google sheets
trial_timing_gs <- gs_title("speed_acc_child_adult_ng_info_measurements")
trial_timing_gs %>% gs_download(ws = "novel_nonoise", 
                                to = file.path(exp_order_sheet_path, "speed_acc_novel_trial_timing.csv"), 
                                overwrite = T)

trial_timing_df <- read_csv(here::here(exp_order_sheet_path, "speed_acc_novel_trial_timing.csv")) %>% 
  select(-speaker) 

#### PULL EVERYTHING TOGETHER

## TODO: this still doesn't work for v2 stimuli 

# timing information and stimulus key
stimuli_info_df <- left_join(df_orders, trial_timing_df, by = "stimulus_name") %>% 
  mutate(trial_num_exp = as.integer(trial_num_exp))

df_final <- left_join(stimuli_info_df, 
                      df_stimulus_key, 
                      by = c("stimulus_name", "trial_num_exp", "order_name")) %>% 
  select(stimulus, everything()) 

# check how many trials we have in the key for each order and age group
df_final %>% count(order_age, order_name) %>% kable()

## Write final stimuli information metadata to .csv
write_csv(df_final, path = here::here(write_path, 
                                      paste0("speed-acc-novel-analysis-order-sheet-", exp_version, ".csv"))
          )
