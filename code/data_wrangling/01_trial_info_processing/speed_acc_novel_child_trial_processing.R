# This script takes in raw .xml files that have trial level information for the 
# speed_acc_novel experiment.
# saves a .csv with ROIs and timing information for each trial

##Load libraries
library(here)
source(here::here("code/helper_functions/libraries_and_functions.R"))

### Define global variables
read_path <- "data/0b_stimuli_information" 

## Read in stimulus log .xml to get stimulus id tag and source name
make_stim_key_value_pairs <- function(xml_obj) {
  tibble(
    key = xml_obj[['GUID']],
    name = xml_obj[['Name']]
  )
}

stim_log_files <- dir(here::here(read_path, "kids/kid_stim_logs/"), 
                 pattern = "*.xml")

stim_log_df <- xmlParse(here::here(read_path, "kids/kid_stim_logs/", stim_log_files[1])) %>% 
  xmlToList() %>% 
  flatten() %>% 
  purrr::map_dfr(make_stim_key_value_pairs)

## Iterate over trial-level .xml files and extract relevant information using ROIs
process_one_stim_xml <- function(xml_obj) {
  tibble(
    stimulus_name = xml_obj$Tag,
    aoi_x_min = xml_obj[['Points']][[1]]$X,
    aoi_y_min = xml_obj[['Points']][[1]]$Y,
    aoi_x_max = xml_obj[['Points']][[2]]$X,
    aoi_y_max = xml_obj[['Points']][[2]]$Y
  ) %>% 
    mutate(aoi_type = ifelse(str_detect(stimulus_name, ".jpg|.png"), "image", "movie"),
           aoi_location = case_when (
             aoi_x_min == 0 ~ 'left',
             aoi_y_min == 0 ~ 'center_face',
             aoi_x_max == 3072 ~ 'right'
           ))
} 

make_roi_key_value_pairs <- function(file) {
  print(file)
  xml_list <- xmlParse(here::here(read_path, "kids/kid_xml_rois/", file)) %>% 
    xmlToList()
  
  xml_list %>% purrr::map_dfr(process_one_stim_xml)
}

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
