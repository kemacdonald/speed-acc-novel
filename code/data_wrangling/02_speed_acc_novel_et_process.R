################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC-NOVEL EXPERIMENT
## read in eye tracking data text files and consolidate them into a single .csv
################################################################################

## PRELIMINARIES
library(here)
source(here::here("code/helper_functions/libraries_and_functions.R"))
raw_data_path <- "data/02_raw_data/pilot_data/"
trial_info_path <- "data/00_stimuli_information/analysis_order_sheets/"

## TODO: double-check these blacklisted roi files 
## I think these are actually the gaze trials, which we seem to not have the 
## correct stimuli_xml files for --> ask elizabeth to upload again
processed_data_path <- "data/03_processed_data/"
calibration_stimulus_name <- "cc3bd7c5c8d457694031c7630357751f"
familiar_stimulus_name <- "ef177c1276275662048244274d3df8cf_1920x1080"
great_job_mov <- "60c3b54031bc9996064b4e3407d26313_1920x1080"

# misc_blacklist <- c("60c3b54031bc9996064b4e3407d26313_1920x1080","67a32392adbd5dcf905315e855d3c988_1920x1080",
#                     "0e779f9026c4158d39f67be74eeef67a_1920x1080","677d254a022e389e3b41d3a3f4572208_1920x1080",
#                     "79ed1ab75aea5a3d969379e466b16fb2_1920x1080","c16406d774cfce622732c8622b464c38_1920x1080",
#                     "6bea988f4e457fbf5542964916522fce_1920x1080","b05c5ce8cfe1528d9fa4cd1b8d8be741_1920x1080",
#                     "149072dff13b88f422e5b3898cb3fdab_1920x1080","890740a06099a589d71ed83e7d6c926c_1920x1080",
#                     "02cd2d10c439c205f28cd62e8906bb37_1920x1080","c7280d7cf8d2071e0a099f77fef07087" )

trial_blacklist <- c("82c3827a23dc8011c3fbcc4d31772ea2_1920x1080.jpg", 
                     calibration_stimulus_name,
                     familiar_stimulus_name,
                     great_job_mov)

## Map read and preprocess functions over participant's data 
## The filter removes any calibration trials
files <- dir(raw_data_path, pattern="*.txt")
all_data <- files[3] %>% purrr::map_dfr(process_et_file, 
                                     file_path = raw_data_path,
                                     x_max = 1920, 
                                     y_max = 1080, 
                                     avg_eyes = TRUE,
                                     sample_rate = 30) %>% 
  mutate(subid = str_trim(subid),
         stimulus = str_remove(stimulus, pattern = ".avi")) %>% 
  filter(!(stimulus %in% trial_blacklist))

# Read stimulus key
df_stimulus_key <- read_csv(here::here(trial_info_path, "speed-acc-stimulus-key.csv"))
all_data %<>% left_join(df_stimulus_key, by = "stimulus")

## Read participant demographics (TODO)
df_demographics <- readxl::read_xlsx(here::here("data/01_participant_logs/elizabeth_child_subject_log.xlsx")) %>% 
  rename(subid = `Subject ID`, order_number = Order) %>% 
  select(-`Child Initials`)

## Join participant demographics, aoi, timing information, and eye tracking data
all_data %<>% left_join(df_demographics, by = "subid")

# Join stimuli metadata with eye tracking data
blah <- left_join(all_data, df_stimulus_key, by = c("stimulus", "order_number"))

## Read AOI information 
df_aois <- read_csv(here::here(aois_path, "speed-acc-novel-aois.csv")) 

# Score each look as left, right, or center based on AOIs
left_image <- df_aois %>% filter(aoi_location == "left")
right_image <- df_aois %>% filter(aoi_location == "right")
center_face <- df_aois %>% filter(aoi_location == "center_face")

all_data %<>% 
  mutate(aoi_looking_type = case_when(
    x <= left_image$aoi_x_max & y <= left_image$aoi_y_max ~ "left",
    x >= right_image$aoi_x_min & y <= left_image$aoi_y_max ~ "right",
    x >= center_face$aoi_x_min & x <= center_face$aoi_x_max & y >= center_face$aoi_y_min ~ "face",
    TRUE ~ "away"
    )
  )

## WRITE DATA TO ZIPPED CSV TO REDUCE FILE SIZE
write_csv(df_final, path=here::here(processed_data_path, "speed_acc_novel_et.csv.gz"))
