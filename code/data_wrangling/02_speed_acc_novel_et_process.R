################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC-NOVEL EXPERIMENT
## read in eye tracking data text files and consolidate them into a single .csv
################################################################################

## PRELIMINARIES
source(here::here("code/helper_functions/libraries_and_functions.R"))
raw_data_path <- "data/02_raw_data/"
trial_info_path <- "data/00_stimuli_information/analysis_order_sheets/"
aois_path <- "data/00_stimuli_information/analysis_order_sheets/speed-acc-novel-aois.csv"

## blacklisted roi files 
processed_data_path <- "data/03_processed_data/"
calibration_stimulus_name <- c("cc3bd7c5c8d457694031c7630357751f", "c7280d7cf8d2071e0a099f77fef07087")
familiar_stimulus_name <- c("ef177c1276275662048244274d3df8cf_1920x1080", "67a32392adbd5dcf905315e855d3c988_1920x1080")
great_job_mov <- c("60c3b54031bc9996064b4e3407d26313_1920x1080", "02cd2d10c439c205f28cd62e8906bb37_1920x1080")
black_image <- "82c3827a23dc8011c3fbcc4d31772ea2_1920x1080.jpg"
trial_blacklist <- c(black_image, 
                     calibration_stimulus_name,
                     familiar_stimulus_name,
                     great_job_mov)

## Map read and preprocess functions over participant's data 
## The filter removes any calibration trials
files <- dir(raw_data_path, pattern="*.txt")
all_data <- files %>% purrr::map_dfr(process_et_file, 
                                     file_path = raw_data_path,
                                     x_max = 1920, 
                                     y_max = 1080, 
                                     avg_eyes = TRUE,
                                     sample_rate = 30) %>% 
  mutate(subid = str_trim(subid),
         stimulus = str_remove(stimulus, pattern = ".avi")) %>% 
  filter(!(stimulus %in% trial_blacklist))

## Rename subid P03 -> SAN-071718-02
## This handles a file naming issue at the data generation step
all_data %<>% mutate(subid = ifelse(subid == "P03", "SAN-071718-02b", subid))

# Read stimulus key
df_stimulus_key <- read_csv(here::here(trial_info_path, "speed-acc-novel-analysis-order-sheet.csv")) %>% 
  select(-`finished?`)

all_data %<>% left_join(df_stimulus_key, by = "stimulus")

## Read participant demographics (TODO)
df_demographics <- readxl::read_xlsx(here::here("data/01_participant_logs/elizabeth_child_subject_log.xlsx")) %>% 
  rename(order_number = order) %>% 
  select(-child_initials)

## Join participant demographics, aoi, timing information, and eye tracking data
all_data %<>% left_join(df_demographics, by = c("subid", "order_number"))

## Read AOI information 
df_aois <- read_csv(here::here(aois_path)) 
buffer_pixels_x <- 100
buffer_pixels_y <- 150
df_aois %<>%
  mutate(aoi_y_max = case_when(
    aoi_type == "image" ~ aoi_y_max + buffer_pixels_y,
    TRUE ~ as.numeric(aoi_y_max)),
    aoi_x_max = case_when(
      aoi_type == "image" & aoi_location == "left" ~ aoi_x_max + buffer_pixels_x,
      TRUE ~ as.numeric(aoi_x_max)),
    aoi_x_min = case_when(
      aoi_type == "image" & aoi_location == "right" ~ aoi_x_min - buffer_pixels_x,
      TRUE ~ as.numeric(aoi_x_min)
    )
  )

# Score each look as left, right, or center based on AOIs
left_image_aoi <- df_aois %>% filter(aoi_location == "left")
right_image_aoi <- df_aois %>% filter(aoi_location == "right")
center_face_aoi <- df_aois %>% filter(aoi_location == "center_face")

df_final <- all_data %>% 
  mutate(aoi_looking_type = case_when(
    x <= left_image_aoi$aoi_x_max & y <= left_image_aoi$aoi_y_max ~ "left",
    x >= right_image_aoi$aoi_x_min & y <= right_image_aoi$aoi_y_max ~ "right",
    x >= center_face_aoi$aoi_x_min & x <= center_face_aoi$aoi_x_max & y >= center_face_aoi$aoi_y_min ~ "face",
    TRUE ~ "away"
    )
  )

## Clean up variable names
names(df_final) <- names(df_final) %>% 
  str_to_lower() %>% 
  str_trim() %>% 
  str_replace_all(pattern = " ", "_") %>% 
  str_replace_all(pattern = "[:punct:]", "_")

## WRITE DATA TO FEATHER FORMAT FOR SPEED
feather::write_feather(x = df_final, here::here(processed_data_path, "speed_acc_novel_timecourse.feather"))
#write_csv(df_final, path = here::here(processed_data_path, "speed_acc_novel_timecourse.csv.gz"))
