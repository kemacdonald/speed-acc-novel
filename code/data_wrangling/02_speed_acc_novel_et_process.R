################################################################################
## PREPROCESSING SCRIPT FOR SPEED-ACC-NOVEL EXPERIMENT
## read in eye tracking data text files and consolidate them into a single .csv
################################################################################

## PRELIMINARIES
source(here::here("code/helper_functions/libraries_and_functions.R"))
raw_data_path <- "data/02_raw_data/"
#raw_data_path <- "data/test_data/version_2/"
trial_info_path <- "data/00_stimuli_information/analysis_order_sheets/"
aois_path <- "data/00_stimuli_information/analysis_order_sheets/speed-acc-novel-aois.csv"

## blacklisted roi files 
processed_data_path <- "data/03_processed_data/novel_words"
calibration_stimulus_name <- c("cc3bd7c5c8d457694031c7630357751f", "c7280d7cf8d2071e0a099f77fef07087")
familiar_stimulus_name <- c("ef177c1276275662048244274d3df8cf_1920x1080", 
                            "67a32392adbd5dcf905315e855d3c988_1920x1080",
                            "5329d3b598a3f7d8d49c1d85d1734522_1920x1080",
                            "e74e6ab1cd3f4b97e184d621ea6296b9_1920x1080",
                            "ef177c1276275662048244274d3df8cf_1920x1080",
                            "6ef3becaf6f0eaccbb2580ee4070f4b0_1920x1080",
                            "55f8a87d3ed59128c19691c971d72f32_1920x1080",
                            "f242c27e12c6122eeb7c89a3fdc494c5_1920x1080",
                            "2501a6ecbb85880234d5fad1010d8b19_1920x1080")

keypress_events <- c("UE-keypress VolumeUp", "UE-keypress VolumeDown", "UE-keypress LWin")

great_job_mov <- c("60c3b54031bc9996064b4e3407d26313_1920x1080", 
                   "02cd2d10c439c205f28cd62e8906bb37_1920x1080", 
                   "1e7766f8109649525f71af1fa2342ce4_1920x1080",
                   "999738d7aec28555b0421930c57f5085_1920x1080",
                   "b2189546ceb4bdb2e8a418a48c75a1e8_1920x1080",
                   "f8f24a6863eec5ddd2f50db053a34a6f_1920x1080",
                   "eb611debac9ddee16b4e580065f7b1b2_1920x1080")

black_image <- "82c3827a23dc8011c3fbcc4d31772ea2_1920x1080.jpg"

elmo <- "05a2ee03a15d82f4cee5b80627290652_1920x1080"

trial_blacklist <- c(black_image, 
                     calibration_stimulus_name,
                     familiar_stimulus_name,
                     great_job_mov,
                     keypress_events,
                     elmo)

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

## Rename subids
## This handles a couple of file naming issue at the data generation step
all_data %<>% mutate(subid = case_when(
  subid == "P03" ~ "SAN-071718-02", 
  subid == "P04" ~ "SAN-072518-03",
  subid == "080618-01" ~ "SAN-080618-01",
  subid == "080318-01" ~ "SAN-080318-01",
  subid == "SAN-pilotB-072018-02" ~ "SAN-072018-02",
  subid == "SAN-pilotB2-072018-03" ~ "SAN-072018-03",
  subid == "SAN-080118-0" ~ "SAN-080118-03",
  TRUE ~ subid)) %>% 
  filter(subid != "bing_pilot_1")

## check how many subids we have in the eye tracking data
all_data %>% select(subid, age_category) %>% unique() %>% count(age_category)

# Read stimulus key
df_stimulus_key <- read_csv(here::here(trial_info_path, "speed-acc-novel-analysis-order-sheet.csv")) %>% 
  select(-`finished?`, -order_age) 

all_data %<>% left_join(., df_stimulus_key, by = "stimulus") %>% 
  mutate(order_number = as.character(order_number)) %>% 
  distinct()

# check how many trials for each participant in each block
all_data %>% 
  distinct(subid, trial_num_exp, trial_num_learn_block) %>% 
  count(subid, trial_num_learn_block) %>% 
  kable()

## Add demographic information
demo_path <- "data/01_participant_logs/es_combined_subject_log.xlsx"

df_demographics <- readxl::read_xlsx(here::here(demo_path), guess_max = 1) %>% 
  filter(!is.na(subid)) %>%
  rename(order_number = order,
         age_category = child_adult) %>%
  mutate(order_number = as.character(order_number),
         age_years = age,
         age_days = as.double(testing_date - birth_date),
         age_group = as.integer(age_days / 365)) %>% 
  select(-contains("initials"))

all_data %<>% left_join(., df_demographics, by = c("subid", "order_number"))

## Read AOI information 
df_aois <- read_csv(here::here(aois_path)) 
buffer_pixels_x <- 200
buffer_pixels_y <- 100

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

## convert the trial measurement information from frames to milliseconds 
## and seconds and remove the original timing variables, so we don't get confused in the future.
df_final %<>% 
  mutate(noun_onset_ms = (noun_onset_sec * 1000) + (noun_onset_frames * 33),
         noun_onset_seconds = noun_onset_ms / 1000,
         sentence_onset_ms = (sentence_onset_sec * 1000) + (sentence_onset_frames * 33),
         sentence_onset_seconds = sentence_onset_ms / 1000,
         gaze_onset_ms = (as.numeric(gaze_onset_sec) * 1000) + (as.numeric(gaze_onset_frames) * 33),
         gaze_onset_seconds = gaze_onset_ms / 1000) %>% 
  select(-noun_onset_sec, -noun_onset_frames, -sentence_onset_sec, -sentence_onset_frames,
         -gaze_onset_sec, -gaze_onset_frames)

## Remove NAs in AOI looking type 
## Convert AOI coding to Target coding
df_final %<>% 
  filter(is.na(aoi_looking_type) == F) %>% 
  mutate(t_stim = ifelse(t_stim == 0, 0, round(t_stim, digits = 3)),
         target_side = ifelse(target_image == left_image, "left", "right"),
         target_looking = case_when(
           aoi_looking_type == "face" ~ "center",
           aoi_looking_type == "away" ~ "away",
           aoi_looking_type == target_side ~ "target", 
           aoi_looking_type != target_side ~ "distracter",
           TRUE ~ "NA"),
         t_rel_noun = t_stim - noun_onset_seconds,
         t_rel_gaze = t_stim - gaze_onset_seconds,
         t_rel_sentence = t_stim - sentence_onset_seconds) 

## round new time columns to nearest sample
sample_rate <- 30
df_final %<>% 
  mutate(t_rel_noun = round(t_rel_noun*sample_rate) / sample_rate,
         t_rel_gaze = round(t_rel_gaze*sample_rate) / sample_rate,
         t_rel_sentence = round(t_rel_sentence*sample_rate) / sample_rate)

## Filter bad blocks of trials based on demographics sheet
df_final %<>%
  mutate(block_excluded = ifelse(block_excluded == "NA", "none", block_excluded),
         keep_block = case_when(
           block_excluded == learning_block ~ "drop", 
           TRUE ~ "keep"
         )) %>% 
  filter(keep_block == "keep")

## check if any samples are missing metadata
check_if_meta_missing <- df_final %>% filter(is.na(target_image))
nrow(check_if_meta_missing) # should be 0

## Remove any trials for which we don't have metadata 
df_final %<>% filter(!(is.na(target_image)))

## WRITE DATA TO FEATHER FORMAT
feather::write_feather(x = df_final, here::here(processed_data_path, "speed_acc_novel_timecourse.feather"))
