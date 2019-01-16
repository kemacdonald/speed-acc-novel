## power analysis for follow-up experiment
library(lme4)
library(simr)


m1 <- ss_novel_face %>% 
  filter(target_looking == "center", trial_type == "exposure") %>%
  lmer(prop_looking ~ (gaze_condition + trial_num_learn_block + age_category)^2 + 
             (gaze_condition + trial_num_learn_block | subid),
           data = .)

summary(m1)

m2 <- ss_novel_obj %>% 
  filter(target_looking == "target") %>% 
  lmer(prop_looking ~ (gaze_condition + trial_type + age_category + trial_num_learn_block)^2 + 
         (gaze_condition | subid),
       data = .)

summary(m2)       
       