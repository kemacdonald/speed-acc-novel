## EWMA function
## Takes in a data frame
## Returns the EWMA statistic and upper threshold for each RT in the data frame

fit_ewma <- function(df, rt_column = "RT", lambda = .01, cs = .5, sigma = .5, L = 1.5) {
  df <- arrange_(df, rt_column)
  results <- data.frame(rt = numeric(), cs = numeric(), ucl = numeric(), 
                        cond = character(), stringsAsFactors = F)
  
  for(row in 1:nrow(df)) {
    subj <- df[row, ]
    cond <- as.character(subj["condition"])
    acc <- as.integer(subj["correct"])
    rt <- as.numeric(subj["RT"])
    cs <- lambda*acc + (1-lambda)*cs # weighted average for each rt (row)
    ucl <- .5 + L*sigma*sqrt((lambda/(2 - lambda))*(1-((1-lambda)^(2*row)))) # threshold
    # add emwa params to results data frame
    subj$cs <- cs
    subj$ucl <- ucl
    results <- rbind(results, subj)
  }
  return(results)
}

## Compute guessing window for EWMA analysis
## Takes a data frame for each participant
## Returns a window of RTs where the participant was producing guesses 

compute_guessing_window <- function(sub_df) {
  # compute guessing window
  crit.window.responses <- sub_df %>%
    ungroup() %>% 
    dplyr::select(guess, RT) %>% 
    group_by(guess) %>% 
    summarise(min_rt = min(RT)) %>% 
    arrange(min_rt) 
  # if "resposne" not in the vector then ss was guessing on all shifts in that condition 
  if (!("response" %in% crit.window.responses$guess)) {
    # just use the diff between earliest and latest shifts
    sub_df$time_guessing <- max(sub_df$RT) - min(sub_df$RT) 
  } else {
    # here we use the diff between the earliest shift and the earliest "valid" (non-guess) shift
    sub_df$time_guessing <- max(crit.window.responses$min_rt) - min(crit.window.responses$min_rt)  
  }
  return(sub_df)
}

#### Read EWMA results file

read_ewma <- function(path, file_name) {
  d <- read_csv(paste0(path, file_name)) %>% 
    select(-RT) # this is a hack to remove the duplicate RT column created by the fit ewma function
  
  names(d) <- str_to_lower(names(d))
  
  # clean up subid variable
  if(sum(str_detect("sub.num", names(d))) > 0) {
    d <- d %>% 
      rename(subid = sub.num) %>% 
      mutate(subid = as.character(subid))
  }
  
  d.clean <- d %>% 
    dplyr::mutate(
      experiment = case_when(
        str_detect(file_name, "trio") ~ "trio",
        str_detect(file_name, "gaze") ~ "gaze",
        str_detect(file_name, "text") ~ "text",
        str_detect(file_name, "noise") ~ "noise",
        str_detect(file_name, "ng") ~ "noise_gaze",
        TRUE ~ "NA"
      ),
      age_code = case_when(
        str_detect(file_name, "kids") ~ "child",
        str_detect(file_name, "adult") ~ "adult",
        TRUE ~ "NA"
      ),
      subid = as.character(subid)
    )

  d.clean
}

#### AGGREGATE EWMA MODEL OUPUT FOR PLOTTING CONTROL CHART

aggregate_ewma <- function(d) {
  ms <- d %>% 
    group_by(ewma_param, subid, condition, rt) %>% 
    summarise(mean_param_ss = mean(param_value)) %>% 
    group_by(ewma_param, condition, rt) %>% 
    summarise(mean_param = mean(mean_param_ss)) %>% 
    ungroup %>% 
    mutate(condition = as.factor(condition))
  
  cutoffs <- d %>% 
    group_by(condition, subid, guess) %>% 
    summarise_(cutoff = interp(~ min(x), x = as.name("rt"))) %>% 
    group_by(condition, guess) %>% 
    summarise(median_param = median(cutoff),
              min_cut = min(cutoff),
              max_cut = max(cutoff),
              ci_lower = median_param - qnorm(0.975) * (sd(cutoff) / sqrt(n())),
              ci_upper = median_param + qnorm(0.975) * (sd(cutoff) / sqrt(n()))
    ) %>% 
    ungroup() %>% 
    tidyr::complete(condition, nesting(guess))
  
  list(ewma_summary = ms, cutoff_summary = cutoffs)
}

#### Construct red/green ribbons for EWMA control charts

make_ribbons <- function(ewma_df, cutoffs_df) {
  # get max rt to handle case where there is no non-guessing signal
  max_rt <- ewma_df %>% 
    group_by(condition) %>% 
    summarise(max_rt = max(rt))
  
  ribbon_df <- ewma_df %>% 
    left_join(., cutoffs_df, by = "condition") %>% 
    left_join(., max_rt, by = "condition") %>% 
    mutate(rt_cut_point = ifelse(is.na(min_cut), max_rt,
                                 min_cut))
  
  red_ribbon <- ribbon_df %>% 
    filter(rt <= rt_cut_point) %>% 
    spread(key = ewma_param, value = mean_param)
  
  green_ribbon <- ribbon_df %>% 
    filter(rt >= rt_cut_point) %>% 
    spread(key = ewma_param, value = mean_param)
  
  ribbons <- list(red = red_ribbon, green = green_ribbon)
  ribbons
}


plot_ewma_chart <- function(model_vals) {
  ewma_df <- model_vals$ewma_summary
  cutoffs_df <- model_vals$cutoff_summary %>% filter(guess == "response")

  # get x-axis limit
  x_max <- max(ewma_df$rt) + 0.1
  
  # make ribbons
  ribbons <- make_ribbons(ewma_df, cutoffs_df)
  
  # create plot
  ewma_df %>% 
    ggplot(aes(x = rt, y = mean_param, color = ewma_param)) +
    geom_segment(aes(x = ci_lower, y = 0.8, xend = ci_upper, yend = 0.8), 
                 color = "black", size = 100, alpha = 0.2,
                 data = cutoffs_df) +
    geom_ribbon(aes(ymin = cs, ymax = ucl, x = rt), fill = "darkred", alpha = 0.5, 
                data = ribbons$red, 
                inherit.aes = F)  +
    geom_ribbon(aes(ymin = cs, ymax = ucl, x = rt), fill = "darkgreen", alpha = 0.5, 
                data = ribbons$green, 
                inherit.aes = F) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = median_param), linetype = 2, 
               data = cutoffs_df) +
    geom_hline(yintercept = 0.5, linetype = "solid") +
    labs(x = "RT (sec)", y = "EWMA statistic") +
    guides(color=F) + 
    xlim(0, x_max) +
    facet_wrap(~condition, ncol = 1) +
    scale_color_manual(values = c("black", "darkgrey")) +
    geom_dl(aes(label = ewma_param), method = "last.bumpup") +
    ggthemes::theme_few() +
    theme(text = element_text(size = 10))
}


make_ewma_boxplot <- function(d) {
  # aggregate prop guessing for each participant
  ss <- d %>% 
    group_by(subid, condition, guess) %>% 
    summarise(count = n()) %>% 
    mutate(prop.responding = round(count / sum(count), 2))
  
  # make plot
  ss %>% 
    filter(guess == "response") %>% 
    ggplot(aes(x = fct_reorder(condition, prop.responding), y = prop.responding)) +
    geom_boxplot(fill = "dodgerblue", width = 0.2, alpha = 0.9, outlier.color = "white",
                 notch = F) +
    lims(y = c(0,1)) +
    labs(x = "Condition", y = "Prop. Language Driven")  +
    scale_x_discrete(expand = c(0,1)) 
  
}