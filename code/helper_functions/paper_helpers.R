# load libraries
library(papaja); library(here); library(lme4); library(directlabels); library(lazyeval)
library(knitr); library(pander); library(magrittr); library(forcats); 
library(cowplot); library(rstanarm); library(xtable);
library(png); library(grid); library(ggthemes)
library(kableExtra); library(rogme); library(ggridges); library(ggrepel)

# load tidyverse last, so no functions get masked
library(tidyverse); 

# set ggplot theme
theme_set(theme_minimal() + 
            theme(panel.border = element_rect(fill = NA, color = "grey", size = 1),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.position = "top",
                  legend.text = element_text(size = 9),
                  legend.title = element_text(size = 10),
                  strip.text = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12)
                  )
          )


# make function to convert logit back to probability 
logit_to_prob <- function(logit) {
  odds <- exp(logit)
  odds / (1 + odds)
}

## Read HDDM file

read_hddm_file <- function(file_name, path, condition_list, param_list) {
  raw_file <- read_csv(paste0(path, file_name), col_names = F)
  
  # get condition and param name from the file name
  condition_name <- str_extract(string = file_name, pattern = paste(condition_list, collapse = '|'))
  param_name <- str_extract(string = file_name, pattern = paste(param_list, collapse = '|'))
  
  final_df <- t(raw_file) %>% 
    as.data.frame() %>% 
    rename(param_value = V1) %>% 
    mutate(condition = condition_name,
           param_name = param_name)
  
  # return the data frame
  final_df
}


## Plot HDDM parameter values

plot_hddm <- function(d, x_lims = c(0, 3)) {
  ggplot(aes(x = param_value, color = condition), data = d) +
    geom_line(stat="density", size = 1.5) + 
    langcog::scale_color_solarized() +
    facet_grid(.~param_name, scales = "free") +
    labs(x = "Parameter value", y = "Density", color = "") +
    guides(color=guide_legend(nrow=2,byrow=TRUE)) +
    ggthemes::theme_few() +
    lims(x = x_lims) +
    theme(text = element_text(size=10),
          legend.position=c(0.75,0.75),
          legend.direction="horizontal",
          legend.background = element_rect(fill=alpha('white', 0.4)))
}

## Score trial function
## Categorizes trials as correct or incorrect in 2-AFC gaze task
## Takes in a row (trial), a start window, and an end window
## Returns whether that trial was a correct or incorrect shift

score_trial_type_lwl <- function(trial, start, end) { 
  start_col <- which(names(trial)==start)
  end_col <- which(names(trial)==end)
  trial_type <- "no_shift"
  rt <- NA
  response <- trial[which(names(trial)=="Response")]
  first_look_signer <- FALSE
  
  if (response == "D") {
    for (col in start_col:end_col) {
      prev_val <- trial[col-1]
      curr_val <- trial[col]
      next_val <- trial[col+1]
      
      if(is.na(curr_val)) { # break if we hit an NA
        break
      } 
      
      if(first_look_signer) {
        
        if(curr_val == "." & prev_val == "0.5") {rt <- names(trial[col])} # store RT
        
        if(curr_val == "." & next_val %in% c("0", "1", "0.5", ".5")) {
          if(next_val == "1") {
            trial_type <- "C_T"
          } else if(next_val == "0.5" | next_val == ".5") {
            trial_type <- "C_C"
          } else {
            trial_type <- "C_D"    
          }
          break
        }
      } else {
        ## check if current value is look to signer
        ## if it is then we should start checking for trial_type
        if(curr_val %in% c("0.5", ".5")) {
          first_look_signer <- TRUE
        }
      }
    }
  } else {
    trial_type <- "off_center"
  }
  return(c(trial_type, rt))
}

## Add trial numbers 
## Takes a participant's data frame and number of trials for each participant
## Returns the df with trial numbers added
add.tr.nums.fun <- function (df) {
  
  n_trials <- df %>% 
    select(subid, stimulus) %>% 
    unique() %>% 
    nrow()
  
  df %<>% select(subid, stimulus, t) %>% 
    arrange(t) %>% 
    select(subid, stimulus) %>% 
    unique() %>% 
    mutate(tr.num = 1:n_trials)
  
  return(df)
}

## Score trial for SMI eye tracking data
## Takes in a data frame for each trial and 
## Returns (1) an RT and (2) whether the shift was correct vs. incorrect

score_trial_et <- function(trial_df, crit_onset_type = "noun") {
  # filter trial to keep just that onset type
  trial_df %<>% filter(response_onset_type == crit_onset_type)
  # print the subid and trial number to make debugging easier
  print(paste(trial_df$tr.num[1], trial_df$subid[1]))
  
  # build variables to index each trial
  response.type.index <- paste0(crit_onset_type, ".onset") 
  t.filter.type <- paste0("t.rel.", crit_onset_type, " > 0")
  t.select.type <- paste0("t.rel.", crit_onset_type)
  
  # check if there is a shift in the trial after the critical onset
  # and record where the shift started and ended
  crit.window.responses <- trial_df %>% 
    filter_(t.filter.type) %>% 
    select_("target_looking", t.select.type) %>% 
    group_by(target_looking) %>% 
    summarise_(min_t = interp(~ min(x), x = as.name(t.select.type))) %>% 
    arrange(min_t)
  
  # store info about the shift
  shift.start <- crit.window.responses$target_looking[1]
  shift.info <-paste(crit.window.responses$target_looking[1], crit.window.responses$target_looking[2],
                     sep = "-")
  
  # check if there is only one "response" in the target_looking vector
  # if 1, then there was no shift (i.e., no change from response at crit.onset)
  if (nrow(crit.window.responses) == 1) {
    trial_score <- trial_df %>% 
      mutate(rt = NA, shift_type = "no_shift") %>% 
      select(rt, shift_type) 
  } else {
    # get the earliest time point when target looking switches from the critical onset value 
    trial_score <- trial_df %>% 
      filter_(t.filter.type) %>% 
      filter(target_looking != shift.start) %>% 
      select_(t.select.type, "target_looking") %>% 
      group_by(target_looking) %>% 
      summarise_(rt = interp(~ min(x), x = as.name(t.select.type))) %>% 
      filter(rt == min(rt)) %>% 
      mutate(shift_type = ifelse(shift.info == "center-target", "C_T", 
                                 ifelse(shift.info == "center-distracter", "C_D",
                                        ifelse(shift.info == "target-distracter", "T_D",
                                               ifelse(shift.info== "target-center", "T_C",
                                                      ifelse(shift.info == "distracter-target", "D_T",
                                                             ifelse(shift.info == "distracter-center", "D_C")))))),
             shift_accuracy = ifelse(shift_type == "C_T", "correct", "incorrect")) %>%
      select(rt, shift_type, shift_accuracy) 
  }
  
  # add the rt and score to the trial data frame
  trial_df <- cbind(trial_df, trial_score)
  
  return(trial_df)
}


## Fit DDM function 
## Takes in a data frame that has been split by participant and condition
## Returns a data frame of DDM parameter values for that participant/condition
## Note that if the participant does not have any valid RTs, then it returns NAs for DDM vals

fit_ddm <- function(df, condition_col, subid_col, bysub = T, niter = 500) {
  # get the condition and subid values
  cond <- unique(df[[condition_col]])
  sub <- unique(df[[subid_col]])
  param_names <- c("separation", "non.decision", "bias", "drift")
  df %<>% 
    select(q, resp) %>% 
    as.data.frame()
  # fit ddm if there are valid responses to fit
  if (nrow(df) > 0) {
    # fit ddm for each participant 
    fit.vals <- optim(c(1, .1, .1, 1), wiener_deviance, control = list(maxit = niter),
                      dat=df, method="Nelder-Mead")
    pars <- cbind(data.frame(fit.vals = fit.vals$par), param = param_names, convergence = fit.vals$convergence) 
  } else {
    pars <- cbind(data.frame(fit.vals = c(NA, NA, NA, NA)), param = param_names, convergence = NA)
  }
  
  # add info to the sub_df and return
  if (bysub == T) {
    pars %<>% 
      mutate(subid = sub, condition = cond,
             fit.vals = round(fit.vals, 3))
  } else {
    pars %<>% 
      mutate(condition = cond, 
             fit.vals = round(fit.vals, 3))
  }
  
  return(pars)
}

## Remove outlier values
## takes in a data frame for an experimental condition, a stdev cutpoint, and a column name to compute over
## returns that data frame with extreme values +/- stdev cutpoint from the mean for that condition

remove_extreme_vals <- function(data_frame, sd_cut_val = 2, value_column) {
  m.val <- mean(data_frame[[value_column]])
  sd.val <- sd(data_frame[[value_column]])
  # compute cut points
  cut_point_upper <- round(m.val + (sd_cut_val * sd.val), 3)
  cut_point_lower <- round(m.val - (sd_cut_val * sd.val), 3)
  
  # standard eval stuff
  filter_criteria_upper <- interp(~y <= x, .values=list(y = as.name(value_column), x = cut_point_upper))
  filter_criteria_lower <- interp(~y >= x, .values=list(y = as.name(value_column), x = cut_point_lower))
  
  # filter and return df
  data_frame %<>% 
    mutate(cut_point_upper = cut_point_upper,
           cut_point_lower = cut_point_lower) %>% 
    filter_(filter_criteria_upper, 
            filter_criteria_lower)
  
  return(data_frame)
}


################################################################################
## IDF CONVERT
## read in data file from SMI tracker's IDF converter utility
## major issue is that new stimuli are marked as new "events" 
## need to be converted to their own column
##
## adapted from aen and dy
################################################################################

to.n <- function (x) {
  as.numeric(as.character(x))
}


read.smi.idf <- function (file.name, suffix.len = 4) {
  
  file.name <- paste(raw.data.path,files[10],sep="")
  
  ## read the header from the file to paste back into the new file
  tmp.header <- scan(file.name, what = character(), sep="\n", 
                     nlines = 40, quiet=TRUE)
  
  ## trim to just contain header information 
  header <- vector()
  for(index in 1:length(tmp.header)) {
    line <- tmp.header[index]
    header <- c(header, line)
    if (line == "## ") {break}
  }
  
  ## get subject id from the header
  subid <- header[str_detect(header, "Subject")]
  subid <- str_split(subid, pattern = "\t")[[1]][2]
  
  ## get the length of the header so we know how many rows to skip before reading the data
  header.rows <- length(header)
  
  ## DATA CLEANING 
  # read in data and get rid of header rows
  all.d <- read_tsv(file.name, skip = header.rows, col_types = cols(Time = "c"))
  
  ## split data into messages and data
  ## First get data:
  d <- all.d %>% filter(all.d$Type=="SMP")
  
  # convert to numeric here
  d$rx <- to.n(d$"R POR X [px]")
  d$ly <- to.n(d$"L POR Y [px]")
  d$ry <- to.n(d$"R POR Y [px]")
  d$lx <- to.n(d$"L POR X [px]") 
  
  #clean up data frame
  d %<>% 
    select(Time, lx, ly, rx, ry) %>%
    rename(t = Time) %>% 
    mutate(t = to.n(t))
  
  ## Now get "messages" - about the stimulus that's being presented
  all.d$rawx <- all.d$"L Raw X [px]"
  
  msgs <- all.d %>% 
    filter(Type=="MSG") %>%
    select(Time, rawx) %>%
    rename(t = Time,
           msg = rawx) %>%
    mutate(stimulus = gsub("# Message: ", "",msg),
           t = to.n(t))
  
  ## merge stimulus information back into d frame as a column
  d$stimulus <- sapply(d$t,
                       function(x) {
                         set <- msgs$stimulus[msgs$t < x]
                         set[length(set)]
                       })
  
  d$stimulus <- as.character(d$stimulus)
  
  ## drop the times before the first video
  d %<>% filter(stimulus != "character(0)") 
  
  ## add subid
  d$subid <- subid
  
  return(d)
}

################################################################################
## PREPROCESS DATA 
## take data file with l and r, x and y, as well as stimulus, average
## eyes, do whatever preprocessing needs to be done. 
################################################################################

preprocess.data <- function(d, 
                            x.max = 1680, y.max=1050,
                            samp.rate = 120,
                            avg.eyes=TRUE) {
  
  ## drop the .jpg from the stimulus
  ##d$stimulus <- str_replace(d$stimulus,pattern=".jpg",replacement="")
  
  # remove bad looks before averaging the eyes
  d %<>%
    mutate(rx = ifelse(rx < 1 | rx > 1919, NA, rx),
           lx = ifelse(lx < 1 | lx > 1919, NA, lx),
           ry = ifelse(ry < 1 | ry > 1079, NA, ry),
           ly = ifelse(ly < 1 | ly > 1079, NA, ly))
  
  ## average the eyes
  if (avg.eyes) {
    # if we have both eyes, then average to the nearest pixel
    # if one of the eyes is missing, then just use the other eye's coordinate
    d %<>%
      mutate(x = ifelse(!is.na(rx) & !is.na(lx), (lx+rx) / 2,
                        ifelse(is.na(rx) & !is.na(lx), lx, 
                               ifelse(is.na(lx) & !is.na(rx), rx, 
                                      NA))),
             y = ifelse(!is.na(ry) & !is.na(ly), (ly+ry) / 2,
                        ifelse(is.na(ry), ly, 
                               ifelse(is.na(ly), ry, 
                                      NA)))
      )
    
    # remove the l/r eye variable names
    d <- d[, !(names(d) %in% c("lx","rx","ly","ry"))]
  }
  
  ## clip off out of range numbers
  d$x[d$x < 0 | d$x > x.max] <- NA
  d$y[d$y < 0 | d$y > y.max] <- NA
  
  ## convert the time into seconds
  d$t <- round((d$t - d$t[1])/(1000000), 3)
  ms.increment <- c(0, diff(d$t))
  
  ## add a column of times for each video segment
  ## note this code makes me somewhat ashamed; it's slow and it abuses the R namespace
  ## because it's basically a for loop. but I don't know how to fix it. -mcf
  stim.change <- c(diff(as.numeric(factor(d$stimulus))) != 0,0)
  dt <- c(diff(d$t),0)
  t <- 0
  d$t.stim <- mapply(function (x,y) { 
    if(x==TRUE) { # if stimulus changes
      t <<- 0 # reset counter
      return(t)
    } else { # if stimulus is the same
      t <<- t + y # increment counter
      return(t)
    }},stim.change,dt)
  
  ## round to the nearest sample
  d$t.stim <- round(d$t.stim*samp.rate)/samp.rate
  
  ## y flip (so origin is cartesian, not matrix (bottom left, instead of top left)
  d$y <- y.max - d$y
  
  ## finished
  return (d)
}

################################################################################
## ROI CHECK
## takes a list of ROIs as x, y, w, h boxes
## returns ROI number or NAN
################################################################################

roi.check <- function (d, rois) {
  roi <- factor(NA,levels=names(rois))
  
  for (i in 1:length(rois)) {
    r <- rois[[i]]
    roi[d$x > r[1] & d$x < r[1] + r[3] &
          d$y > r[2] & d$y < r[2] + r[4]] <- names(rois)[i]
  }
  
  return(roi)
}

################################################################################
## REZERO TRIALS
## create timestamps starting from the point of disambiguation
################################################################################

rezero.trials <- function (d,onset.name="target.onset") {
  ddply(d,.(stimulus,subid), function(x) {
    x$t.crit <- x$t.stim - x[,onset.name]
    return(x)
  })
}

################################################################################
## ROI IMAGE1
## takes a list of ROIs as x, y, w, h boxes
## makes a picture so you can check them
################################################################################

roi.image <- function (rois,y.max=1050,x.max=1680) {
  plot(NA,xlim=c(0,x.max),ylim=c(0,y.max),bty="n",xlab="x",ylab="y")
  
  for (i in 1:length(rois)) {
    r <- rois[[i]]
    rect(r[1], r[2], r[1] + r[3], r[2] + r[4])
    text(r[1] + r[3]/2,
         r[2] + r[4]/2,
         names(rois)[i])
  }
}


