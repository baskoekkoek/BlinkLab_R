# convert SQL output to long format

# set values
max_n_samples <- 300
single_sample_dur <- 10 # duration of 1 sample in ms

# set wd
rm(wd)
wd <- setwd("G:/My Drive/BlinkLab smartphone/Data analysis/R scripts/QuickCheck")
print(wd)

# open csv and convert to dataframe
df <- data.frame(read.csv("df.csv"))
nrow(df)
ncol(df)
colnames(df)

# make data strings the same lenght
colnames(df)
df_split <- split(df, list(df$trial_number, df$side))
# formulate function
cutter <- function(n) {
  vector <- as.double(unlist(strsplit(toString(n$data),",")))[1:max_n_samples]
  vector <- vector * -1 # invert signal
  n$data <- as.character(gsub(" ", "", toString(vector), fixed = TRUE))
  vector <- as.double(unlist(strsplit(toString(n$time_axis),",")))[1:max_n_samples]
  n$time_axis <- as.character(gsub(" ", "", toString(vector), fixed = TRUE))
  return(n)
}
# apply function on all splits
df_list <- lapply(df_split, cutter)
# make new dataset1 with new columns added
df <- do.call(rbind, df_list)

# to long format
# convert data
df_data <- read.table(text = df$data, sep = ",", colClasses = "numeric") 
df1 <- cbind (df,  df_data)
df1_long <- gather(df1, sample, value, V1:paste0("V", max_n_samples))
df1_long$sample <- as.numeric(gsub("V", "", paste(df1_long$sample)))
df1_long$data<- as.numeric(df1_long$value)

df1_long$time_axis <- as.numeric(df1_long$sample * single_sample_dur) - single_sample_dur
df1_long$sample <- NULL
df1_long$value <- NULL

# butter filt filt
df <- df1_long
# low pass filter
nrow(df)
df_split <- split(df, list(df$trial_number, df$side))
# formulate function
filt <- function(n) {
  bf <- butter(2, 1/5, type="low") # define butterworth filter
  n$data_filt <- filtfilt(bf, n$data)
  return(n)
}
# apply function on all splits
df_list <- lapply(df_split, filt)
# make new dataset1 with new columns added
df <- do.call(rbind, df_list)
nrow(df)

# to normal rownames
rownames(df) <- 1:nrow(df)

# new time axis, 0 = first stim onset
df$time_axis_corr <- df$time_axis - 1500

# to factor
df$trial_type_f <- factor(df$trial_type, levels=c("prepulse-soft", "prepulse-medium", "prepulse-hard",
                                                  "pulse-soft", "pulse-medium", "pulse-hard")) # to factor trial names

# mean trace
df_aggr <- df %>% group_by (time_axis_corr, trial_type_f, side) %>% summarise_at (c("data_filt"), funs( mean (., na.rm=T)))


unique(df$trial_type)
ggplot(data = df, aes(x = time_axis_corr, y = data_filt)) + 
  geom_line(aes(group = trial_number, color = side)) +
  geom_line(data = df_aggr, aes(x = time_axis_corr, y = data_filt), size = 0.8) + 
  theme_bw() + 
  scale_x_continuous(limits = c(-500, 1000), name = "Time after noise onset (ms)") +
  scale_y_continuous(name = "Normalized eyelid closure") +
  facet_grid (trial_type_f ~ side) +
  geom_vline(xintercept= 0, linetype="dotted", color = "blue", size=1) + 
  geom_vline(xintercept= 120, linetype="dotted", color = "red", size=1)
  # theme(axis.title.x = element_text(size=14),
  #       axis.text.x  = element_text(size=12),
  #       axis.title.y = element_text(size=14),
  #       axis.text.y  = element_text(size=12),
  #       strip.text.y = element_text (size = 12, angle = 0))

