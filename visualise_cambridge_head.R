# Visualise 2023 Cambridge Winter Head results

# Run the script with data as the argument (wh_time.csv):
# `Rscript --vanilla visualise_cambridge_head.R wh_time.csv`
args <- commandArgs(trailingOnly = TRUE)

# Get working directory
wd <- getwd()

# Add the two strings to get the path, eg /home/ilkin/Documents/rworks/wh_time.csv
# Change to backslash if using Windows
path <- paste(wd, args[1], sep = "/")

# Unprocessed dataframe (table)
unproc_df <- read.csv(path)

# Copy into proc_df and include only select columns
proc_df <- subset(unproc_df, select= c(Position, Total, BoatType, Club, Gender))

clubs <- c("St George's Hospital", "Barts & RLH", "Kings College London", "RUMS", "(other)")
teams <- c("M1", "W1", "M2", "W2", "NM1", "W3", "NW")

# For ranking RUMS teams later
count <- 1

# Loop through all the rows of the dataframe
for (row in 1:nrow(proc_df)) {
  
  type <- proc_df[row, "BoatType"]
  
  # Take the first character of string and convert to a number
  proc_df[row, "BoatType"] <- as.numeric(substr(type, 1, 1)) # "8+" -> 8
  
  club <- proc_df[row, "Club"]
  
  # Match club to list and get the its index eg RUMS is 4th position so club_idx = 4
  club_idx <- which(clubs == club)
  
  # If club isn't in the list, then it is (other)
  if (identical(club_idx, integer(0))) {
    club_idx <- 5
  }
  
  # Label for the RUMS teams on plot, eg M1
  proc_df[row, "Label"] <- ""
  
  new_club <- clubs[club_idx]
  
  # Rename male boats' clubs as RUMS (M), and the same with women
  if (new_club == "RUMS" & proc_df[row, "Gender"] == "Male") {
    new_club <- "RUMS (M)"
    
    # If count is 1, label = teams[1] = M1, as they were first place among the RUMS crews
    proc_df[row, "Label"] <- teams[count]
    count = count + 1
    
  } else if (new_club == "RUMS") {
    new_club <- "RUMS (W)"
    
    # RUMS (W) is a new club category so it's given its own index
    club_idx <- 6
    proc_df[row, "Label"] <- teams[count]
    count <- count + 1
  }
  
  # Update club data with names of our own choice
  proc_df[row, "Club"] <- new_club
  
  # Character time is processed to decimal time
  chr_time <- proc_df[row, "Total"]
  proc_df[row, "Time"] <- as.numeric(substr(chr_time, 3, 4)) + as.numeric(substr(chr_time, 6, 9)) / 60
}

# Select the non zero entries and remove non-decimal time
df <- subset(proc_df, select= -c(Total), Time > 0.1)

library(ggplot2)

# Custom colors in particular order
colors = c("snow2", "cyan", "tomato1", "dodgerblue1",  "yellow","aquamarine4")

# Custom resolution
png(paste(wd, "/ch.png", sep=""), units="px", width=1200, height=1500, res=190)

# Specify for ggplot to use our `colors`
custom_fill = function() {
  ggplot2::scale_fill_manual(values = colors)
}

ggplot(df, aes(x=Position, y=Time, fill=Club)) +
  
  # Bar width = 1
  geom_bar(stat = "identity", width=1) +
  
  # Add labels to the RUMS columns, then shift them to the left and above
  geom_text(aes(label = Label), vjust = -0.5, hjust = 1) +
  
  # Set limits for y axis
  coord_cartesian(ylim=c(8, 15.3)) +
  theme_light() +
  
  # Move legend
  theme(legend.position = c(0.2, 0.8)) +
  custom_fill()