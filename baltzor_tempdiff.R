# Get average difference between black and white gravel treatments at Baltzor

# Load libraries
library(tidyverse); library(lubridate)

# Read in logger data
temp <- read_csv("data/BCtemploggers.csv")

# Clean and format logger data for plotting
temp %>% 
  filter(Site == "Balzor") %>% 
  # Cut all data to be between Jan-1 and Jun-22 (where both gravel treatments
  # have continuous data)
  filter(Date >= "2022-01-01" & Date <= "2022-06-22") -> baltzor_data 
  
# Make a plot to look at it
baltzor_data %>% 
  ggplot(aes(x = Date, y = Temp_C, color = Color)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Site) +
  scale_color_manual(values = c("black", "gray67")) +
  labs(y = "Temperature (°C) at 0-5 cm soil depth",
       x = "Date") 

# Get mean and standard errors for each treatment at the difference
baltzor_data %>% 
  select(Date, Color, Temp_C) %>% 
  spread(key = Color, value = Temp_C) %>% 
  mutate(Diff = Black - White) %>% 
  summarize(mean_black = mean(Black, na.rm = T),
            se_black = sd(Black, na.rm = T)/sqrt(n()),
            mean_white = mean(White, na.rm = T),
            se_white = sd(White, na.rm = T)/sqrt(n()),
            mean_diff = mean(Diff, na.rm = T),
            se_diff = sd(Diff, na.rm = T)/sqrt(n()))
