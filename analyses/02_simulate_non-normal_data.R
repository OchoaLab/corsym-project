###This script simulates data from the simulated functions. Data is reordered as necessary for testing and corsym and pearsons' R are calculated for each run. This data acts as input into the plotting script
# load new function
source( 'corsym.R' )
source( 'simulation_analysis_functions.R' )

# load libraries
library(MASS)
library(tidyverse)
library(copula)

############################
#Supplemental Non-normal Data
############################

rhos<-c(-1,-.5,0,.5,1)

# combine into one long data frame
sim_data <- do.call(rbind, lapply(rhos, simulate_non_normal_cordata,distribution="uniform"))

#Create and export the necessary data for plot 1a, a dataframe with the information of the fully ordered and non-ordered samples, and a correlation dataframe saving the specific correlation values for text addition
nonnormal_plot_data<-sim_data %>%
  mutate(row_index = row_number())%>%
  #Ensure the original data is assigned as random
  transmute(rho, row_index,
            x = x, y = y, type = "Random") %>%
  bind_rows(
    sim_data %>%
      mutate(row_index = row_number(),
             var3 = ifelse(x > y, y, x),
             var4 = ifelse(x > y, x, y)) %>%
      transmute(rho, row_index,
                x = var3, y = var4, type = "Ordered")
  ) %>%
  filter(!is.na(x), !is.na(y))%>%
  #Round for easy visuals and make Random vs Ordered a type for facetting
  mutate(rho=round(rho,digits = 3),
         type= factor(type,
                      levels = c("Random", "Ordered")),
         Plot_rename="True Rho"
  )

write.csv(nonnormal_plot_data, file = "nonnormal_plot_data.csv", row.names = FALSE,quote=FALSE)

#Create Correlation Dataframe for labels
nonnormal_cor_df <- nonnormal_plot_data %>%
  #Rename for better plotting
  mutate(Rho=rho)%>%
  group_by(type, Rho) %>%
  summarise(
    R = pearson(x, y),
    corR = corsym(x,y),
    .groups = "drop"
  )%>%
  #Make nice labelled correlations for plotting
  mutate(
    label = sprintf(
      "<i>r<sub>p</sub></i> = %.2f<br><i>r<sub>c</sub></i> = %.2f",
      R, corR)
  )
write.csv(nonnormal_cor_df, file = "nonnormal_cor_df.csv", row.names = FALSE,quote=FALSE)


