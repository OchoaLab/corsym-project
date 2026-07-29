###This script simulates data from the simulated functions. Data is reordered as necessary for testing and corsym and pearsons' R are calculated for each run. This data acts as input into the plotting script

# load new function
source( 'corsym.R' )
source( 'simulation_analysis_functions.R' )

# load libraries
library(MASS)
library(tidyverse)
library(copula)

############################
#Figure 1A Data
############################
#Simulate data by rho for 1, -.5, 0, -0.5, and 1 for n=1000
rhos<-c(-1,-.5,0,.5,1)
n=1000

#Generate data
sim_data <- do.call(rbind, lapply(rhos, simulate_cordata))

#Create and export the necessary data for plot 1a, a dataframe with the information of the fully ordered and non-ordered samples, and a correlation dataframe saving the specific correlation values for text addition
plot1a_data <-sim_data %>%
  mutate(row_index = row_number())%>%
  #Ensure the original data is assigned as random
  transmute(rho, row_index,
            x = x, y = y, type = "Random") %>%
  bind_rows(
    dat %>%
      #Reordered and name the reordered data as such
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
write.csv(plot1_data, file = plot1a_data.csv, row.names = FALSE,quote=false)

plot1a_cor_df <- plot1a_data %>%
  #Rename for better plotting
  mutate(Rho=rho)%>%
  #Get summary correlation coefficients
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
write.csv(plot1a_cor_df, file = plot1a_cor_df.csv, row.names = FALSE,quote=false)

############################
#Figure 1B Data
############################

############################
#Figure 1C Data
############################

############################
#Figure 1D Data
############################
