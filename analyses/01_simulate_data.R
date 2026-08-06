###This script simulates data from the simulated functions. Data is reordered as necessary for testing and corsym and pearsons' R are calculated for each run. This data acts as input into the plotting script
# load new function
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
    sim_data %>%
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
write.csv(plot1a_data, file = "plot1a_data.csv", row.names = FALSE,quote=FALSE)

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
write.csv(plot1a_cor_df, file = "plot1a_cor_df.csv", row.names = FALSE,quote=FALSE)


############################
#Figure 1B Data
############################
#Simulate data by rho for -1 to 1 by 0.01 for multiple sample sizes
rhos <- seq(-1, 1, by = 0.01)
plot1b_data<-data.frame(Rho=numeric(),Size=numeric(),run=numeric(),corsym=numeric(),pearson=numeric())
for (rho_num in rhos){
  #Iterate over sample sizes
  for (n in c(10,50,100,500,1000)){
    #Iterate over reps per sample size
    for (i in (1:10)){
      samples <- simulate_cordata(rho_num,n)%>%
        reorder_prop(.,"x","y",prop=1)
      plot1b_data <- plot1b_data %>%
        add_row(Rho=rho_num,Size=n,run=i,corsym=corsym(samples$x.1,samples$y.1),
                pearson=pearson(samples$x.1,samples$y.1)
        )
    }
  }
}

#Select and rename columns and export
plot1b_data<-plot1b_data[,c("Rho","Size","run","corsym","pearson")]
colnames(plot1b_data)<-c("Rho","Size","run","CorSym","Pearson")
write.csv(plot1b_data, file = "plot1b_data.csv", row.names = FALSE,quote=FALSE)

############################
#Figure 1C Data
############################
#Simulate data by rho for -1 to 1 by 0.09 for multiple sample sizes
rhos <- seq(-1, 1, by = 0.09)
plot1c_data<-data.frame(Rho=numeric(),Size=numeric(),run=numeric(),corsym_r=numeric(),corsym_o=numeric(),pearson_r=numeric(),pearson_o=numeric())
for (rho_num in rhos){
  #Iterate over sample sizes
  for (n in c(10,20,30,40,50,60,70,80,90,100)){
    #Iterate over reps per sample size
    for (i in (1:100)){
      samples <- simulate_cordata(rho_num,n)%>%
      reorder_prop(.,"x","y",prop=1)
      plot1c_data <- plot1c_data %>%
        add_row(Rho=rho_num,Size=n,run=i,corsym_r=corsym(samples$x,samples$y),
                corsym_o=corsym(samples$x.1,samples$y.1),
                pearson_r=pearson(samples$x,samples$y),
                pearson_o=pearson(samples$x.1,samples$y.1)
        )
    }
  }
}

plot1c_data <- plot1c_data %>%
mutate(Delta_r=pearson_o-corsym_o)%>%
group_by(Size,Rho)%>%
summarise(variance_r=var(Delta_r))

write.csv(plot1c_data, file = "plot1c_data.csv", row.names = FALSE,quote=FALSE)
     
############################
#Figure 1D Data
############################
#Simulate data by rho for -1 to 1, by 0.01 for n=1000 
rhos <- seq(-1, 1, by = 0.01)
n    <- 1000

#Generate data and apply ordering
bias_dataset <- do.call(rbind, lapply(rhos, simulate_cordata))

bias_dataset <- Reduce(
  \(bias_dataset, value) reorder_prop(bias_dataset, "x", "y", value),
  c(1, .75, .5, .25),
  init = bias_dataset
)
  
bias_dataset<-bias_dataset%>%
  rename(x.0=x,y.0=y)

#Pivot data for calculating correlation coefficients                        
bias_dataset<-bias_dataset %>%
  pivot_longer(
    cols = starts_with(c("x.", "y.")),
    names_to = c(".value", "index"),
    names_pattern = "([xy])\\.(.+)"
  ) %>%
  mutate(bias = as.numeric(index))

#Calculate Correlations and export dataset                        
plot_1d_data<-data.frame(Rho=numeric(),Bias=numeric(),Cor=numeric(),Pear=numeric())
for (rho_num in unique(bias_dataset$rho)){
  for (bias_prop in c(0,.25,.5,0.75,1)) {
    bias_run<-subset(bias_dataset, rho == rho_num & bias==bias_prop)
    plot_1d_data <- plot_1d_data %>%
    add_row(Rho=rho_num,
            Bias=bias_prop,
            Cor=corsym(bias_run$x,bias_run$y),
            Pear=pearson(bias_run$x,bias_run$y),
           )
  }
}
write.csv(plot_1d_data, file = "plot_1d_data.csv", row.names = FALSE,quote=FALSE)

#Clear r environment
rm(list = ls())
                        


                        
