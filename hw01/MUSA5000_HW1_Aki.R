# Homework 1 for Stats (MUSA5000)

# Akira Di Sandro
# started: 2024-09-30

# set-up ----
{
  # set working directory (from stats project)
  # setwd("homework/hw01/")
  
  # load packages
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(kableExtra)
  library(gridExtra)
  library(grid)
  library(ggcorrplot)
  
  # functions
  `%notin%` <- Negate(`%in%`)
}

# load data ----
{
  regdata <- read.csv("RegressionData.csv")
  regdata_shp <- st_read("Lecture 1 - RegressionData.shp/RegressionData.shp")
  
}

# exploratory data analysis ----
{
  ## histograms of all variables (MEDHVAL, PCTBACHMOR, MEDHHINC, PCTVACANT, PCTSINGLES, NBELPOV100) ----
  {
    # MEDHVAL
    hist_medhval <- ggplot(regdata) +
      geom_histogram(aes(x = MEDHVAL), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Median House Value",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Median House Value",
           y = "Count") +
      scale_x_continuous(labels = scales::dollar)
    
    # PCTBACHMOR
    hist_pctbach <- regdata %>% 
      mutate(PCTBACHMOR = PCTBACHMOR / 100) %>% 
      ggplot() +
      geom_histogram(aes(x = PCTBACHMOR), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Proportion of residents with at least a Bachelor's Degree",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Proportion of residents with at least a Bachelor's Degree",
           y = "Count") +
      scale_x_continuous(labels = scales::percent)
    
    # MEDHHINC
    hist_medhhinc <- ggplot(regdata) +
      geom_histogram(aes(x = MEDHHINC), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Median Household Income",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Median Household Income",
           y = "Count") +
      scale_x_continuous(labels = scales::dollar)
    
    # PCTVACANT
    hist_pctvac <- regdata %>% 
      mutate(PCTVACANT = PCTVACANT / 100) %>% 
      ggplot() +
      geom_histogram(aes(x = PCTVACANT), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Proportion of housing units that are vacant",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Proportion of housing units that are vacant",
           y = "Count") +
      scale_x_continuous(labels = scales::percent)
    
    # PCTSINGLES
    hist_pctsing <- regdata %>% 
      mutate(PCTSINGLES = PCTSINGLES / 100) %>% 
      ggplot() +
      geom_histogram(aes(x = PCTSINGLES), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Percent of housing units that are detached single family houses",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Percent of housing units that are detached single family houses",
           y = "Count") +
      scale_x_continuous(labels = scales::percent)
    
    # NBELPOV100
    hist_nbelpov <- ggplot(regdata) +
      geom_histogram(aes(x = NBELPOV100), fill = "#451077FF") +
      theme_minimal() +
      labs(title = "Distribution of Number of households living in poverty",
           subtitle = "Philadelphia Block Groups",
           caption = "Figure XX.",
           x = "Number of households living in poverty",
           y = "Count")
    
  }
  
  # summary statistics ----
  {
    sumstats <- regdata %>% 
      summarise(
        mean_MEDHVAL = mean(MEDHVAL),
        sd_MEDHVAL = sd(MEDHVAL),
        mean_PCTBACHMOR = mean(PCTBACHMOR),
        sd_PCTBACHMOR = sd(PCTBACHMOR),
        mean_MEDHHINC = mean(MEDHHINC),
        sd_MEDHHINC = sd(MEDHHINC),
        mean_PCTVACANT = mean(PCTVACANT),
        sd_PCTVACANT = sd(PCTVACANT),
        mean_PCTSINGLES = mean(PCTSINGLES),
        sd_PCTSINGLES = sd(PCTSINGLES),
        mean_NBELPOV100 = mean(NBELPOV100),
        sd_NBELPOV100 = sd(NBELPOV100),
      )
    
    means <- sumstats %>% 
      pivot_longer(cols = all_of(starts_with("mean_")),
                   names_to = "variable",
                   values_to = "Mean",
                   names_prefix = "mean_") %>% 
      dplyr::select(variable:Mean)
    
    
    sds <- sumstats %>% 
      pivot_longer(cols = all_of(starts_with("sd_")),
                   names_to = "variable",
                   values_to = "SD",
                   names_prefix = "sd_") %>% 
      dplyr::select(variable:SD)
    
    sumstats_tab <- left_join(means, sds, by = "variable") %>% 
      rename(Variable = 1)
    
    # rounding appropriately
    sumstats_tab[c(1,3),2:3] <- data.frame(t(apply((sumstats_tab[c(1,3),2:3]), 1, round, digits = 2)))
    sumstats_tab[c(2,4,5),2:3] <- data.frame(t(apply((sumstats_tab[c(2,4,5),2:3]), 1, round, digits = 1)))
    sumstats_tab[6,2:3] <- data.frame(t(apply((sumstats_tab[6,2:3]), 1, round)))
    
    # rename Variables to be more readable
    sumstats_tab[,1] <- c("Median House Value (MEDHVAL)", "Pct Bachelor or more (PCTBACHMOR)", "Median Household Income (MEDHHINC)",
                          "Pct Vacant (PCTVACANT)", "Pct Single Unit (PCTSINGLES)", "Units Below Poverty level (NBELPOV100)")
    
    sumstats_tab %>% 
      kbl(caption = "Table 1. Summary Statistics of Dependent and Independent Variables") %>% 
      kable_styling() %>%
      kable_classic(full_width = F, html_font = "Cambria") 
    
    # STILL NEED TO EDIT THE SUMMARY STATS TABLE TO LOOK PRETTY
    
  }
  
  # log-transformed variables ----
  {
    # regdata with log-transformed variables
    # all "pct" vars and nbelpov100 were increased by 1 before log transforming because there all have zero values
    regdata_log <- regdata %>% 
      mutate(LNMEDHVAL = log(MEDHVAL),
             LNPCBACHMORE = log(1 + PCTBACHMOR),
             LNNBELPOV100 = log(1 + NBELPOV100),
             LNPCTVACANT = log(1 + PCTVACANT),
             LNPCTSINGLES = log(1 + PCTSINGLES),
             LNMEDHHINC = log(MEDHHINC))
    
    # histograms
    {
      # LNMEDHVAL
      hist_lnmedhval <- ggplot(regdata_log) +
        geom_histogram(aes(x = LNMEDHVAL), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Median House Value",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Median House Value",
             y = "Count")
      
      # log PCTBACHMOR
      hist_lnpctbach <- regdata_log %>% 
        ggplot() +
        geom_histogram(aes(x = LNPCBACHMORE), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Proportion of residents with at least a Bachelor's Degree",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Proportion of residents with at least a Bachelor's Degree",
             y = "Count")
      
      # log MEDHHINC
      hist_lnmedhhinc <- ggplot(regdata_log) +
        geom_histogram(aes(x = LNMEDHHINC), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Median Household Income",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Median Household Income",
             y = "Count")
      
      # log PCTVACANT
      hist_lnpctvac <- regdata_log %>% 
        ggplot() +
        geom_histogram(aes(x = LNPCTVACANT), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Proportion of housing units that are vacant",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Proportion of housing units that are vacant",
             y = "Count")
      
      # log PCTSINGLES
      hist_lnpctsing <- regdata_log %>% 
        ggplot() +
        geom_histogram(aes(x = LNPCTSINGLES), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Percent of housing units that are detached single family houses",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Percent of housing units that are detached single family houses",
             y = "Count")
      
      # log NBELPOV100
      hist_lnnbelpov <- ggplot(regdata_log) +
        geom_histogram(aes(x = LNNBELPOV100), fill = "#451077FF") +
        theme_minimal() +
        labs(title = "Distribution of Log-transformed Number of households living in poverty",
             subtitle = "Philadelphia Block Groups",
             caption = "Figure XX.",
             x = "Log-transformed Number of households living in poverty",
             y = "Count")
      
    }
  }
  
  # scatterplots ----
  {
    scatter_pctbach <- regdata_log %>% 
      ggplot() +
      geom_point(aes(x = PCTBACHMOR, y = LNMEDHVAL), color = "#451077FF") +
      theme_minimal() +
      labs(title = "Proportion of residents with at least a Bachelor's Degree",
           x = "Proportion of residents with at least a Bachelor's Degree",
           y = "Median House Value (Log-transformed)") 
    
    scatter_pctvac <- regdata_log %>% 
      ggplot() +
      geom_point(aes(x = PCTVACANT, y = LNMEDHVAL), color = "#451077FF") +
      theme_minimal() +
      labs(title = "Proportion of housing units that are vacant",
           x = "Proportion of housing units that are vacant",
           y = "Median House Value (Log-transformed)") 
    
    scatter_pctsing <- regdata_log %>% 
      ggplot() +
      geom_point(aes(x = PCTSINGLES, y = LNMEDHVAL), color = "#451077FF") +
      theme_minimal() +
      labs(title = "Percent of housing units that are detached single family houses",
           x = "Percent of housing units that are detached single family houses",
           y = "Median House Value (Log-transformed)") 
    
    scatter_lnnbelpov <- regdata_log %>% 
      ggplot() +
      geom_point(aes(x = LNNBELPOV100, y = LNMEDHVAL), color = "#451077FF") +
      theme_minimal() +
      labs(title = "Number of households living in poverty (log-transformed)",
           x = "Number of households living in poverty (log-transformed)",
           y = "Median House Value (Log-transformed)") 
    
    # display all four figures in one
    grid.arrange(scatter_pctbach, scatter_pctvac, scatter_pctsing, scatter_lnnbelpov,
                 ncol = 2,
                 top = textGrob("Median House Value (log-transformed) as a function of Different Predictors", gp = gpar(fontsize = 16, fontface = "bold")))
    
  }
  
  # correlation matrix ----
  {
    cormat <- regdata_log %>% 
      dplyr::select(PCTVACANT,PCTSINGLES,PCTBACHMOR,LNNBELPOV100) %>% 
      cor()
    
    cor_plot <- ggcorrplot(cormat, 
                           method = "circle",
                           type = "lower",
                           lab = TRUE,
                           lab_size = 3,
                           outline.color = "white",
                           title = "Correlation Matrix of Predictors",
                           legend.title = "Correlation") +
      theme_minimal() +  
      labs(x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 20))
  
  }
  
  # chloropleth maps ----
  {
    
  }
  
}

# regression analysis ----
{
  
}
