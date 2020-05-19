#################################################################
# Title: Thermal Safety Margin Paper Data Analysis & Vis
# Created by : L. Pandori
# Created on : 4/29/2019
# Last Edited : 05/14/2020
#################################################################
##### Prepare: load packages #####
# Clear workspace
rm(lists=ls())

# load libraries
library(tidyr)
library(forcats)
library(lubridate)
library(RColorBrewer)
library(readr)
library(dunn.test)
library(dplyr)
library(ggplot2)
library(PNWColors)

##### Load & reshape size @ life stage data ######
# Load data
size <- read_csv("GonadStage.csv")

# Designate factors
size$Tideht <- ordered(size$Tideht, 
                       levels = c('High', 'Mid', 'Low')) 
size$Lifestage <- ordered(size$Lifestage, 
                          levels = unique(size$Lifestage))
            
##### Test if length @ life stages differs across tideht #####
  
# Does adult size differ across tide height?
  adult<-subset(size, size$Lifestage == 'Adult')
  kruskal.test(Leng ~ Tideht, data = adult)
  
  # Pairwise tests
  adult.dunn <- dunn.test(
    x = adult$Leng, g = adult$Tideht, 
    method = 'bonferroni', kw = TRUE, label = TRUE, list = TRUE)
  
# Does juvenile size differ across tide height?
  juvenile<-subset(size, size$Lifestage == 'Juvenile')
  kruskal.test(Leng ~ Tideht, data = juvenile)
  
  # Pairwise tests
  juv.dunn <- dunn.test(
    x = juvenile$Leng, g = juvenile$Tideht, 
    method = 'bonferroni', kw = TRUE, label = TRUE, list = TRUE)
  
# Does min adult size differ across tide height?
  # Subset 5 smallest for each tide height
  adult_sm <- adult %>%
    group_by(Tideht) %>%
    arrange(Leng) %>%
    top_n(-3, Leng)
  
  # note: there are only 2 adults @ high tide height
  kruskal.test(Leng ~ Tideht, data = adult_sm)
  
##### Plot set-up : theme & color palette #####
  
  # specify color palette
  discr_starfish <- pnw_palette('Bay', 5, type = 'discrete')
  
  # specify overarching plot theme
  theme_set(
    theme_bw() + 
      theme(text = element_text(size = 12),
            # no background to wrap panels
          strip.background = element_blank(),
            # panel labels outside x axis labels
          strip.placement = 'outside',
            # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(angle = 45, 
                        hjust = 1, size = 12, color = 'black'),
          strip.text.x = element_text(size = 12, color = 'black'), 
            # no legends
          plot.caption = element_text(family = 'serif', size = 12, color = 'black', hjust = 0)))
  
###### Fig1 - Boxplot of mussel sizes across life stages #####

  # Make boxplot w/ points @ min adult size across tide heights
  # make plot
  Fig1 <- ggplot() + 
    # lines deliniating collection sizes
    geom_hline(yintercept = 20, linetype = 'longdash', 
               color = discr_starfish[5]) + 
    geom_hline(yintercept = 30, linetype = 'longdash', 
               color = discr_starfish[5]) +
    # boxplot of mussel sizes (outliers not included)
    geom_boxplot(data = size, 
            mapping = aes(x = Lifestage, y = Leng, 
            fill = Lifestage), outlier.shape = NA) +
    # sample sizes
    geom_text(data = size %>% count(Lifestage, Tideht), 
              mapping = aes(x = Lifestage, y = -4,
                            label = paste('(',n,')'))) +
    facet_wrap(~Tideht, strip.position = 'bottom') + 
    xlab('Tide height and lifestage') + 
    ylab('Mussel length (mm)') + 
    scale_fill_manual(values = c(discr_starfish[4], 
                                 discr_starfish[1])) +
    # add room for sample sizes
    coord_cartesian(ylim = c(-5,100)) + 
    theme(legend.position = 'none')

  # call plot
  Fig1
  
  # save
  ggsave('Fig1_Lifestage_Size_Boxplot.png', 
         width = 4, height = 3, units = 'in', dpi = 300)

##### Clean up workspace for next segment #####
remove(adult_sm)
remove(adult.dunn)
remove(juv.dunn)
remove(adult)
remove(juvenile)
remove(sizemodel)
remove(adultmodel)

##### Load & linear model for in vivo mussel body/air temp #####
invivo <- read_csv('BodyEnv_Temp_Data_Summary.csv')

# Create factor (life stage)
invivo$lifestage <- ordered(invivo$lifestage, levels = unique(invivo$lifestage))
  
# ANCOVA - Test if slopes significantly different across groups
# source: http://r-eco-evo.blogspot.com/2011/08/comparing-two-regression-slopes-by.html

  interact_model <- aov(bodyenv_dif_avg ~ ambtemp_avg*lifestage, data = invivo)
  # factor = life stage
  # dependent variable = temp difference (bodyenv_dif_avg)
  # independent variable = ambient temp (ambtemp_avg)
  
  summary(interact_model)
  # no significant interaction (NSD in slope b/w body temp & amb temp across life stages)

# More parsimonious model
  simplified_model <- aov(bodyenv_dif_avg ~ ambtemp_avg + lifestage, data = invivo)
  
  summary(simplified_model)
  # there is NSD in intercepts for life stages
  
  # conclusion: fit single linear regression for all life stages

# Is there a significant linear relationship b/w body-env temp and ambient temp for M. californianus?
  linearmodel <- lm(bodyenv_dif_avg ~ ambtemp_avg, data = invivo)

##### Fig2 - linreg between body-env and amb temp #####
  
  Fig2 <- ggplot(data = invivo, mapping = aes(x = ambtemp_avg, y = bodyenv_dif_avg)) + 
    geom_hline(yintercept = 0, linetype = 'longdash', color = discr_starfish[5]) + 
    geom_point(aes(color = lifestage)) + 
    geom_smooth(method = 'lm', color = 'black') +
    geom_text(mapping = aes(x = 42.5, y = 1, label = 'Mussels = ambient'), color = discr_starfish[5], hjust = 1) + 
    geom_text(mapping = aes(x = 42.5, y = 4, label = 'Mussels hotter than ambient'), color = discr_starfish[5], hjust = 1) + 
    geom_text(mapping = aes(x = 42.5, y = -2.5, label = 'Mussels cooler than ambient'), color = discr_starfish[5], hjust = 1) + 
    xlab('Ambient temperature (°C)') + 
    ylab('Body - ambient temperature (°C)') + 
    coord_cartesian(xlim = c(17.5, 42)) + 
    scale_color_manual(values = c(discr_starfish[1], 
                                  discr_starfish[4], 
                                  discr_starfish[2]),
                       name = 'Life stage') +
    theme(axis.text.x = element_text(angle = 0, 
               hjust = 0.5, size = 12, color = 'black')) 
    
    Fig2
    
    ggsave('Fig2_BodyAmbient_Linreg.png', 
           width = 6, height = 4, units = 'in', dpi = 300)
    
  # Fig 2 Version 2 (dif linear regressinos for each life stage)
    # Adult	34.66291155	y = -0.3257x + 6.2903
    # Juvenile	33.96752196	y = -0.4077x + 7.711
    # Recruit	27.52379333	y = -0.3828x + 6.453
    
    
    Fig2b <- ggplot(data = invivo, mapping = aes(x = ambtemp_avg, y = bodyenv_dif_avg)) + 
      geom_hline(yintercept = 0, linetype = 'longdash', color = discr_starfish[5]) + 
      geom_point(aes(color = lifestage)) + 
      # recruit line
      geom_smooth(data = filter(invivo, lifestage == 'Recruit'),
               method = 'lm', 
          color = discr_starfish[2], fill = discr_starfish[2]) +
      # juvenile line
      geom_smooth(data = filter(invivo, lifestage == 'Juvenile'),
                  method = 'lm', 
          color = discr_starfish[4], fill = discr_starfish[4]) +
      # adult line
      geom_smooth(data = filter(invivo, lifestage == 'Adult'),
                  method = 'lm', 
          color = discr_starfish[1], fill = discr_starfish[1]) +
      geom_text(mapping = aes(x = 42.5, y = 1, label = 'Mussels = ambient'), color = discr_starfish[5], hjust = 1) + 
      geom_text(mapping = aes(x = 42.5, y = 4, label = 'Mussels hotter than ambient'), color = discr_starfish[5], hjust = 1) + 
      geom_text(mapping = aes(x = 42.5, y = -2.5, label = 'Mussels cooler than ambient'), color = discr_starfish[5], hjust = 1) + 
      xlab('Ambient temperature (°C)') + 
      ylab('Body - ambient temperature (°C)') + 
      coord_cartesian(xlim = c(17.5, 42)) + 
      scale_color_manual(values = c(discr_starfish[1], 
                                    discr_starfish[4], 
                                    discr_starfish[2]),
                         name = 'Life stage') +
      theme(axis.text.x = element_text(angle = 0, 
                                       hjust = 0.5, size = 12, color = 'black')) 
    
    Fig2b
    
    ggsave('Fig2b_BodyAmbient_Linreg.png', 
           width = 6, height = 4, units = 'in', dpi = 300)
    
    # save version w/o CIs around lines
  
    Fig2c <- ggplot(data = invivo, mapping = aes(x = ambtemp_avg, y = bodyenv_dif_avg)) + 
      geom_hline(yintercept = 0, linetype = 'longdash', color = discr_starfish[5]) + 
      geom_point(aes(color = lifestage)) + 
      # recruit line
      geom_smooth(data = filter(invivo, lifestage == 'Recruit'),
                  method = 'lm', 
                  color = discr_starfish[2], fill = discr_starfish[2], se = FALSE) +
      # juvenile line
      geom_smooth(data = filter(invivo, lifestage == 'Juvenile'),
                  method = 'lm', 
                  color = discr_starfish[4], fill = discr_starfish[4], se = FALSE) +
      # adult line
      geom_smooth(data = filter(invivo, lifestage == 'Adult'),
                  method = 'lm', 
                  color = discr_starfish[1], fill = discr_starfish[1], se = FALSE) +
      geom_text(mapping = aes(x = 42.5, y = 1, label = 'Mussels = ambient'), color = discr_starfish[5], hjust = 1) + 
      geom_text(mapping = aes(x = 42.5, y = 4, label = 'Mussels hotter than ambient'), color = discr_starfish[5], hjust = 1) + 
      geom_text(mapping = aes(x = 42.5, y = -2.5, label = 'Mussels cooler than ambient'), color = discr_starfish[5], hjust = 1) + 
      xlab('Ambient temperature (°C)') + 
      ylab('Body - ambient temperature (°C)') + 
      coord_cartesian(xlim = c(17.5, 42)) + 
      scale_color_manual(values = c(discr_starfish[1], 
                                    discr_starfish[4], 
                                    discr_starfish[2]),
                         name = 'Life stage') +
      theme(axis.text.x = element_text(angle = 0, 
                                       hjust = 0.5, size = 12, color = 'black')) 
    
    Fig2c
    
    
    ggsave('Fig2c_BodyAmbient_Linreg.png', 
           width = 6, height = 4, units = 'in', dpi = 300)
    
##### Cleanup environment #####
remove(interact_model)
remove(simplified_model)
remove(linearmodel)

##### Fig 3 - thermal sensitivity across life stages #####
# load data
ltdata <- read_csv("LT50_Summary_Data.csv")

# Is there a significant difference in LT50 across life stages? 
ltmodel <- aov(lt50 ~ lifestage, data = ltdata)

  # significant difference, follow-up with pairwise post-hoc       comparisons
  TukeyHSD(ltmodel)
  # recruits are significantly more sensitive than juveniles or    adults (juveniles and adults are NSD)

# summarize data
  ltdata_summary <- ltdata %>%
    group_by(lifestage) %>%
    summarize(avg = mean(lt50),
              sd = sd(lt50))

# Make barplot w SD errors
  Fig3 <- ggplot(data = ltdata_summary) + 
    geom_col(mapping = aes(x = lifestage, y = avg, fill = lifestage)) +
    xlab('Life stage') + 
    ylab (expression('LT'[50]*' (°C)')) + 
    scale_fill_manual(values = c(discr_starfish[1], 
                                  discr_starfish[4], 
                                  discr_starfish[2]),
                       name = 'Life stage') +
    geom_errorbar(mapping = aes(x = lifestage, ymin = (avg - sd), ymax = (avg + sd)), width = 0.25) + 
    geom_text(mapping = aes(x = lifestage, y = (avg + sd + 2)), label = c('a', 'a', 'b')) +
    theme(legend.position = 'none')
  
  Fig3
  
  ggsave('Fig3_LT50_Barplot.png', 
         width = 6, height = 4, units = 'in', dpi = 300)
    
##### Cleanup environment #####
remove(ltmodel)

##### Fig 4a and 4b - TSM and temp distribution #####
# Load & Tidy data
  
# Exposure data (temp records 2015-2017)
 tempdata <- read_csv('iButton_FilledIn_5_5_19.csv')
  
  # get daytime air temp @ mid tide height, filled-in only
  tempdata <- tempdata %>%
    filter(tideht == 'mid') %>%
    filter(light == 'light') %>%
    select(nearestthirty, fill) %>%
    filter(!is.na(fill))
  
# Generate TSM data (ambient only and well-informed approaches)
  min(tempdata$fill) # min is 6.4*C
  max(tempdata$fill) # max is 43.2*C
  
  # make range from 6 - 44*C for predictions to match range
  TSM <- tibble(ambtemp = seq(from = 6, to = 44, by = 1))
  
  # add difference between temp and LT50 (naive approach)
  
  TSM <- TSM %>%
  mutate(
      # naive approach for 3 life stages (LT50 - ambient temp)
      adult_n = 
      filter(ltdata_summary, lifestage == 'Adult')$avg 
      - ambtemp,
      
      juv_n = 
      filter(ltdata_summary, lifestage == 'Juvenile')$avg 
      - ambtemp, 
      
      rec_n = 
      filter(ltdata_summary, lifestage == 'Recruit')$avg 
      - ambtemp,
      
      # well-informed appraoch for 3 life stages (LT50 - amb temp discounted by negative linear relationship b/w )
      adult_wi = 
      filter(ltdata_summary, lifestage == 'Adult')$avg 
      -(ambtemp+((-0.3257*ambtemp)+6.2903)), 
      
      juv_wi = 
      filter(ltdata_summary, lifestage == 'Juvenile')$avg 
      -(ambtemp+((-0.4077*ambtemp)+7.711)),
      
      rec_wi = 
      filter(ltdata_summary, lifestage == 'Recruit')$avg 
      -(ambtemp+((-0.3828*ambtemp)+6.453))
      ) 
  
# Make main figure (linregs)

  Fig4a <- ggplot(data = TSM) + 
    geom_rect(mapping = aes(xmin = min(ambtemp), xmax = max(ambtemp), ymin = min(TSM), ymax = 0), fill = 'lightcoral') + 
    # naive approach
    geom_line(mapping = aes(x = ambtemp, y = adult_n),
              color = discr_starfish[1], linetype = 'longdash',
              size = 1) +
    geom_line(mapping = aes(x = ambtemp, y = juv_n),
              color = discr_starfish[4], linetype = 'longdash',
              size = 1) +
    geom_line(mapping = aes(x = ambtemp, y = rec_n),
              color = discr_starfish[2], linetype = 'longdash',
              size = 1) +
    # well-informed approach
    geom_line(mapping = aes(x = ambtemp, y = adult_wi),
              color = discr_starfish[1], size = 1) +
    geom_line(mapping = aes(x = ambtemp, y = juv_wi),
              color = discr_starfish[4], size = 1) +
    geom_line(mapping = aes(x = ambtemp, y = rec_wi),
              color = discr_starfish[2], size = 1) + 
    xlab('Ambient Temperature (°C)') + 
    ylab ('TSM (°C)') 
  
  Fig4a
  
  ggsave('Fig4a_TSM.png', 
         width = 4, height = 4, units = 'in', dpi = 300)

  Fig4b <- ggplot(data = tempdata, mapping = aes(x = fill)) + 
                    geom_histogram(stat = '')
  Fig4b
    
  ggsave('Fig4b_Temp_Histogram.png', 
         width = 4, height = 2, units = 'in', dpi = 300)
    
  # add frequency diagram
  
  
    
  
  
  
