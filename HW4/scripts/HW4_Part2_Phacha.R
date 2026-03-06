library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
getwd()


#-------------------------------QUESION 4 ------------------------------------#

cars93 <- MASS::Cars93
nrow(cars93)

ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  
  ##lm______________________________________lm##
  #a.1) method 'lm', color '#8fe388'
  #change se = TRUE as required in b
  #add color as required in c
  
  geom_smooth(se = TRUE, method = 'lm', formula = y ~ x, color = '#8fe388' ) +
  #geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  labs(title = 'Smoothing Method: Linear Model') + # add title as required in d)
  # add theme as required in e)
  theme(plot.title = element_text(size = 14, color ='#8fe388', face = "bold" ))
  
  ggsave("plot_lm.png", path = "/Users/phacha/Documents/R Projects/HW4/outputs", width = 8, height = 6)

  
   ##glm______________________________________glm##

  #a.2) method 'glm', color '#fe8d6d'
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = 'glm', formula = y ~ x, color = '#fe8d6d' ) +
  #geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  labs(title = 'Smoothing Method: Generalized Linear Model') + # add title as required in d)
  theme(plot.title = element_text(size = 14, color ='#fe8d6d', face = "bold"))
  
  ggsave("plot_glm.png", path = "/Users/phacha/Documents/R Projects/HW4/outputs", width = 8, height = 6)

  
##gam______________________________________gam##

  #a.3) method 'gam', color '#7c6bea'
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = 'gam', formula = y ~ x, color = '#7c6bea') +
  #geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  labs(title = 'Smoothing Method: Generalized Additive Model') + # add title as required in d)
  theme(plot.title = element_text(size = 14, color ='#7c6bea', face = 'bold'))

  ggsave("plot_gam.png", path = "/Users/phacha/Documents/R Projects/HW4/outputs", width = 8, height = 6)

#-------------------------------QUESION 5 ------------------------------------#
#________________this is example from Prof. plus  TimeSeries_Trends.R_____________________#
#
#load("./preprint_growth.rda") #please change the path if needed

# head(preprint_growth)
# 
# preprint_growth %>% filter(archive == "bioRxiv") %>%
#   filter(count > 0) -> biorxiv_growth
# preprints<-preprint_growth %>% filter(archive %in%
#                                         c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
#   mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
# preprints_final <- filter(preprints, date == ymd("2017-01-01"))
# 
# 
# ggplot(preprints) +
#   aes(date, count, color = archive, fill = archive) +
#   geom_line(size = 1) +
#   scale_y_continuous(
#     limits = c(0, 600), expand = c(0, 0),
#     name = "preprints / month",
#     sec.axis = dup_axis( #this part is for the second y axis
#       breaks = preprints_final$count, #and we use the counts to position our labels
#       labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
#       name = NULL)
#   ) +
#   scale_x_date(name = "year",
#                limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
#   scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
#                      name = NULL) +
#   theme(legend.position = "none")

#________________My code start here__________________#

load("/Users/phacha/Documents/R Projects/HW4/Data/preprint_growth.rda")

head(preprint_growth)
preprint_full <- preprint_growth %>%
  drop_na() %>%
  filter(count > 0, date > ymd("2004-12-31"))

preprint_full_1 <- preprint_full %>%
  filter(archive %in% c('bioRxiv', 'F1000Research'))%>%
  mutate(archive = factor(archive, levels = c('bioRxiv', 'F1000Research')))
#This was created just for y legend labels
preprint_full_2 <- preprint_full_1 %>%
  filter(date == max(date))


ggplot(preprint_full_1, aes(x = date, y = count, color = archive)) +
  geom_line(size =1) +
  scale_color_manual(values = c("bioRxiv" = "#7c6bea", "F1000Research" = "#fe8d6d")) +
  
  
  scale_y_continuous(
    name = "Preprints/month",
    
    #Here using the snapshot dataset (preprint_full_2) just for the labels
    sec.axis = dup_axis(
      breaks = preprint_full_2$count,
      labels = preprint_full_2$archive,
      name = NULL
    )
  )+

  scale_x_date(name ='Year', limits = c(ymd("2014-02-01"), NA)) +
  labs(title = "Preprint Counts")+
  theme(legend.position = 'none') 
  
  ggsave("plot_preprint_counts.png", path = "/Users/phacha/Documents/R Projects/HW4/outputs", width = 8, height = 6)
