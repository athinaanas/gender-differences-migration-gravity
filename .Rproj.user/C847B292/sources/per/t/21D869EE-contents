rm(list = ls())
cat("\014")

library(tidyverse)
library(countrycode)
library(ggpubr)
library(Hmisc)
library(patchwork)
library(WDI)
options(scipen = 999)


data_all <- read_csv("./data/dataclean_gravity_final.csv")

#####################
### GLOBALIZATION ###
#####################

globdata <- data_all %>% 
  group_by(orig, dest, startyear) %>% 
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0)) %>% 
  ungroup() %>% 
  filter(!totalflow == 0) %>% # remove zero flows
  dplyr::select(startyear, orig, dest, sex, flow, distance) %>% 
  distinct()

worldpop <- WDI(indicator= c("SP.POP.TOTL.FE.IN", "SP.POP.TOTL.MA.IN"), country = "all", start = 1990, end = 2015, extra = F) %>% 
  filter(country == "World",
         year %in% seq(1990, 2015, 5)) %>% 
  rename(female = SP.POP.TOTL.FE.IN,
         male = SP.POP.TOTL.MA.IN,
         startyear = year) %>% 
  dplyr::select(startyear, female, male) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "worldpop")

globdata <- left_join(globdata, worldpop)

# bootsrap function for median (takes a few minutes to run)
set.seed(123)
boot_weighted_median <- function(df, reps = 1000) {
  n <- nrow(df)
  medians <- replicate(reps, {
    # sample
    resampled <- df[sample(nrow(df), replace = TRUE), ]
    # weighted median
    wtd.quantile(resampled$distance, weights = resampled$flow, probs = 0.5, na.rm = TRUE)
  })
  quantile(medians, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
}

# apply within each group
distquantiles <- globdata %>%
  filter(!is.na(distance), !is.na(flow)) %>%
  group_by(startyear, sex) %>%
  group_modify(~ {
    qs <- boot_weighted_median(.x)
    tibble(q10 = qs[1], q50 = qs[2], q90 = qs[3])
  }) %>%
  ungroup()

qmeandistplot <- ggplot(distquantiles)+
  geom_errorbar(aes(x = startyear, ymin = q10/1000, ymax = q50/1000, color = sex), width = 0.6, size = 1.2, alpha = .5) + 
  geom_errorbar(aes(x = startyear, ymin = q50/1000, ymax = q90/1000, color = sex), width = 0.6, size = 1.2, alpha = .5) +
  geom_point(aes(x = startyear, y = q50/1000, fill = sex), size = 6, shape = 21, stroke = 1, color = "white") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "grey70", size = .5, linetype = 1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, colour = "grey30"),
        plot.title = element_text(size = 14, color = "grey30", face = "bold"),
        axis.title.x = element_text(size = 11, colour = "grey30", face = "bold"),
        axis.title.y = element_text(size = 11, colour = "grey30", face = "bold"))+
  labs(y = "distance in km", x = "",
       title = "Median migration distance weighted by flow size")+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_x_continuous(breaks = seq(1990, 2015, 5))
qmeandistplot


### Migration intensity (migration in % of world population) ###
bootquantiles <- function(df, reps = 1000) {
  bootmeans <- replicate(reps, {
    resampled <- df[sample(nrow(df), replace = TRUE), ]
    sum(resampled$flow/resampled$worldpop*100)
  })
  quantile(bootmeans, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
}

# Apply by group
migratequantiles <- globdata %>%
  group_by(startyear, sex) %>%
  group_modify(~ {
    qs <- bootquantiles(.x)
    tibble(q10 = qs[[1]], q50 = qs[[2]], q90 = qs[[3]])
  }) %>%
  ungroup()

qmigrateplot <- ggplot(migratequantiles)+
  geom_errorbar(aes(x = startyear, ymin = q10, ymax = q50, color = sex), width = 0.6, size = 1.2, alpha = .5) + 
  geom_errorbar(aes(x = startyear, ymin = q50, ymax = q90, color = sex), width = 0.6, size = 1.2, alpha = .5) +
  geom_point(aes(x = startyear, y = q50, fill = sex), size = 6, shape = 21, stroke = 1, color = "white") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "grey70", size = .5, linetype = 1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, colour = "grey30"),
        plot.title = element_text(size = 14, color = "grey30", face = "bold"),
        axis.title.x = element_text(size = 11, colour = "grey30", face = "bold"),
        axis.title.y = element_text(size = 11, colour = "grey30", face = "bold"))+
  labs(y = "migration flows in % of world population", x = "",
       title = "Global migration intensity")+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_x_continuous(breaks = seq(1990, 2015, 5))
qmigrateplot


### Emigration Spread Spread
bootquantiles <- function(df, reps = 1000) {
  bootresults <- replicate(reps, {
    resampled <- df[sample(nrow(df), replace = TRUE), ]
  
    EM_df <- resampled %>%
      group_by(orig) %>%
      dplyr::summarize(EM = sum(flow, na.rm = TRUE), .groups = "drop")
    
        M = sum(EM_df$EM)
        ESg = 1 - sum((EM_df$EM/M)^2, na.rm = TRUE)
    ESg
  })

  tibble(
    q10 = quantile(bootresults, 0.1, na.rm = TRUE),
    q50 = quantile(bootresults, 0.5, na.rm = TRUE),
    q90 = quantile(bootresults, 0.9, na.rm = TRUE)
  )
}

spreadEquantiles <- globdata %>%
  group_by(startyear, sex) %>%
  group_modify(~ bootquantiles(.x)) %>%
  ungroup()

### Immigration Spread 
bootquantiles <- function(df, reps = 1000) {
  bootresults <- replicate(reps, {
    resampled <- df[sample(nrow(df), replace = TRUE), ]
    
    IM_df <- resampled %>%
      group_by(dest) %>%
      dplyr::summarize(IM = sum(flow, na.rm = TRUE), .groups = "drop")
    
    M = sum(IM_df$IM)
    ISg = 1 - sum((IM_df$IM/M)^2, na.rm = TRUE)
    ISg
  })
  
  tibble(
    q10 = quantile(bootresults, 0.1, na.rm = TRUE),
    q50 = quantile(bootresults, 0.5, na.rm = TRUE),
    q90 = quantile(bootresults, 0.9, na.rm = TRUE)
  )
}

spreadIquantiles <- globdata %>%
  group_by(startyear, sex) %>%
  group_modify(~ bootquantiles(.x)) %>%
  ungroup()

qspreadplot1 <- ggplot(spreadEquantiles)+
  geom_errorbar(aes(x = startyear, ymin = q10, ymax = q50, color = sex), width = 0.6, size = 1.2, alpha = .5) + 
  geom_errorbar(aes(x = startyear, ymin = q50, ymax = q90, color = sex), width = 0.6, size = 1.2, alpha = .5) +
  geom_point(aes(x = startyear, y = q50, fill = sex), size = 6, shape = 21, stroke = 1, color = "white") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "grey70", size = .5, linetype = 1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, colour = "grey30"),
        plot.title = element_text(size = 14, color = "grey30", face = "bold"),
        axis.title.x = element_text(size = 11, colour = "grey30", face = "bold"),
        axis.title.y = element_text(size = 11, colour = "grey30", face = "bold"))+
  labs(y = "global emigrant spread", x = "start of 5-year periods",
       title = "Dispersion of migration across destinations")+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_x_continuous(breaks = seq(1990, 2015, 5))
qspreadplot1

qspreadplot2 <- ggplot(spreadIquantiles)+
  geom_errorbar(aes(x = startyear, ymin = q10, ymax = q50, color = sex), width = 0.6, size = 1.2, alpha = .5) + 
  geom_errorbar(aes(x = startyear, ymin = q50, ymax = q90, color = sex), width = 0.6, size = 1.2, alpha = .5) +
  geom_point(aes(x = startyear, y = q50, fill = sex), size = 6, shape = 21, stroke = 1, color = "white") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "grey70", size = .5, linetype = 1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, colour = "grey30"),
        plot.title = element_text(size = 14, color = "grey30", face = "bold"),
        axis.title.x = element_text(size = 11, colour = "grey30", face = "bold"),
        axis.title.y = element_text(size = 11, colour = "grey30", face = "bold"))+  
  labs(y = "global immigrant spread", x = "start of 5-year periods",
       title = "Dispersion of migration across origins")+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_x_continuous(breaks = seq(1990, 2015, 5))
qspreadplot2


grid <- (qmeandistplot + qmigrateplot) / 
  (qspreadplot1 + qspreadplot2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
grid <- grid + 
  plot_annotation(
    title = "Dimensions of migration globalization",
    theme = theme(plot.title = element_text(face = "bold", size = 20, color = "grey30", 
                                            hjust = 0.5)), tag_levels = "A")
#ggsave("figures/globalizationgridnew.pdf", dpi = 300, width = 12, height = 8)





