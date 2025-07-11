rm(list = ls())
cat("\014")

library(tidyverse)
library(countrycode)
library(readxl)
library(ggpubr)
library(data.table)
library(scales)
library(ggrepel)
library(patchwork)
options(scipen = 999)

dir.create("./figures")

## color scheme ##
color1 <- "#8E203B"
color2 <- "#B43746"
color3 <- "#C0707C"
color4 <- "#B5919A"
color5 <- "#949197"
color6 <- "#669999"
color7 <- "#336666"

data_all <- read_csv("./data/dataclean_gravity_final.csv")

### apply typology for each corridor ###
# remove zero flows
plotdata <- data_all %>% 
  group_by(orig, dest, startyear) %>% 
  mutate(totalflow = round(flow[sex == "male"] + flow[sex == "female"], 0),
         femshare = if_else(totalflow == 0, NA, round(flow[sex == "female"],0)/totalflow),
         type = case_when(
           femshare > 0.53 ~ "female-predominant",
           femshare < 0.47 ~ "male-predominant",
           femshare >= 0.47 & femshare <= 0.53 ~ "gender-balanced",
           totalflow == 0 ~ "ZERO")) %>% 
  ungroup() %>% 
  filter(!totalflow == 0) %>% 
  filter(!is.na(femshare)) %>% 
  mutate(orig_region = countrycode(sourcevar = orig, origin = "iso3c", destination = "region"),
         dest_region = countrycode(sourcevar = dest, origin = "iso3c", destination = "region"))


#################
### DOT PLOTS ###
#################

### DIRECTION ###
plotdata1 <- plotdata %>% 
  mutate(direction = case_when(# think about different names bc its not accurate like this
    INCOME.o != "High income" & INCOME.d == "High income" ~ "LMIC - HIC",
    INCOME.o == "High income" & INCOME.d != "High income" ~ "HIC - LMIC",
    INCOME.o == "High income" & INCOME.d == "High income" ~ "HIC - HIC",
    INCOME.o != "High income" & INCOME.d != "High income" ~ "LMIC - LMIC")) %>% 
  filter(!is.na(direction)) %>% 
  group_by(direction) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  group_by(direction, type) %>% 
  mutate(type_count = n()) %>% 
  ungroup() %>% 
  mutate(type_share = type_count/total_count) %>% 
  dplyr::select(direction, type, type_share) %>% 
  distinct()

direction <- ggplot(plotdata1, aes(x = type_share, y = direction)) + ### add N !
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max",
    linewidth = 1, color = "grey50") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, face = "bold", color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12),
        title = element_text(face = "bold", size = 12, color = "grey30"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = "grey30"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())+
  geom_point(aes(x = type_share, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = 1) +
  geom_point(aes(x = type_share), size = 6, shape = 21, stroke = 1, color = "white", fill = NA) +
  scale_fill_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  geom_text_repel(aes(label = paste0(round(type_share*100, 1), "%"), x = type_share, hjust = .4, vjust= -1.1, color = type),
                  fontface = "bold", family = "sans", size = 4.5, segment.color = 'transparent') +
  annotate("text", label = paste0("N = ", comma(nrow(plotdata %>% filter(!is.na(INCOME.d), !is.na(INCOME.o))))), x = max(plotdata1$type_share, na.rm = TRUE), y = -Inf, hjust = .75, vjust = -1, 
           size = 4.5, color = "#949197")+
  scale_color_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  scale_x_continuous(expand = expansion(add = c(.035, .05))) +
  scale_y_discrete(labels = c("LMIC - HIC" = bquote(bold("LMIC"%->%"HIC")), "HIC - LMIC" = bquote(bold("HIC"%->%"LMIC")),
                              "HIC - HIC" = bquote(bold("HIC"%->%"HIC")), "LMIC - LMIC" = bquote(bold("LMIC"%->%"LMIC"))))+
  labs(title = "Direction of migration flows")
direction
#ggsave("figures/dotplot1new.pdf", dpi = 300, width = 7.5, height = 6, dpi = 300)

### gender inequality index (ORIGIN) ###
## in total (without NA) there are 166 countries ranked, one third is approximately 55
plotdata2 <- plotdata %>% 
  mutate(ranktype.o = case_when(
    genderrank.o <= 55 ~ "high GE",
    56 <= genderrank.o & genderrank.o <= 110 ~"medium GE",
    genderrank.o > 110 ~ "low GE",
    is.na(genderrank.o) == TRUE ~ NA)) %>% 
  filter(!type %in% c("ZERO"),
         !is.na(type),
         !is.na(ranktype.o)) %>% 
  group_by(ranktype.o) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  group_by(ranktype.o, type) %>% 
  mutate(type_count = n()) %>% 
  ungroup() %>% 
  mutate(type_share = type_count/total_count) %>% 
  dplyr::select(ranktype.o, type, type_share) %>% 
  distinct()

genderrank1 <- ggplot(plotdata2, aes(x = type_share, y = ranktype.o)) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max",
    linewidth = 1, color = "grey50") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, face = "bold", color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        title = element_text(face = "bold", size = 12, color = "grey30"),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = "grey30"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_point(aes(x = type_share, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = 1) +
  geom_point(aes(x = type_share), size = 6, shape = 21, stroke = 1, color = "white", fill = NA) +
  scale_fill_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  geom_text_repel(aes(label = paste0(round(type_share*100, 1), "%"), x = type_share, hjust = .4, vjust= -1.1, color = type),
                  fontface = "bold", family = "sans", size = 4.5, segment.color = 'transparent') +
  annotate("text", label = paste0("N = ", comma(nrow(plotdata %>% filter(!is.na(genderrank.o))))), x = max(plotdata1$type_share, na.rm = TRUE), y = -Inf, hjust = .65, vjust = -1, 
           size = 4.5, color = "#949197")+
  scale_color_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  scale_x_continuous(expand = expansion(add = c(.035, .05))) +
  scale_y_discrete(limits = c("high GE", "medium GE", "low GE")) +
  labs(title = "Gender equality in the origin country")
genderrank1
#ggsave("figures/dotplot2new.pdf", dpi = 300, width = 7.5, height = 6, dpi = 300)

### gender inequality index (DESTINATION) ###
plotdata3 <- plotdata %>% 
  mutate(ranktype.d = case_when(
    genderrank.d <= 55 ~ "high GE",
    56 <= genderrank.d & genderrank.d <= 110 ~"medium GE",
    genderrank.d > 110 ~ "low GE",
    is.na(genderrank.d) == TRUE ~ NA)) %>% 
  filter(!type %in% c("ZERO"),
         !is.na(type),
         !is.na(ranktype.d)) %>% 
  group_by(ranktype.d) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  group_by(ranktype.d, type) %>% 
  mutate(type_count = n()) %>% 
  ungroup() %>% 
  mutate(type_share = type_count/total_count) %>% 
  dplyr::select(ranktype.d, type, type_share) %>% 
  distinct()

genderrank2 <- ggplot(plotdata3, aes(x = type_share, y = ranktype.d)) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max",
    linewidth = 1, color = "grey50") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, face = "bold", color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        title = element_text(face = "bold", size = 12, color = "grey30"),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = "grey30"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_point(aes(x = type_share, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = 1) +
  geom_point(aes(x = type_share), size = 6, shape = 21, stroke = 1, color = "white", fill = NA) +
  scale_fill_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  geom_text_repel(aes(label = paste0(round(type_share*100, 1), "%"), x = type_share, hjust = .4, vjust= -1.1, color = type),
                  fontface = "bold", family = "sans", size = 4.5, segment.color = 'transparent') +
  annotate("text", label = paste0("N = ", comma(nrow(plotdata %>% filter(!is.na(genderrank.o))))), x = max(plotdata1$type_share, na.rm = TRUE), y = -Inf, hjust = .5, vjust = -1, 
           size = 4.5, color = "#949197")+
  scale_color_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  scale_x_continuous(expand = expansion(add = c(.035, .05))) +
  scale_y_discrete(limits = c("high GE", "medium GE", "low GE")) +
  labs(title = "Gender equality in the destination country")
genderrank2
#ggsave("figures/dotplot3new.pdf", dpi = 300, width = 7.5, height = 6, dpi = 300)

### Education of females in origin country ###
plotdata4 <- plotdata %>% 
  mutate(education = case_when(
    EDUfem.o < 15 ~ "< 15%",
    EDUfem.o >= 15 & EDUfem.o <= 30 ~ "15% - 30%",
    EDUfem.o > 30 ~ "> 30%",
    is.na(EDUfem.o) == TRUE ~ NA)) %>% 
  filter(!type %in% c("ZERO"),
         !is.na(type),
         !is.na(education)) %>% 
  group_by(education) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  group_by(education, type) %>% 
  mutate(type_count = n()) %>% 
  ungroup() %>% 
  mutate(type_share = type_count/total_count) %>% 
  dplyr::select(education, type, type_share) %>% 
  distinct()

education <- ggplot(plotdata4, aes(x = type_share, y = education)) +
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max",
    linewidth = 1, color = "grey50") +
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, face = "bold", color = "grey30"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12),
        title = element_text(face = "bold", size = 12, color = "grey30"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = "grey30"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_point(aes(x = type_share, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = 1) +
  geom_point(aes(x = type_share), size = 6, shape = 21, stroke = 1, color = "white", fill = NA) +
  scale_fill_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  geom_text_repel(aes(label = paste0(round(type_share*100, 1), "%"), x = type_share, hjust = .4, vjust= -1.1, color = type),
                  fontface = "bold", family = "sans", size = 4.5, segment.color = 'transparent') +
  annotate("text", label = paste0("N = ", comma(nrow(plotdata %>% filter(!is.na(EDUfem.o))))), x = max(plotdata1$type_share, na.rm = TRUE), y = -Inf, hjust = .75, vjust = -1, 
           size = 4.5, color = "#949197")+
  scale_color_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))+
  scale_x_continuous(expand = expansion(add = c(.035, .05))) +
  scale_y_discrete(limits = c("< 15%", "15% - 30%", "> 30%"))+
  labs(title = "Share of formally educated women in origin")
education
#ggsave("figures/dotplot4new.pdf", dpi = 300, width = 7.5, height = 6, dpi = 300)

## make dot plot grid ##
grid <- (genderrank1 + genderrank2) / 
  (direction + education) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
grid <- grid + 
  plot_annotation(
    title = "Gender composition of migration flows by country characteristics",
    theme = theme(plot.title = element_text(face = "bold", size = 20, color = "grey30", 
                                            hjust = 0.5)), tag_levels = "A")
#ggsave("figures/dotplotgrid_new.pdf", dpi = 300, grid, width = 12, height = 8)


#################
### BAR PLOTS ###
#################

testdata1 <- plotdata %>% 
  dplyr::select(startyear, orig, dest, totalflow, flow, sex, femshare) %>% 
  filter(orig == "NPL" & dest == "MYS") %>% 
  arrange(orig, dest, sex, startyear)
ggplot()

barplotdata <- plotdata %>% 
  filter(orig == "VEN" & dest == "COL"|
           orig == "POL" & dest == "DEU"|
           orig == "MEX" & dest == "USA"|
           orig == "MAR" & dest == "ESP"|
           orig == "NPL" & dest == "MYS"|
           orig == "BFA" & dest == "CIV") %>% 
  mutate(country.o = countrycode(sourcevar = orig, origin = "iso3c", destination = "country.name"),
         country.d = countrycode(sourcevar = dest, origin = "iso3c", destination = "country.name"),
         corridor = paste(country.o, country.d, sep = " - "))

barplotlabels <- barplotdata %>% 
  group_by(orig, corridor, startyear) %>% 
  summarize(femshare = round(femshare*100, 1)) %>% 
  ungroup() %>% 
  distinct()

bar1 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "VEN",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "VEN",], aes(x=startyear, y=femshare*16000), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "VEN",], aes(x=startyear, y=femshare*16000), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "VEN",], aes(x=startyear, y=femshare*16000, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "", sec.axis = sec_axis(transform=~./16000, name ="share of females"), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Venezuela"%->%"Colombia")), x = "", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar1
#ggsave("./figures/bar1.pdf", dpi = 300, width = 4.5, height = 3.5)

bar2 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "POL",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "POL",], aes(x=startyear, y=femshare*3500), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "POL",], aes(x=startyear, y=femshare*3500), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "POL",], aes(x=startyear, y=femshare*3500, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "", sec.axis = sec_axis(transform=~./3500, name ="share of females"), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Poland"%->%"Germany")), x = "", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar2
#ggsave("./figures/bar2.pdf", dpi = 300, width = 4.5, height = 3.5)

bar3 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "MEX",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "MEX",], aes(x=startyear, y=femshare*30000), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "MEX",], aes(x=startyear, y=femshare*30000), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "MEX",], aes(x=startyear, y=femshare*30000, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "no. of migrants", sec.axis = sec_axis(transform=~./30000, name =""), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Mexico"%->%"United States")), x = "", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar3
#ggsave("./figures/bar3.pdf", dpi = 300, width = 4.5, height = 3.5)

bar4 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "MAR",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "MAR",], aes(x=startyear, y=femshare*3000), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "MAR",], aes(x=startyear, y=femshare*3000), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "MAR",], aes(x=startyear, y=femshare*3000, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "no. of migrants", sec.axis = sec_axis(transform=~./3000, name =""), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Morocco"%->%"Spain")), x = "", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar4
#ggsave("./figures/bar4.pdf", dpi = 300, width = 4.5, height = 3.5)

bar5 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "NPL",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "NPL",], aes(x=startyear, y=femshare*5000), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "NPL",], aes(x=startyear, y=femshare*5000), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "NPL",], aes(x=startyear, y=femshare*5000, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "no. of migrants", sec.axis = sec_axis(transform=~./5000, name =""), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Nepal"%->%"Malaysia")), x = "start of 5-year time periods", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar5
#ggsave("./figures/bar5.pdf", dpi = 300, width = 4.5, height = 3.5)

bar6 <- ggplot() +
  geom_bar(data=barplotdata[barplotdata$orig == "BFA",], 
           aes(x = startyear, y = flow, fill = sex), position="stack", stat="identity", alpha = .8)+
  geom_line(data=barplotlabels[barplotlabels$orig == "BFA",], aes(x=startyear, y=femshare*4000), size = 1) +
  geom_point(data=barplotlabels[barplotlabels$orig == "BFA",], aes(x=startyear, y=femshare*4000), size = 4, shape = 18)+
  geom_text(data = barplotlabels[barplotlabels$orig == "BFA",], aes(x=startyear, y=femshare*4000, label = paste0(femshare, "%")), vjust = -1, fontface = "bold") +
  scale_y_continuous(name = "", sec.axis = sec_axis(transform=~./4000, name ="share of females"), labels = scales::comma)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "top",
        text = element_text(size = 12, face = "bold", color = "grey30"),
        title = element_text(hjust=0.5, size = 14, color = "grey30", face = "bold"),
        strip.background = element_blank())+
  labs(title = bquote(bold("Burkina Faso"%->%"Côte d'Ivoire")), x = "start of 5-year time periods", fill = "Gender")+
  scale_fill_manual(values = c(color7, "grey"))
bar6
#ggsave("./figures/bar6.pdf", dpi = 300, width = 4.5, height = 3.5)

## make grid ##
grid <- (bar3 + bar2) / (bar4 + bar1) / (bar5 + bar6) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
grid <- grid + 
  plot_annotation(
    title = "Migration and gender composition in six selected corridors",
    theme = theme(plot.title = element_text(face = "bold", size = 20, color = "grey30", 
                                            hjust = 0.5)), tag_levels = "A")
#ggsave("figures/barplotgridnew.pdf", dpi = 300, width = 12, height = 12)


#####################
### DISTANCE PLOT ###
#####################

distancedata <- plotdata %>% 
  filter(!is.na(distance)) %>% 
  group_by(orig, dest) %>% 
  mutate(flow = mean(flow),
         femshare = mean(femshare),
         distance = mean(distance)/1000, # in km
         type = case_when( # mean corridor classification
           femshare > 0.53 ~ "female-predominant",
           femshare < 0.47 ~ "male-predominant",
           femshare >= 0.47 & femshare <= 0.53 ~ "gender-balanced")) %>% 
  ungroup() %>% 
  dplyr::select(type, distance, flow, femshare) %>% 
  distinct()
  
distance <- ggplot(distancedata)+ # country level
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12, face = "bold", color = "grey30"),
        plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
        strip.background = element_blank())+
  geom_point(aes(y = femshare, x = distance, fill = type, size = flow), 
             shape = 21, stroke = 1, color = "white")+
  annotate("text", label = paste0("N = ", comma(nrow(distancedata))), x = max(distancedata$distance, na.rm = TRUE), y = -Inf, hjust = .75, vjust = -1, 
           size = 4.5, color = color5)+
  guides(size = "none", fill = guide_legend(override.aes = list(size = 5)))+
  scale_size(range = c(1, 10))+
  scale_x_continuous(labels = scales::comma)+
  labs(y = "share of females", x = "Distance in km",
       title = "Relationship between share of female migrants and distance in a corridor")+
  scale_fill_manual(values = c("female-predominant" = "#A04D98", "male-predominant" = "#077368", "gender-balanced" = "grey"))
distance
#ggsave("figures/distanceplot.pdf", dpi = 300, height = 6, width = 7.5, dpi=300)


##################
### GINI plots ###
##################

## GINI index for gender distribution (female share vs male share)
gini <- function(x) {
  x <- sort(x)      
  n <- length(x)
  index <- 1:n
  G <- (2 * sum(index * x)) / (n * sum(x)) - (n + 1) / n
  return(G)
}

## make gini plots
# by world region (origin)
ginidata1 <- plotdata %>% 
  filter(!is.na(orig_region)) %>% 
  group_by(orig_region, startyear) %>%
  summarize(ginifem = gini(femshare),
            ginimal = gini(1-femshare)) %>% 
  ungroup()
ginidata1 <- ginidata1 %>% 
  pivot_longer(cols = c("ginifem", "ginimal"), names_to = "sex", values_to = "gini") %>% 
  mutate(sex = if_else(sex == "ginifem", "female", "male"))

ggplot(ginidata1)+
  geom_line(aes(x = startyear, y = gini, color = sex), linewidth = 1)+
  geom_point(aes(x = startyear, y = gini, fill = sex), size = 2,
             shape = 21, stroke = 1, color = "white")+
  facet_wrap(~orig_region)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        plot.title = element_text(face = "bold", size = 18, color = "grey30", hjust=.5),
        text = element_text(size = 12, face = "bold", color = "grey30"),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold", color = "grey30"))+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  labs(title = "Gini index of gender distribution by world region of origin",
       y = "Gini index", x = "start of 5-year time periods")
#ggsave("figures/gini_origin.pdf", dpi = 300, width = 12, height = 8)


# by world region (destination)
ginidata2 <- plotdata %>% 
  filter(!is.na(dest_region)) %>% 
  group_by(dest_region, startyear) %>%
  summarize(ginifem = gini(femshare),
            ginimal = gini(1-femshare)) %>% 
  ungroup()
ginidata2 <- ginidata2 %>% 
  pivot_longer(cols = c("ginifem", "ginimal"), names_to = "sex", values_to = "gini") %>% 
  mutate(sex = if_else(sex == "ginifem", "female", "male"))

ggplot(ginidata2)+
  geom_line(aes(x = startyear, y = gini, color = sex), linewidth = 1)+
  geom_point(aes(x = startyear, y = gini, fill = sex), size = 2,
             shape = 21, stroke = 1, color = "white")+
  facet_wrap(~dest_region)+
  theme_bw() + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill='transparent', color=NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color = "grey30", face="bold"),
        text = element_text(size = 12, face = "bold", color = "grey30"),
        plot.title = element_text(face = "bold", size = 18, color = "grey30", hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold", color = "grey30"))+
  scale_color_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  scale_fill_manual(values = c("female" = "#A04D98", "male" = "#077368"))+
  labs(title = "Gini index of gender distribution by world region of destination",
       y = "Gini index", x = "start of 5-year time periods")
#ggsave("figures/gini_destination.pdf", dpi = 300, width = 12, height = 8)
