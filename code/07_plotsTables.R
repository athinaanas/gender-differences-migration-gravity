

library(tidyverse)
library(scales)
options(scipen = 999)

## color scheme ##
color1 <- "#8E203B"
color2 <- "#B43746"
color3 <- "#C0707C"
color4 <- "#B5919A"
color5 <- "#949197"
color6 <- "#669999"
color7 <- "#336666"

#####################
### MAIN ANALYSIS ###
#####################

# combine the different results data frames
error_hist <- error_hist %>% mutate(model = "Historical Mean") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_pers <- error_pers %>% mutate(model = "Persistence") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_ols <- error_ols %>% mutate(model = "OLS gravity") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_hurdle <- error_hurdle %>% mutate(model = "Poisson hurdle") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")

error_metrics <- rbind(error_hist, error_pers, error_ols, error_hurdle)

tabledata <- error_metrics %>% 
  group_by(model, metric) %>% 
  mutate(rank = as.factor(rank(-value, ties.method = "max")),
         model = factor(model, levels = rev(c("Historical Mean", "Persistence", "OLS gravity", "Poisson hurdle")))) %>% 
  ungroup() %>% 
  mutate(typename = case_when(
           type == "FPD" ~ paste0(sex,"s \nin ","FPD \nflows"),
           type == "MPD" ~ paste0(sex,"s \nin ", "MPD \nflows"),
           type == "GB" ~ paste0(sex,"s \nin ", "GB \nflows")),
         typename = factor(typename, levels = c("females \nin FPD \nflows", "males \nin FPD \nflows", "females \nin GB \nflows", 
                                                "males \nin GB \nflows", "females \nin MPD \nflows", "males \nin MPD \nflows")))

ggplot(tabledata, aes(x = typename, y = model, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(round(value, 2))), color = "white", size = 3.5, fontface = "bold") +
  facet_wrap(~metric, ncol = 1) +
  scale_fill_manual(values = c("1" = color1, "2" = color2, "3" = color3, "4" = color4, "5"= color6, "6" = color7),
                    labels = c("1" = "Low", "2" = "", "3" = "", "4" = "", "5" = "", "6" = "High"),
                    name = "Accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30", face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill='transparent', color=NA),
    legend.title = element_text(size = 11, color = "grey30", face = "bold"),
    legend.text = element_text(size = 11, color = "grey30", face = "bold"),
    text = element_text(size = 12),
    title = element_text(face = "bold", size = 10, color = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, colour = "grey30"),
    axis.text.x = element_text(hjust = 0.5, color = "grey30", face = "italic", size= 11),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  labs(title = "Comparison of model accuracy across flow types and error metrics")
#ggsave("figures/heatmap_errormetrics.pdf", dpi = 300, height = 7, width = 8)


## separate plot for MASE
ggplot(tabledata %>% filter(metric == "MASE"), aes(x = typename, y = model, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(round(value, 3))), color = "white", size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("1" = color1, "2" = color2, "3" = color3, "4" = color4, "5"= color6, "6" = color7),
                    labels = c("1" = "Low", "2" = "", "3" = "", "4" = "", "5" = "", "6" = "High"),
                    name = "Accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30", face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill='transparent', color=NA),
    legend.title = element_text(size = 11, color = "grey30", face = "bold"),
    legend.text = element_text(size = 11, color = "grey30", face = "bold"),
    text = element_text(size = 12),
    title = element_text(face = "bold", size = 10, color = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, colour = "grey30"),
    axis.text.x = element_text(hjust = 0.5, color = "grey30", face = "italic", size= 11),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  labs(title = "Comparison of model accuracy across flow types")
#ggsave("figures/heatmap_main_mase.pdf", dpi = 300, height = 2.5, width = 8)


#########################
### ROBUSTNESS CHECKS ###
#########################

error_model1 <- error_model1 %>% mutate(model = "Simple gravity") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_model2 <- error_model2 %>% mutate(model = "Complex gravity") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_model3 <- error_model3 %>% mutate(model = "Geographical gravity") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")

error_metrics <- rbind(error_model1, error_model2, error_model3)

tabledata <- error_metrics %>% 
  group_by(model, metric) %>% 
  mutate(rank = as.factor(rank(-value, ties.method = "max")),
         model = factor(model, levels = rev(c("Simple gravity", "Complex gravity", "Geographical gravity")))) %>% 
  ungroup() %>% 
  mutate(typename = case_when(
    type == "FPD" ~ paste0(sex,"s \nin ","FPD \nflows"),
    type == "MPD" ~ paste0(sex,"s \nin ", "MPD \nflows"),
    type == "GB" ~ paste0(sex,"s \nin ", "GB \nflows")),
    typename = factor(typename, levels = c("females \nin FPD \nflows", "males \nin FPD \nflows", "females \nin GB \nflows", 
                                           "males \nin GB \nflows", "females \nin MPD \nflows", "males \nin MPD \nflows")))

ggplot(tabledata, aes(x = typename, y = model, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(round(value, 2))), color = "white", size = 3.5, fontface = "bold") +
  facet_wrap(~metric, ncol = 1) +
  scale_fill_manual(values = c("1" = color1, "2" = color2, "3" = color3, "4" = color4, "5"= color6, "6" = color7),
                    labels = c("1" = "Low", "2" = "", "3" = "", "4" = "", "5" = "", "6" = "High"),
                    name = "Accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30", face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill='transparent', color=NA),
    legend.title = element_text(size = 11, color = "grey30", face = "bold"),
    legend.text = element_text(size = 11, color = "grey30", face = "bold"),
    text = element_text(size = 12),
    title = element_text(face = "bold", size = 10, color = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, colour = "grey30"),
    axis.text.x = element_text(hjust = 0.5, color = "grey30", face = "italic", size= 11),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  labs(title = "Comparison of model accuracy across flow types and error metrics")
#ggsave("figures/heatmap_robustchecks.pdf", dpi = 300, height = 6, width = 8)

### for total female vs. male flows ###
error_histall <- error_histall %>% mutate(model = "Historical Mean") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_persall <- error_persall %>% mutate(model = "Persistence") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_olsall <- error_olsall %>% mutate(model = "OLS gravity") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
error_hurdleall <- error_hurdleall %>% mutate(model = "Poisson hurdle") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")

error_metrics <- rbind(error_histall, error_persall, error_olsall, error_hurdleall)

tabledata <- error_metrics %>% 
  group_by(model, metric) %>% 
  mutate(rank = as.factor(rank(-value, ties.method = "max")),
         model = factor(model, levels = rev(c("Historical Mean", "Persistence", "OLS gravity", "Poisson hurdle")))) %>% 
  ungroup()

ggplot(tabledata, aes(x = sex, y = model, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(round(value, 2))), color = "white", size = 3.5, fontface = "bold") +
  facet_wrap(~metric, ncol = 1) +
  scale_fill_manual(values = c("1" = color3, "2" = color6),
                    labels = c("1" = "Low", "2" = "High"),
                    name = "Accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30", face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill='transparent', color=NA),
    legend.title = element_text(size = 11, color = "grey30", face = "bold"),
    legend.text = element_text(size = 11, color = "grey30", face = "bold"),
    text = element_text(size = 12),
    title = element_text(face = "bold", size = 10, color = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, colour = "grey30"),
    axis.text.x = element_text(hjust = 0.5, color = "grey30", face = "italic", size= 11),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  labs(title = "Comparison of model accuracy by gender and error metric")
#ggsave("figures/heatmap_totalflows.pdf", dpi = 300, height = 7, width = 8)

### for validation with other data sets ###
errorQM <- errorQM %>% mutate(dataset = "Europe estimates") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
errorAP <- errorAP %>% mutate(dataset = "Asia Pacific estimates") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
errorLA <- errorLA %>% mutate(dataset = "Latin America estimates") %>% pivot_longer(cols = c("MAE", "MAPE", "MASE"), names_to = "metric", values_to = "value")
errorWL <- error_ols %>% mutate(dataset = "Global estimates") %>% dplyr::select(-model)

error_metrics <- rbind(errorWL, errorQM, errorAP, errorLA)

tabledata <- error_metrics %>% 
  group_by(dataset, metric) %>% 
  mutate(rank = as.factor(rank(-value, ties.method = "max")),
         rank = if_else(value == Inf, NA, rank),
         value = if_else(value == Inf, NA, value),
         dataset = factor(dataset, levels = rev(c("Global estimates", "Europe estimates", "Asia Pacific estimates", "Latin America estimates")))) %>% 
  ungroup() %>% 
  mutate(typename = case_when(
    type == "FPD" ~ paste0(sex,"s \nin ","FPD \nflows"),
    type == "MPD" ~ paste0(sex,"s \nin ", "MPD \nflows"),
    type == "GB" ~ paste0(sex,"s \nin ", "GB \nflows")),
    typename = factor(typename, levels = c("females \nin FPD \nflows", "males \nin FPD \nflows", "females \nin GB \nflows", 
                                           "males \nin GB \nflows", "females \nin MPD \nflows", "males \nin MPD \nflows")))

ggplot(tabledata, aes(x = typename, y = dataset, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(round(value, 2))), color = "white", size = 3.5, fontface = "bold") +
  facet_wrap(~metric, ncol = 1) +
  scale_fill_manual(values = c("1" = color1, "2" = color2, "3" = color3, "4" = color4, "5"= color6, "6" = color7),
                    labels = c("1" = "Low", "2" = "", "3" = "", "4" = "", "5" = "", "6" = "High"),
                    name = "Accuracy",
                    na.value = "grey") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "grey30", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, color = "grey30", face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill='transparent', color=NA),
    legend.title = element_text(size = 11, color = "grey30", face = "bold"),
    legend.text = element_text(size = 11, color = "grey30", face = "bold"),
    text = element_text(size = 12),
    title = element_text(face = "bold", size = 10, color = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, colour = "grey30"),
    axis.text.x = element_text(hjust = 0.5, color = "grey30", face = "italic", size= 11),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())+
  labs(title = "Comparison of model accuracy across flow types and error metrics")
#ggsave("figures/heatmap_otherdata.pdf", dpi = 300, height = 6, width = 8)

# empty data folder for GitHub
unlink("./data/*", recursive = TRUE)
