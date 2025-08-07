# Explore pre vs. post COVID eviction rates based on landlord behaviors

# Libraries ----
library(tidyverse)
library(car)
library(GGally)
library(broom)
library(ggeffects)
library(modelsummary)

#2024 ----
# Eviction summaries
df24 <- read_csv("modeling/data/collapsed24.csv")

# N Landlords 
n_landlords24 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2024") %>%
  select(plaintiff_name) %>%
  n_distinct() #17028

# N Filings
n_evictions24 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2024") %>%
  nrow()

# Scale and center-mean values 
df24 <- df24 %>% 
  mutate(pct_bus_width = cut(pct_business, 3),
         pct_bus_size = factor(ntile(pct_business, 3)),
         pct_bus_man = cut(pct_business, c(0,30,50,100)),
         scale_burden = c(scale(pct_burdened)),
         cm_burden = pct_burdened - mean(pct_burdened, na.rm = TRUE),
         cm_mobility = pct_moved - mean(pct_moved, na.rm = TRUE),
         scale_mobility = c(scale(pct_moved)),
         cm_students = pct_students - mean(pct_students, na.rm = TRUE),
         scale_students = c(scale(pct_students)),
         cm_exploit = exploit - mean(exploit, na.rm = TRUE),
         scale_exploit = c(scale(exploit)),
         cm_white = percent_white - mean(percent_white, na.rm = TRUE),
         scale_white = c(scale(percent_white)),
         cm_diswb = dissim_wb - mean(dissim_wb, na.rm = TRUE),
         scale_diswb = c(scale(dissim_wb)))

df24_plt <- df24 %>%
  mutate(bus_lab = case_when(
    pct_bus_man == "(0,30]" ~ "0-30%",
    pct_bus_man == "(30,50]" ~ "31-50%",
    pct_bus_man == "(50,100]" ~ "51-100%")) %>%
  drop_na(pct_bus_man)

df24_plt$bus_lab <- factor(df24_plt$bus_lab, levels = c("0-30%", "31-50%", "51-100%"))

ggplot(df24_plt, aes(bus_lab, fill = bus_lab)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)), position = position_dodge(), color = "black", size = 6, vjust = -0.2) +
  theme_bw() +
  scale_fill_manual(values = c("#aadc32ff", "#2c728eff", "#481f70ff")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(x = "Percent of Landlords Operating as a Business",
       y = "Number of Counties",
       title = paste0("Landlords: ", scales::comma(n_landlords24))) +
  ylim(0, 67)

ggsave("modeling/images/landlords2024.png")



#Model ----

summary(df24$scale_burden) #(-3.4 to 2.3, mean = 0)

m1_man_e24 <- glm(total_filed24 ~ pct_bus_man*scale_burden + 
                    scale_mobility + scale_students +  
                    scale_exploit + scale_white + scale_diswb + offset(log(rental_units)),
                  data = df24, family = "poisson")

m1_e24_tidy <- tidy(m1_man_e24, conf.int = TRUE, conf.level = 0.9)

m1_e24_tidy <- m1_e24_tidy %>% 
  mutate(terms = factor(term, levels = c("(Intercept)", "pct_bus_man(30,50]",
                                         "pct_bus_man(50,100]", "scale_burden",
                                         "pct_bus_man(30,50]:scale_burden",
                                         "pct_bus_man(50,100]:scale_burden",
                                         "scale_mobility", "scale_students",
                                         "scale_exploit", "scale_white",
                                         "scale_diswb"),
                        labels = c("(Intercept)", "31-50% Landlords Businesses", 
                                   ">51% Landlords Businesses", "Percent Rent Burdened",
                                   "31-50% Landlords Business\n*Percent Rent Burdened",
                                   ">51% Landlords\n*Percent Rent Burdened",
                                   "Residential Mobility", "Percent Students",
                                   "Rent Exploitation", "Percent White", 
                                   "Black/White\nResidential Dissimilarity")))

m1_e24_tidy %>%
  filter(!terms %in% "(Intercept)") %>%
  ggplot( aes(x = estimate, y = fct_rev(terms))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkblue") +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18)) +
  labs(x = "Estimate",
       y = NULL,
       title = paste0("Filings: ", scales::comma(sum(df24$total_filed24))))

ggsave("modeling/images/coefficients24.png")

## Effect Sizes ----

m1_e24_eff <- predict_response(m1_man_e24, terms = c("scale_burden [-3,-1.5,0,1.5,3]", "pct_bus_man"),
                               condition = c(rental_units = 3000))

ggplot(m1_e24_eff, aes(x, predicted, group = group)) + 
  geom_ribbon(aes(ymax = conf.high, ymin = conf.low), alpha = 0.2) +
  geom_line(aes(color = group), linewidth = 2) +
  theme_bw() +
  scale_color_manual("Landlord\nBusiness\nComposition", values = c("#8fbd20", "#48a2c6", "#7e36c3"),
                     labels = c("< 30%", "31-50%", "> 51%")) +
  labs(x = "Distance from Average Rent Burden",
       y = "Predicted Evictions",
       title = "2024") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

ggsave("modeling/images/effects24.png")

# 2019 ----
# Eviction summaries
df19 <- read_csv("modeling/data/collapsed19.csv")

# N Landlords 
n_landlords19 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2019") %>%
  select(plaintiff_name) %>%
  n_distinct() #22316

# N Filings
n_evictions19 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2019") %>%
  nrow()

# Scale and center-mean values 
df19 <- df19 %>% 
  mutate(pct_bus_width = cut(pct_business, 3),
         pct_bus_size = factor(ntile(pct_business, 3)),
         pct_bus_man = cut(pct_business, c(0,30,50,100)),
         scale_burden = c(scale(pct_burdened)),
         cm_mobility = pct_moved - mean(pct_moved, na.rm = TRUE),
         scale_mobility = c(scale(pct_moved)),
         cm_students = pct_students - mean(pct_students, na.rm = TRUE),
         scale_students = c(scale(pct_students)),
         cm_exploit = exploit - mean(exploit, na.rm = TRUE),
         scale_exploit = c(scale(exploit)),
         cm_white = percent_white - mean(percent_white, na.rm = TRUE),
         scale_white = c(scale(percent_white)),
         cm_diswb = dissim_wb - mean(dissim_wb, na.rm = TRUE),
         scale_diswb = c(scale(dissim_wb)))


df19_plt <- df19 %>%
  mutate(bus_lab = case_when(
    pct_bus_man == "(0,30]" ~ "0-30%",
    pct_bus_man == "(30,50]" ~ "31-50%",
    pct_bus_man == "(50,100]" ~ "51-100%")) %>%
  drop_na(pct_bus_man)

df19_plt$bus_lab <- factor(df19_plt$bus_lab, levels = c("0-30%", "31-50%", "51-100%"))

ggplot(df19_plt, aes(bus_lab, fill = bus_lab)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)), position = position_dodge(), color = "black", size = 5, vjust = -0.2
  ) +
  theme_bw() +
  scale_fill_manual(values = c("#aadc32ff", "#2c728eff", "#481f70ff")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(x = "Percent of Landlords Operating as a Business",
       y = "Number of Counties",
       title = paste0("Landlords: ", scales::comma(n_landlords19))) +
  ylim(0, 88)

ggsave("modeling/images/landlords2019.png")


## Model ----
m3_man_e19 <- glm(total_filed19 ~ pct_bus_man*scale_burden + 
                    scale_mobility + scale_students +  
                    scale_exploit + scale_white + scale_diswb + offset(log(rental_units)),
                  data = df19, family = "poisson")

summary(df19$scale_burden) # -2.99 to 2.53, mean = 0

m3_e19_tidy <- tidy(m3_man_e19, conf.int = TRUE, conf.level = 0.9)

m3_e19_tidy <- m3_e19_tidy %>% 
  mutate(terms = factor(term, levels = c("(Intercept)", "pct_bus_man(30,50]",
                                         "pct_bus_man(50,100]", "scale_burden",
                                         "pct_bus_man(30,50]:scale_burden",
                                         "pct_bus_man(50,100]:scale_burden",
                                         "scale_mobility", "scale_students",
                                         "scale_exploit", "scale_white",
                                         "scale_diswb"),
                        labels = c("(Intercept)", "31-50% Landlords Businesses", 
                                   ">51% Landlords Businesses", "Percent Rent Burdened",
                                   "31-50% Landlords Business\n*Percent Rent Burdened",
                                   ">51% Landlords\n*Percent Rent Burdened",
                                   "Residential Mobility", "Percent Students",
                                   "Rent Exploitation", "Percent White", 
                                   "Black/White\nResidential Dissimilarity")))

m3_e19_tidy %>%
  filter(!terms %in% "(Intercept)") %>%
  ggplot( aes(x = estimate, y = fct_rev(terms))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkblue") +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(x = "Estimate",
       y = NULL,
       title = paste0("Filings: ", scales::comma(sum(df19$total_filed19))))

ggsave("modeling/images/coefficients19.png")

## Effect Sizes ----

m3_e19_eff <- predict_response(m3_man_e19, terms = c("scale_burden [-3,-1.5,0,1.5,3]", "pct_bus_man"),
                               condition = c(rental_units = 3000))

ggplot(m3_e19_eff, aes(x, predicted, group = group)) + 
  geom_ribbon(aes(ymax = conf.high, ymin = conf.low), alpha = 0.2) +
  geom_line(aes(color = group), linewidth = 2) +
  theme_bw() +
  scale_color_manual("Landlord\nBusiness\nComposition", values = c("#8fbd20", "#48a2c6", "#7e36c3"),
                     labels = c("<30%", "31-50%", ">51%")) +
  labs(x = "Distance from Average Rent Burden",
       y = NULL,
       title = "2019") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

ggsave("modeling/images/effects19.png")

