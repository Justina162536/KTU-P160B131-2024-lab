library(tidyverse)
library(plotly)
library(lubridate)
install.packages("plotly")
install.packages("ggplot2")
library(ggplot2)
data <- readRDS("../data/duomenys.rds")

# 2.1 Histograma

hist <- ggplot(data = data, aes(x = avgWage))+
  geom_histogram(bins = 100, fill = "lightblue", color = "black", linewidth = 0.5)+
  labs(title = "Visdutinio atlyginimo histograma")

ggsave("../img/uzd1.png",hist,  width = 3000, height = 1500, units = ("px"))

# 2.2 Top 5 įmonės pagal faktinį sumokėtą darbo užmokestį, jų vidutinio atlyginimo kitimas
top5 <- data %>%
  group_by(name) %>%
  summarise(didWage = max(avgWage)) %>%
  arrange(desc(didWage)) %>%
  head(5)

p <- data %>%
  filter(name %in% top5$name) %>%
  mutate(Mėnesiai = ym(month)) %>%
  ggplot(aes(x = Mėnesiai, y = avgWage, color = name)) +
  geom_line()+theme_classic()+
  labs(title = "Top 5 įmonių atlyginimų kitimas per metus", 
       color = "Įmonės pavadinimas")


 
ggsave("../img/uzd2.png", p, width = 3000, height = 1500, units = ("px"))

# 2.3 užduotis

dr = data %>% 
  filter(name%in%top5$name) %>% 
  group_by(name) %>% 
  summarise(Insured = max(numInsured)) %>% 
  arrange(desc(Insured))

dr$name = factor(dr$name, levels = dr$name[order(dr$Insured, decreasing = T)])

im3 = dr %>% 
  ggplot(aes(x = name, y = Insured, fill = name)) + geom_col() +
  theme_classic()+ labs(title = "Top 5 įmonių apdraustųjų darbuotojų skaičius", 
                        fill = "Įmonės pavadinimas")

ggsave("../img/uzd3.png", im3, width = 3000, height = 1500, units = ("px"))


