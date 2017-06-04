library(stringr); library(purrr)
library(lubridate); library(anytime)
library(XML); library(readr)
library(ggplot2)
library(forcats)
library(tidyr); library(dplyr)

bljak <- function(x) as.numeric(as.character(x))

#
theurl <- "http://vrijeme.hr/aktpod.php?id=more_n"
tables <- readHTMLTable(theurl, header = TRUE)

file.to.write <- paste0("temp_mora__", today(), ".csv")

df.temps <- tables[[5]] %>%
  as_data_frame()

df.temps.sliced <- slice(df.temps, 2:(n()-2))

# ispravi (trim) neki glupi pad_left
df.temps.sliced.2 <- df.temps.sliced %>%
  mutate(V1 = str_trim(V1))
  
df.temps.3 <- df.temps.sliced.2 %>% 
  droplevels() %>% 
  mutate_at(vars(-V1), bljak) %>% 
  setNames(c("grad", "t_06","t_07","t_10","t_13","t_14","t_16"))
  

df.temps.long <- df.temps.3 %>% 
  gather(sat, temp, t_06 : t_16) %>% 
  separate(col = sat, into = c("visak", "satt")) %>% 
  mutate(vrijeme.tmp = paste(today(), satt, sep = " "),
         vrijeme = anytime(vrijeme.tmp),
         grad = fct_relevel(grad, "Zadar")) %>% 
  select(grad, temp, vrijeme)

# write it!

setwd("/home/ivan/temp_mora/podaci")
write.csv2(df.temps.long, file.to.write)

#

# VIZZ #

# setwd("/home/ivan/Dropbox/temp_mora")

read_tempZ <- function(x) {
  read_csv2(x,
            col_types = cols(
              grad = col_character(),
              temp = col_double(),
              vrijeme = col_datetime(format = "")
            ))
}

csv.only <- list.files()[str_detect(list.files(), "csv")]

df.temps.long <- map_df(csv.only, read_tempZ)

gradovi.s.plutacom <- c("Dubrovnik", "Zadar", "Mali Lošinj", "Malinska", "Crikvenica") # od juga prema sjeveru

# # fct_reorder2 je OK, ali je osjetljiv na zadnji rezultat, bolje ovako...
# dataframe %>% mutate(grad = factor(grad, levels = gradovi.s.plutacom, ordered = TRUE))

df.to.pljot <- df.temps.long %>%
  filter(grad %in% gradovi.s.plutacom) %>%
  mutate(grad = factor(grad, levels = gradovi.s.plutacom, ordered = TRUE))
  
pljot <- df.to.pljot %>%   
  ggplot(aes(x = vrijeme, y = temp, colour = grad)) +
  geom_smooth(se = FALSE) +
  geom_point(size = .2) + # data = filter(df.temps.long, grad == "Zadar"), size = 1.5) +
  labs(x = "", y = "temperatura", colour = "grad", caption = today()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))


setwd("/home/ivan/temp_mora/graf")
ggsave("temp_graf.svg", pljot)

# # broj validnih mjerenja
na.omit(df.temps.long) %>% xtabs(~grad, .) %>% sort(decreasing = TRUE)