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

# View it

# View(df.temps.long)
# write it!

setwd("/home/ivan/temp_mora/podaci")
# setwd("/home/ivan/mali_git_projekti/temp_mora/podaci")
write.csv2(df.temps.long, file.to.write, row.names = FALSE)

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

df.temps.long <- map_df(csv.only, read_tempZ) %>% select(-X1)

gradovi.s.plutacom <- c("Dubrovnik", "Zadar", "Mali Lošinj", "Malinska", "Crikvenica") # od juga prema sjeveru

# # fct_reorder2 je OK, ali je osjetljiv na zadnji rezultat, bolje ovako...
# dataframe %>% mutate(grad = factor(grad, levels = gradovi.s.plutacom, ordered = TRUE))
# kad bi dataframe$grad bio faktor, koristio bi forcats::relevel(grad, gradovi.s.plutacom)

df.to.pljot <- df.temps.long %>%
  filter(grad %in% gradovi.s.plutacom) %>%
  mutate(grad = factor(grad, levels = gradovi.s.plutacom, ordered = TRUE))
  
pljot <- df.to.pljot %>%   
  ggplot(aes(x = vrijeme, y = temp, colour = grad)) +
  geom_smooth(se = FALSE) +
  geom_point(size = .2) + # data = filter(df.temps.long, grad == "Zadar"), size = 1.5) +
  labs(x = "", y = "temperatura", colour = "grad", caption = today()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# zoom in
granice <- c(
  today() - ddays(14),
  today()
  ) %>% as.POSIXct()

pljot.zoomed <- pljot +
  coord_cartesian(xlim = granice, ylim = c(20,25)) +
  geom_point(size = .5)

setwd("/home/ivan/temp_mora/graf")
# setwd("/home/ivan/mali_git_projekti/temp_mora/graf")

ggsave("temp_graf.svg", pljot, width = 210, height = 148, units = "mm")
ggsave("temp_graf_zoom.svg", pljot.zoomed, width = 210, height = 148, units = "mm")

# # broj validnih mjerenja
na.omit(df.temps.long) %>% xtabs(~grad, .) %>% sort(decreasing = TRUE)
