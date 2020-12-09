# Load packages -----------------------------------------------------------

## basic utility
# install.packages('extrafont', dependencies = TRUE)
# library(extrafont)
# font_import()

## load fonts  must load before ggplot2
suppressMessages(extrafont::loadfonts(device = "postscript"))
## each time after install a new font,
## have to import it to R with extrafont::font_import() 
## see available fonts, use 
## extrafont::fonts()
## theme_minimal(base_family = "Comfortaa", base_size = 14) +
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("summarize", "dplyr")

library(tidyverse)
conflict_prefer("alpha", "ggplot2")
conflict_prefer("annotate", "ggplot2")
library(here)
conflict_prefer("here", "here")
library(lubridate)
library(zoo)
library(rvg)
library(patchwork)
library(emojifont)
library(export)
library(scales)
library(readxl)
library(ggpmisc)
library(Cairo)
library(grid)
library(gridExtra)
windowsFonts(Times=windowsFont("TT Times New Roman"))
library(patchwork)

library(Hmisc)

library(readxl)

#library(stargazer)
library(xtable)
#devtools::install_github("cmartin/ggConvexHull")
#library(ggConvexHull)

# library(ggrepel)
# library(ggstance)
# library(ggsci)
library(ggthemes)
library(ggpubr)
library(PerformanceAnalytics)

library(utf8)
library(Unicode)
# library(devtools)
# devtools::install_github('')
## for text analysis
# library(stringr)
# library(rebus)

## expression ###
## library(latex2exp)
## quickly test
# plot(TeX("A $\\LaTeX$ formula: $\\frac{2hc^2}{\\lambda^5} \\, 
#               \\frac{1}{e^{\\frac{hc}{\\lambda k_B T}} - 1}$"), cex=2)
# latex2exp_examples()
# latex2exp_supported(plot=TRUE)
## double check time zone
Sys.timezone()

### observation dates for ET partitioning in tallgrass prairie study
my_observation_dates <- 
  c(lubridate::ymd("2016-06-04"),
    lubridate::ymd("2016-06-06"),
    lubridate::ymd("2016-06-08"),
    lubridate::ymd("2016-06-10"),
    lubridate::ymd("2016-06-12"),
    lubridate::ymd("2016-06-28"),
    lubridate::ymd("2016-06-29"),
    lubridate::ymd("2016-06-30"))

my_observation_dates_1 <- 
  c(lubridate::ymd("2016-06-04"),
    lubridate::ymd("2016-06-06"),
    lubridate::ymd("2016-06-08"),
    lubridate::ymd("2016-06-10"),
    lubridate::ymd("2016-06-12"),
    lubridate::ymd("2016-06-27"),
    lubridate::ymd("2016-06-28"),
    lubridate::ymd("2016-06-29"),
    lubridate::ymd("2016-06-30"))
