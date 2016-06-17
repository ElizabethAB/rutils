# A utility for when I find myself on a new computer and need to reinstall
# all of the packages and tools I use every day.

install.packages(c("dplyr", "ggmap", "tidyr", "readxl", "devtools", "readr",
                   "haven", "pryr", "rgdal", "maptools", "RODBC", "installr",
                   "ctv"))

installr::install.Rtools()
installr::install.RStudio()
installr::install.notepadpp()
installr::install.npptor()
installr::install.git()
installr::install.MikTeX()

ctv::install.views(c("Econometrics", "Graphics", "HighPerformanceComputing",
                     "Robust", "ReproducibleResearch"))


