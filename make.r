# hi France

source("load.r")
source("functions.r")
source("functions-fr.r")
source("functions-pgsql.r")
source("parties.r")

# folders

dir.create("data"      , showWarnings = FALSE)
dir.create("photos_an" , showWarnings = FALSE)
dir.create("photos_se" , showWarnings = FALSE)
dir.create("plots"     , showWarnings = FALSE)
dir.create("raw_an"    , showWarnings = FALSE)
dir.create("raw_se"    , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE

meta = c("France", "Parlement")
mode = "fruchtermanreingold"

sessions = 8:14
legs = c("1" = 1958, "2" = 1962, "3" = 1967, "4" = 1968, "5" = 1973,
         "6" = 1978, "7" = 1981, "8" = 1986, "9" = 1988, "10" = 1993,
         "11" = 1997, "12" = 2002, "13" = 2007, "14" = 2012)

# build routine

source("sponsors-an.r")
source("dossiers-an.r")
source("build-an.r")
source("comm-an.r")

# keep a zipped copy of the dosleg database
if(!file.exists("data/dosleg.zip"))
	download.file("http://data.senat.fr/data/dosleg/dosleg.zip",
							  "data/dosleg.zip", mode = "wb", quiet = TRUE)

source("sponsors-se.r")
source("dossiers-se.r")
source("build-se.r")
source("comm-se.r")

# have a nice day
