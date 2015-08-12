# hi France

source("load.r")
source("functions.r")
source("functions-fr.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos_an.zip"))
  unzip("photos_an.zip")

dir.create("photos_an", showWarnings = FALSE)

if (file.exists("photos_se.zip"))
  unzip("photos_se.zip")

dir.create("photos_se", showWarnings = FALSE)

if (file.exists("raw_an.zip"))
  unzip("raw_an.zip")

if (file.exists("raw_se.zip"))
  unzip("raw_se.zip")

dir.create("raw_an"          , showWarnings = FALSE)
dir.create("raw_an/bills"    , showWarnings = FALSE)
dir.create("raw_se"          , showWarnings = FALSE)
dir.create("raw_se/senateur" , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "France",
  "lang" = "fr", # Wikipedia language for chamber and constituencies
  "an" = "Assemblée_nationale_(France)",
  "se" = "Sénat_(France)",
  "type-an" = "Lower",
  "type-se" = "Upper",
  "ipu-an" = 2113,
  "ipu-se" = 2114,
  "seats-an" = 577,
  "seats-se" = 348
)

sessions = 8:14
legs = c("1" = 1958, "2" = 1962, "3" = 1967, "4" = 1968, "5" = 1973,
         "6" = 1978, "7" = 1981, "8" = 1986, "9" = 1988, "10" = 1993,
         "11" = 1997, "12" = 2002, "13" = 2007, "14" = 2012, "15" = 2017)

# build routine

source("sponsors-an.r")
source("dossiers-an.r")
source("build-an.r")
source("comm-an.r")

source("sponsors-se.r")
source("dossiers-se.r")
source("build-se.r")
source("comm-se.r")

save(list = ls(pattern = "^(co)?(net|edges|bills)_fr_(an|se)\\d{4}$"), 
     file = "data/net_fr.rda")

# have a nice day
