data = "data/sponsors-an.csv"

if (!file.exists(data)) {
  
  root = "http://www.assemblee-nationale.fr/sycomore/"
  
  sycomore = c()
  for (x in sessions) { # accepts 1:14
    
    file = paste0("raw_an/mps-", x, ".html")
    
    if (!file.exists(file))
      try(download.file(paste0(root, "result.asp?radio_dept=tous_departements&regle_nom=contient&Nom=&departement=&choixdate=intervalle&D%C3%A9butMin=&FinMin=&Dateau=&legislature=", x + 47, "&choixordre=chrono&Rechercher=Lancer+la+recherche"),
                   file, mode = "wb", quiet = TRUE), silent = TRUE)
    
    s = htmlParse(file, encoding = "UTF-8")
    s = unlist(xpathSApply(s, "//a[contains(@href, 'num_dept')]/@href"))
   
    cat("Parsing legislature", sprintf("%2.0f", x), ":", length(s), "MPs\n")
    sycomore = c(sycomore, s)
    
  }
  
  sycomore = paste0(root, unique(sycomore))
  cat("Parsing", length(sycomore), "MPs\n")
  
  # MP-level details
  dep = data_frame()
  for (i in rev(sycomore)) {
    
    file = paste0("raw_an/mps/", gsub("\\D", "", i), ".html")
    
    if (!file.exists(file))
      download.file(i, file, mode = "wb", quiet = TRUE)
    
    html = try(htmlParse(file, encoding = "UTF-8"), silent = TRUE)
    
    if ("try-error" %in% class(html)) {
      
      cat("Parser error:", i, "\n")
      
    } else {
      
      name = xpathSApply(html, "//h1[@class='deputy-headline-title']", xmlValue)
      name = str_clean(name)
      
      birth = xpathSApply(html, "//dl[@class='deputy-attribute-list']/dd/ul/li[1]", xmlValue)
      birth = str_clean(birth)
      
      born = gsub("(.*) le (.*) à (.*)", "\\2", birth)
      born = substr(strptime(born, "%d/%m/%Y"), 1, 4)
      born_loc = str_trim(gsub("(.*) à (.*)", "\\2", birth))
      
      sex = NA
      sex[ grepl("(N|n)ée le", birth) ] = "F"
      sex[ grepl("(N|n)é le", birth) ] = "M"
      
      mandate = xpathSApply(html, "//div[@id='assemblee']/*/ul/li/div[@class='article-content']/p[count(b) > 0]", 
                           xmlValue)
      mandate = mandate[ !grepl("Présidence", mandate) ]
      
      if (!length(mandate)) {
        constituency = party = NA
      } else {
        constituency = str_trim(gsub("(.*) : (.*) - (.*)", "\\2", mandate))
        party = str_trim(gsub("(.*) : (.*) - (.*)", "\\3", mandate))
      }
      
      # vector of individual mandates
      mandate = xpathSApply(html, "//div[@id='assemblee']/*/ul/li/div[@class='article-content']/p/b", xmlValue)
      mandate = str_trim(mandate[ !grepl("Présidence", mandate) ])
      
      if (!length(mandate))
        mandate = NA
      
      if (identical(c(mandate, party), c(NA, NA)))
        cat(sprintf("%4.0f", which(sycomore == i)), "[ no details at", i, "]\n")
      # else
      # cat(sprintf("%4.0f", which(sycomore == i)), nom_de_famille, "\n")
      
      # match url_an from other datasets
      url_an = xpathSApply(html, "//a[contains(@onclick, 'tribun')]/@onclick")
      url_an = as.vector(gsub("window.location=|'", "", url_an))
      url_an = ifelse(!length(url_an), NA, paste0("http://www.assemblee-nationale.fr", url_an))
      
      photo = xpathSApply(html, "//img[@class='deputy-profile-picture']/@src")
      
      dep = rbind(dep, data_frame(
        url = gsub("\\D", "", i), name,
        sex, born, born_loc,
        mandate, constituency, party, url_an,
        photo = ifelse(!length(photo), NA, photo)
      ))
      
    }
    
  }
  
  cat("Getting", n_distinct(na.omit(dep$photo)), "photos\n")
  
  for (i in unique(na.omit(dep$photo))) {
    
    photo = paste0("photos_an/", gsub("\\D", "", i), ".jpg")
    
    if (!file.exists(photo))
      download.file(paste0("http://www.assemblee-nationale.fr", str_trim(i)),
                    photo, mode = "wb", quiet = TRUE)
    
    if (!file.info(photo)$size) {
      
      file.remove(photo)
      dep$photo[ dep$photo == i ] = NA
      
    } else {
      
      dep$photo[ dep$photo == i ] = photo
      
    }
    
  }
  
  # save original name
  dep$fullname = dep$name
  
  # simplify
  dep$name = str_trim(clean_names(dep$name))

  # fix family names
  dep$name[ dep$name == "JEAN GARRAUD" ] = "JEAN PAUL GARRAUD"
  dep$name[ dep$name == "ODETTE EUGENIE GERMAINE DURIEZ NEE GAMELIN" ] = "ODETTE DURIEZ"
  dep$name[ dep$name == "CHRISTIANE TAUBIRA DELANNON" ] = "CHRISTIANE TAUBIRA"
  dep$name[ dep$name == "MARTINE AURILLAC NEE ADRIAN" ] = "MARTINE AURILLAC"
  dep$name[ dep$name == "FRANCOISE BRANGET NEE MINELLO" ] = "FRANCOISE BRANGET"
  dep$name[ dep$name == "FRANCOISE VALLET NEE JOUANNE" ] = "FRANCOISE VALLET"
  dep$name[ dep$name == "BERNADETTE PAIX NEE ABRIBAT" ] = "BERNADETTE PAIX"
  dep$name[ dep$name == "MARCELLE MAURICETTE RAMONET NEE CHITRE" ] = "MARCELLE RAMONET"
  dep$name[ dep$name == "COLETTE MARIE FRANCOISE MOAL NEE AMICEL" ] = "COLETTE MOAL"
  dep$name[ dep$name == "PAULETTE GUINCHARD KUNTZLER" ] = "PAULETTE GUINCHARD"
  dep$name[ dep$name == "JOSIANE BOYCE NEE PONTIEUX" ] = "JOSIANE BOYCE"

  # fix first names
  dep$name[ dep$name == "JACQUES MICHEL PIERRE CHABAN DELMAS" ] = "JACQUES CHABAN DELMAS"
  dep$name[ dep$name == "CHARLES PINETON CHAMBRUN" ] = "CHARLES CHAMBRUN"
  dep$name[ dep$name == "MICHEL PAUL PIERRE GHYSEL" ] = "MICHEL GHYSEL"
  dep$name[ dep$name == "JEAN NOEL LIPKOWSKI" ] = "JEAN LIPKOWSKI"
  dep$name[ dep$name == "JEAN PIERRE FERNAND SCHENARDI" ] = "JEAN PIERRE SCHENARDI"
  dep$name[ dep$name == "ROLAND LEON LOUIS DUMAS" ] = "ROLAND DUMAS"
  dep$name[ dep$name == "ANDRE MARIE ANTOINE DROITCOURT" ] = "ANDRE DROITCOURT"
  dep$name[ dep$name == "ALBERT LIKUVALU" ] = "APELETO ALBERT LIKUVALU" # longer
  dep$name[ dep$name == "MICHEL JEAN ELI ROSSI" ] = "MICHEL ROSSI"
  dep$name[ dep$name == "GUY GEORGES CAMILLE LEFRAND" ] = "GUY LEFRAND"
  dep$name[ dep$name == "PHILIPPE LUCIEN MARIE MORENVILLIER" ] = "PHILIPPE MORENVILLIER"
  dep$name[ dep$name == "RAYMOND JOANNES DURAND" ] = "RAYMOND DURAND"
  dep$name[ dep$name == "GERARD ANDRE MILLET" ] = "GERARD MILLET"
  dep$name[ dep$name == "PATRICE OLIVIER PAUL DEBRAY" ] = "PATRICE DEBRAY"
  dep$name[ dep$name == "ROBERT JEAN PIERRE MARIE DIAT" ] = "ROBERT DIAT"
  dep$name[ dep$name == "CLAUDE ANNE DARCIAUX" ] = "CLAUDE DARCIAUX"
  dep$name[ dep$name == "PHILIPPE CALIXTE GERARD EDMOND MARIETTE" ] = "PHILIPPE EDMOND MARIETTE"
  dep$name[ dep$name == "LAURENT EMILE MICHEL HENART" ] = "LAURENT HENART"
  dep$name[ dep$name == "PIERRE ADRIEN MENJUCQ" ] = "PIERRE MENJUCQ"
  dep$name[ dep$name == "JEAN PIERRE ANDRE MARCHE" ] = "JEAN PIERRE MARCHE"
  dep$name[ dep$name == "PIERRE CLAUDE LANFRANCA" ] = "CLAUDE LANFRANCA"
  dep$name[ dep$name == "AUGUSTE DAMASE LEGROS" ] = "AUGUSTE LEGROS"
  dep$name[ dep$name == "GEORGES ROBERT DELATRE" ] = "GEORGES DELATRE"
  dep$name[ dep$name == "GEORGES PAUL MARIE WAGNER" ] = "GEORGES PAUL WAGNER"
  dep$name[ dep$name == "HENRI CHARLES MICHEL" ] = "HENRI MICHEL"
  dep$name[ dep$name == "JACQUES ROBERT BAUMEL" ] = "JACQUES BAUMEL"
  dep$name[ dep$name == "ANDRE LOUIS RENE DELEHEDDE" ] = "ANDRE DELEHEDDE"
  dep$name[ dep$name == "ADRIEN JEAN LOUIS DURAND" ] = "ADRIEN DURAND"
  dep$name[ dep$name == "ELIE ARISTIDE MARTY" ] = "ELIE MARTY"
  dep$name[ dep$name == "PIERRE EMILE GUILLAIN BENOUVILLE" ] = "PIERRE BENOUVILLE"
  dep$name[ dep$name == "BERNARD CLAUDE SAVY" ] = "BERNARD SAVY"
  dep$name[ dep$name == "ALBERT ADOLPHE MARCEL BROCHARD" ] = "ALBERT BROCHARD"
  
  # fix homonyms
  dep$name[ with(dep, name == "JEAN MICHEL BOUCHERON" & constituency == "Charente") ] = "JEAN MICHEL BOUCHERON CHARENTE"
  dep$name[ with(dep, name == "JACQUES HOUSSIN" & born == "1928") ] = "JACQUES HOUSSIN NORD PREMIER"
  dep$name[ with(dep, name == "PIERRE BERNARD" & constituency == "Tarn") ] = "PIERRE BERNARD TARN"
  dep$name[ with(dep, name == "JEAN BERNARD" & constituency == "Isère") ] = "JEAN BERNARD ISERE"
  
  # detect homonyms
  d = summarise(group_by(dep, name), n = n_distinct(url))
  u = d$n > 1
  if (sum(u)) {
    cat(sum(u), "homonyms detected:\n")
    print(dep[ dep$name %in% d$name[ u ], ])
    stop()
  }
  
  dep$legislature = strptime(substr(dep$mandate, 1, 10), "%d/%m/%Y")
  dep$legislature = as.Date(substr(dep$legislature, 1, 10))
  dep$legislature = parse_legislature(dep$legislature)
  
  # fix legislature of Marc Francina (elected three months before election?!)
  # http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=12108
  dep$legislature[ dep$name == "MARC FRANCINA" & dep$legislature == 11 ] = 12
  
  # table(dep$legislature, exclude = NULL)
  dep = subset(dep, !is.na(legislature)) # NA = Fourth Republic
  
  # remove two buggy MPs
  dep = subset(dep, !is.na(party))
  
  # add them back
  dep = rbind(dep, data_frame(
    url = c("18563", "10617"),
    name = c("MEYER HABIB", "EDOUARD FRITCH"),
    sex = c("M", "M"),
    born = c("1961", "1952"),
    born_loc = c("PARIS 12E (PARIS - FRANCE)", "PAPEETE (POLYNÉSIE FRANÇAISE - FRANCE)"),
    mandate = c("17/06/2012 - ?", "17/06/2012 - ?"),
    constituency = c("Français établis hors de France", "Polynésie française"),
    party = c("Union des démocrates et indépendants", "Union des démocrates et indépendants"),
    url_an = c("http://www.assemblee-nationale.fr/14/tribun/fiches_id/695100.asp", NA),
    photo = c(NA, "photos_an/10617.jpg"),
    fullname = c("Meyer HABIB", "Édouard FRITCH"),
    legislature = c(14, 14)
  ))

  # mandate years
  dep$nyears = sapply(dep$mandate[ !is.na(dep$mandate) ], function(x) {
    x = as.numeric(unlist(str_extract_all(x, "[0-9]{4}")))
    if (length(x) == 1)
      x = c(x, 2014)
    paste0(seq(min(x), max(x)), collapse = ";")
  })
    
  # mandate years, generalised to all legislatures
  for (i in unique(dep$url)) {
    x = dep$nyears[ dep$url == i ]
    x = as.numeric(unlist(str_split(x, ";")))
    dep$nyears[ dep$url == i ] = paste0(sort(x), collapse = ";")
  }
  
  # years in office before start of legislature
  for (i in 1:nrow(dep)) {
    x = dep$nyears[ i ]
    x = as.numeric(unlist(strsplit(x, ";")))
    dep$nyears[ i ] = sum(x < legs[ as.character(dep$legislature[ i ]) ])
  }
    
  # subset to legislatures under examination
  dep = subset(dep, legislature %in% sessions)
  
  # fix extra content in party groups
  dep$party = gsub("(\\s|-)+([R|r]éélu|Invalidé|\\d+)(.*)", "", dep$party)
  
  # fix party group of Bernard Gérard
  # http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=17212
  dep$party[ dep$name == "BERNARD GERARD" ] = "Union pour un mouvement populaire"
    
  # assign cross-legislature parliamentary groups
  # note: two groups also look at the constituency column due to a few inversions in the parsed data
  y = rep(NA, nrow(dep))
  
  # SE
  y[ grepl("Anciens départements d'Algérie|Unité de la République", dep$party) ] = "SE"
  y[ grepl("aucun groupe|non inscrits|Non( |-)inscrit", dep$party) ] = "SE"
  
  # RAD, PG ~ 4/4.1
  y[ grepl("République et liberté", dep$party) ] = "RL" # AN 1993-97; centre-gauche (MRG, MDC, MR)
  # RAD, gauche plurielle AN 1997-02, mostly PRG (4.1) + MDC (1.2) + Verts (3.2) + DVG
  y[ grepl("Radical, citoyen et verts", dep$party) ] = "RCV"
  # RAD, RRDP
  y[ grepl("Radical, républicain, démocrate et progressiste", dep$party) ] = "RRDP"

  y[ y %in% c("RL", "RCV", "RRDP") ] = "RAD"
  
  # CEN - droite non gaulliste
  y[ grepl("Progrès et démocratie moderne", dep$party) ] = "PDM" # CNIP + CD (5.7) + CDP (5.9) + CR, l. 3-4
  y[ grepl("Indépendants et paysans d'action sociale", dep$party) ] = "CNIP" # PG = 7.6
  y[ grepl("[C|c]entre démocratique", dep$party) ] = "CD" # Lecanuet, PG = 5.7
  # CEN - UDF
  y[ grepl("Réformateurs démocrates sociaux", dep$party) ] = "RDS" # MR, 1973-74, fusion UC 74, fusion RI 78 = UDF
  y[ grepl("Union (centriste|du centre)", dep$party) ] = "UC"
  y[ grepl("Républicains indépendants", dep$party) | grepl("Républicains indépendants", dep$constituency) ] = "RI" # 7.1
  y[ grepl("Union pour la démocratie française", dep$party) ] = "UDF" # PG = 6.1
  # CEN - post-UDF
  y[ grepl("Nouveau Centre", dep$party) ] = "NC" # PG = 6
  y[ grepl("Union des démocrates et indépendants", dep$party) ] = "UDI"
  
  y[ y %in% c("PDM", "CNIP", "CD", "RDS", "UC", "RI", "UDF", "NC", "UDI") ] = "CEN"
  
  # DRO - gaullistes
  y[ grepl("Union pour la nouvelle République", dep$party) ] = "UNR" # de Gaulle
  y[ grepl("Union (des démocrates pour la République|démocratique pour la V° République)", dep$party) ] = "UDR" # id.
  y[ grepl("Rassemblement pour la République", dep$party) ] = "RPR"
  y[ grepl("Rassemblement pour la République", dep$constituency) ] = "RPR"
  y[ grepl("Démocratie libérale", dep$party) | grepl("Démocratie libérale", dep$constituency) ] = "DL"
  y[ grepl("Union pour (la majorité présidentielle|un [M|m]ouvement [P|p]opulaire)", dep$party) ] = "UMP"
  y[ grepl("Union pour (la majorité présidentielle|un [M|m]ouvement [P|p]opulaire)", dep$constituency) ] = "UMP"

  y[ y %in% c("UNR", "UDR", "RPR", "DL", "UMP") ] = "DRO"

  # SOC
  y[ grepl("Entente démocratique|Rassemblement démocratique|[S|s]ocialiste", dep$party) ] = "SOC"
  y[ grepl("[S|s]ocialiste", dep$constituency) ] = "SOC"
  
  # ECOLO
  y[ grepl("Écologiste", dep$party) ] = "ECO"
  
  # FN
  y[ grepl("Front national", dep$party) ] = "FN"
  
  # COM - PCF + GDR
  y[ grepl("[C|c]ommuniste|Gauche démocrate et républicaine", dep$party) ] = "COM"
  
  # manual fixes
  y[ dep$name == "DANIELE HOFFMAN RISPAL" ] = "SOC"
  y[ dep$name == "YVON ABIVEN" ] = "SE"
  y[ dep$name == "JEAN YVES AUTEXIER" ] = "SOC"
  y[ dep$name == "ALEXANDRE LEONTIEFF" ] = "SE" # ex-RPR
  y[ dep$name == "JEAN CLAUDE CHERMANN" ] = "SE"
  y[ dep$name == "PIERRE RINGENBACH" ] = "DRO"
  
  # "Suppléant de Patrick Devedjian de 1988 à 2002 il équilibre la 
  #  circonscription par ses conseils, ses convictions et sa fibre
  #  sociale." (from his Wikipedia page, oldid 100562264, 2014)
  
  y[ dep$name == "VICTOR RINGEISEN" ] = "SE" # radical valoisien
  y[ dep$name == "BENOIT ROY" ] = "SE" 
  y[ dep$name == "FRANCIS GALIZI" ] = "CEN"
  y[ dep$name == "GUY DESESSART" ] = "SE" # election cancelled
  
  # manual fixes, 2007-2012, mostly leftwing
  y[ dep$name == "JACQUELINE FARREYROL" ] = "DRO"
  y[ dep$name == "ARNAUD ROBINET" ] = "DRO"
  y[ dep$name == "CECILE DUMOULIN" ] = "DRO"
  y[ dep$name == "ANNY POURSINOFF" ] = "SOC" # Green
  y[ dep$name == "JEAN PIERRE FOUGERAT" ] = "SOC"
  y[ dep$name == "JEAN LUC DRAPEAU" ] = "SOC"
  y[ dep$name == "JEAN CLAUDE GOUGET" ] = "SOC" # PRG
  y[ dep$name == "ANDRE ROUXEL" ] = "SOC"
  y[ dep$name == "THERESE GUILBERT" ] = "SOC"
  y[ dep$name == "EMMANUELLE AJON" ] = "SOC"
  y[ dep$name == "JOELLE BOROWSKI" ] = "SOC"
  y[ dep$name == "PATRICE FRANCOIS" ] = "SE" # Isère
  y[ dep$name == "YVON ROBERT" ] = "SOC"
  y[ dep$name == "JEROME GUEDJ" ] = "SOC"
  y[ dep$name == "REMI CHAINTRON" ] = "SOC"
  y[ dep$name == "JACQUES MOIGNARD" ] = "SOC" # in RDSE seat (wtf)
  y[ dep$name == "AUDREY MARIE" ] = "SE" # Guyane
  y[ dep$name == "JEAN MARIE BEFFARA" ] = "SOC"
  y[ dep$name == "CARLOS DA SILVA" ] = "SOC"
  y[ dep$name == "FLORENCE DELAUNAY" ] = "SOC"
  
  # solve remaining MPs from colonial territories
  y[ is.na(y) & with(dep, party == constituency) ] = "SE"
  y[ dep$name == "TETUAAPUA POUVANAA OOPA" ] = "SE"
    
  stopifnot(all(!is.na(y)))
  
  # add as sponsor variable
  dep$party = ifelse(y == "SE", "IND", y)
  
  # ============================================================================
  # CHECK CONSTITUENCIES
  # ============================================================================
  
  # clean up extra content in constituencies
  dep$constituency = gsub("(.*) : (.*)", "\\2", dep$constituency)
  dep$constituency = gsub("(.*) - (Fédération|Union|Rassemblement|Républicains|Socialiste)(.*)", "\\1",
                           dep$constituency)
  dep$constituency = str_trim(dep$constituency)

  # constituencies
  dep$constituency[ dep$constituency == "Seine-St-Denis" ] = "Seine-Saint-Denis"
  dep$constituency[ dep$constituency == "Basses-Alpes" ] = "Alpes-de-Haute-Provence"
  dep$constituency[ dep$constituency == "Côtes d'Armor" ] = "Côtes-d'Armor"
  dep$constituency[ dep$constituency == "Côtes-du-Nord" ] = "Côtes-d'Armor" # same as above
  dep$constituency[ dep$constituency == "Territoire-de-Belfort" ] = "Territoire de Belfort"
  dep$constituency = gsub("\\s", "_", dep$constituency)
  
  cat("Checking constituencies,", sum(is.na(dep$constituency)), "missing...\n")
  for (i in na.omit(unique(dep$constituency))) {
    
    g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))
    
    if (status_code(g) != 200)
      cat("Missing Wikipedia entry:", i, "\n")
    
    g = xpathSApply(htmlParse(g), "//title", xmlValue)
    g = gsub("(.*) — Wikipédia(.*)", "\\1", g)
    
    if (gsub("\\s", "_", g) != i)
      cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")
    
  }
  
  # save master sponsor dataset
  write.csv(select(dep, legislature, fullname, name, sex, born, party,
                   constituency, nyears, url, url_an, photo), data,
            row.names = FALSE)

}

dep = read.csv(data, stringsAsFactors = FALSE)

# add missing years of birth (from Sycomore or WP-FR)

dep$born[ dep$name == "ANDRE ROUXEL" ] = 1946
dep$born[ dep$name == "ANNIE GENEVARD" ] = 1956
dep$born[ dep$name == "AUDREY MARIE" ] = 1947
dep$born[ dep$name == "BENOIT ROY" ] = 1962
dep$born[ dep$name == "CARLOS DA SILVA" ] = 1974
dep$born[ dep$name == "EMMANUELLE AJON" ] = 1971
dep$born[ dep$name == "JOELLE BOROWSKI" ] = 1961
dep$born[ dep$name == "MARIE CLAUDE MARCHAND" ] = 1954
dep$born[ dep$name == "PATRICE FRANCOIS" ] = 1953
dep$born[ dep$name == "PIERRE RINGENBACH" ] = 1930
dep$born[ dep$name == "RAYMOND LANCELIN" ] = 1944
dep$born[ dep$name == "REMI CHAINTRON" ] = 1972
dep$born[ dep$name == "VICTOR RINGEISEN" ] = 1930
dep$born[ dep$name == "YVON ROBERT" ] = 1949
dep$born = as.integer(dep$born)

dep$sex[ dep$name == "VICTOR RINGEISEN" ] = "M"
dep$sex[ dep$name == "BENOIT ROY" ] = "M"
dep$sex[ dep$name == "PIERRE RINGENBACH" ] = "M"

dep$url = paste0("http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=", dep$url)

# ==============================================================================
# QUALITY CONTROL
# ==============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(dep$born)), "years of birth\n")
stopifnot(is.integer(dep$born) & nchar(dep$born) == 4 | is.na(dep$born))

cat("Missing", sum(is.na(dep$constituency)), "constituencies\n")
stopifnot(is.character(dep$constituency))

cat("Missing", sum(is.na(dep$photo)), "photos\n")
stopifnot(is.character(dep$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", dep$photo) | is.na(dep$photo))

stopifnot(!is.na(dep$sex) & dep$sex %in% c("F", "M"))
stopifnot(!is.na(dep$nyears) & is.integer(dep$nyears))
stopifnot(!is.na(dep$url) & grepl("^http(s)?://(.*)", dep$url))
stopifnot(dep$party %in% names(colors))
