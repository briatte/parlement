data = "data/sponsors-an.csv"

if(!file.exists(data)) {
  
  root = "http://www.assemblee-nationale.fr/sycomore/"
  
  sycomore = c()
  for(x in sessions) { # accepts 1:14
    
    file = paste0("raw_an/mps-", x, ".html")
    
    if(!file.exists(file))
      try(download(paste0(root, "result.asp?radio_dept=tous_departements&regle_nom=contient&Nom=&departement=&choixdate=intervalle&D%C3%A9butMin=&FinMin=&Dateau=&legislature=", x, "&choixordre=chrono&Rechercher=Lancer+la+recherche"),
                   file, mode = "wb", quiet = TRUE), silent = TRUE)
    
    s = htmlParse(file, encoding = "UTF-8")
    s = unlist(xpathSApply(s, "//a[contains(@href, 'num_dept')]/@href"))
   
    cat("Parsing legislature", sprintf("%2.0f", x), ":", length(s), "MPs\n")
    sycomore = c(sycomore, s)
    
  }
  
  sycomore = paste0(root, unique(sycomore))
  cat("Parsing", length(sycomore), "MPs\n")
  
  # MP-level details
  dep = data.frame()
  for(i in rev(sycomore)) {
    
    file = paste0("raw_an/mps/", gsub("\\D", "", i), ".html")
    
    if(!file.exists(file))
      download.file(i, file, mode = "wb", quiet = TRUE)
    
    html = try(htmlParse(file, encoding = "UTF-8"), silent = TRUE)
    
    if("try-error" %in% class(html)) {
      
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
      
      if(!length(mandate)) {
        constituency = party = NA
      } else {
        constituency = str_trim(gsub("(.*) : (.*) - (.*)", "\\2", mandate))
        party = str_trim(gsub("(.*) : (.*) - (.*)", "\\3", mandate))
      }
      
      # vector of individual mandates
      mandate = xpathSApply(html, "//div[@id='assemblee']/*/ul/li/div[@class='article-content']/p/b", xmlValue)
      mandate = str_trim(mandate[ !grepl("Présidence", mandate) ])
      
      if(!length(mandate))
        mandate = NA
      
      if(identical(c(mandate, party), c(NA, NA)))
        cat(sprintf("%4.0f", which(sycomore == i)), "[ no details at", i, "]\n")
      # else
      # cat(sprintf("%4.0f", which(sycomore == i)), nom_de_famille, "\n")
      
      # match url_an from other datasets
      url_an = xpathSApply(html, "//a[contains(@onclick, 'tribun')]/@onclick")
      url_an = as.vector(gsub("window.location=|'", "", url_an))
      url_an = ifelse(!length(url_an), NA, paste0("http://www.assemblee-nationale.fr", url_an))
      
      dep = rbind(dep, data.frame(url = gsub("\\D", "", i), name,
                                  sex, born, born_loc,
                                  mandate, constituency, party, url_an,
                                  stringsAsFactors = FALSE))
      
    }
    
  }
  
  cat("Parsing", n_distinct(dep$url), "photos\n")
  
  # download photos
  dep$photo = 0
  
  for(i in unique(dep$url)) {
    
    photo = paste0("photos_an/", i, ".jpg")
    
    if(!file.exists(photo))
      try(download.file(paste0("http://www.assemblee-nationale.fr/sycomore/biographies/photo/jpg/", i, ".jpg"),
                        photo, mode = "wb", quiet = TRUE), silent = TRUE)
    
    if(!file.info(photo)$size)
      file.remove(photo)
    
    if(file.exists(photo))
      dep$photo[ dep$url == i ] = 1
    
  }
  
  # save original name
  dep$fullname = dep$name
  
  # extract family name
  dep$family_name = sapply(dep$name, function(x) {
    paste0(unlist(str_extract_all(x, "[A-ZÉÀÙÊÎÔÈÙ]{2,}")), collapse = " ")
  })
  
  # simplify
  dep$name = str_trim(clean_names(dep$name))
  dep$family_name = str_trim(clean_names(dep$family_name))
  
  # fix cedilla encoding issues
  dep$family_name[ dep$name == "MICHEL FRANCAIX" ] = "FRANCAIX" 
  dep$family_name[ dep$name == "PHILIPPE BOENNEC" ] = "BOENNEC" 
  
  # fix family names
  dep$name[ dep$name == "ODETTE EUGENIE GERMAINE DURIEZ NEE GAMELIN" ] = "ODETTE DURIEZ"
  dep$family_name[ dep$name == "ODETTE DURIEZ" ] = "DURIEZ"
  dep$name[ dep$name == "CHRISTIANE TAUBIRA DELANNON" ] = "CHRISTIANE TAUBIRA"
  dep$family_name[ dep$name == "CHRISTIANE TAUBIRA" ] = "TAUBIRA"
  dep$name[ dep$name == "MARTINE AURILLAC NEE ADRIAN" ] = "MARTINE AURILLAC"
  dep$family_name[ dep$name == "MARTINE AURILLAC" ] = "AURILLAC"
  dep$name[ dep$name == "FRANCOISE BRANGET NEE MINELLO" ] = "FRANCOISE BRANGET"
  dep$family_name[ dep$name == "FRANCOISE BRANGET" ] = "BRANGET"
  dep$name[ dep$name == "FRANCOISE VALLET NEE JOUANNE" ] = "FRANCOISE VALLET"
  dep$family_name[ dep$name == "FRANCOISE VALLET" ] = "VALLET"
  dep$name[ dep$name == "BERNADETTE PAIX NEE ABRIBAT" ] = "BERNADETTE PAIX"
  dep$family_name[ dep$name == "BERNADETTE PAIX" ] = "PAIX"
  dep$name[ dep$name == "MARCELLE MAURICETTE RAMONET NEE CHITRE" ] = "MARCELLE RAMONET"
  dep$family_name[ dep$name == "MARCELLE RAMONET" ] = "RAMONET"
  dep$name[ dep$name == "COLETTE MARIE FRANCOISE MOAL NEE AMICEL" ] = "COLETTE MOAL"
  dep$family_name[ dep$name == "COLETTE MOAL" ] = "MOAL"
  dep$name[ dep$name == "PAULETTE GUINCHARD KUNTZLER" ] = "PAULETTE GUINCHARD"
  dep$family_name[ dep$name == "PAULETTE GUINCHARD" ] = "GUINCHARD"
  dep$name[ dep$name == "JOSIANE BOYCE NEE PONTIEUX" ] = "JOSIANE BOYCE"
  dep$family_name[ dep$name == "JOSIANE BOYCE" ] = "BOYCE"
  
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
  if(sum(u)) {
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
  dep = rbind(dep, data.frame(
    url = c("18563", "10617"),
    name = c("MEYER HABIB", "EDOUARD FRITCH"),
    sex = c("M", "M"),
    born = c("1961", "1952"),
    born_loc = c("PARIS 12E (PARIS - FRANCE)", "PAPEETE (POLYNÉSIE FRANÇAISE - FRANCE)"),
    mandate = c("17/06/2012 - ?", "17/06/2012 - ?"),
    constituency = c("Français établis hors de France", "Polynésie française"),
    party = c("Union des démocrates et indépendants", "Union des démocrates et indépendants"),
    url_an = c("http://www.assemblee-nationale.fr/14/tribun/fiches_id/695100.asp", NA),
    photo = c(0, 1),
    fullname = c("Meyer HABIB", "Édouard FRITCH"),
    family_name = c("HABIB", "FRITCH"),
    legislature = c(14, 14)
  ))

  # mandate years
  dep$nyears = sapply(dep$mandate[ !is.na(dep$mandate) ], function(x) {
    x = as.numeric(unlist(str_extract_all(x, "[0-9]{4}")))
    if(length(x) == 1)
      x = c(x, 2014)
    paste0(seq(min(x), max(x)), collapse = ";")
  })
    
  # mandate years, generalised to all legislatures
  for(i in unique(dep$url)) {
    x = dep$nyears[ dep$url == i ]
    x = as.numeric(unlist(str_split(x, ";")))
    dep$nyears[ dep$url == i ] = paste0(sort(x), collapse = ";")
  }
  
  # years in office before start of legislature
  for(i in 1:nrow(dep)) {
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
    
  # clean up extra content in constituencies
  dep$constituency = gsub("(.*) : (.*)", "\\2", dep$constituency)
  dep$constituency = gsub("(.*) - (Fédération|Union|Rassemblement|Républicains|Socialiste)(.*)", "\\1",
                           dep$constituency)
  dep$constituency = str_trim(dep$constituency)
  
  # geocode constituencies
  geo = "data/geocodes-an.csv"
  
  if(!file.exists(geo))
    write.csv(parse_geo(dep$constituency), geo, row.names = FALSE)
    
  dep = merge(dep, read.csv(geo, stringsAsFactors = FALSE),
                  by = "constituency", all.x = TRUE)
  
  print(table(dep$constituency[ is.na(dep$lon) ], exclude = NULL))
    
  j = c("legislature", "fullname", "name", "family_name", "sex", "born",
        "party", "constituency", "nyears", "lon", "lat", "url", "photo")
  
  # save master sponsor dataset
  write.csv(dep[, j], data, row.names = FALSE)

}

dep = read.csv(data, stringsAsFactors = FALSE)
