sponsors = "data/sponsors-se.csv"

if(!file.exists(sponsors)) {
  
  sen = data.frame()
  for(i in c("anciens-senateurs", "senateurs")) {
    
    root = "http://www.senat.fr"
    ancien = grepl("anciens", i)
    
    file = paste0("raw_se/", i, ".html")
    
    if(!file.exists(file))
      download.file(paste0(root, "/", ifelse(ancien, 
                                             paste0(i, "-5eme-republique"), 
                                             i), "/senatl.html"),
                           file, mode = "wb", quiet = TRUE)
    
    html = htmlParse(file, encoding = "UTF-8")
    urls = unique(as.vector(xpathSApply(html, "//a[contains(@href,'/senateur/')]/@href")))
    
    cat("Parsing", length(urls), i, "\n")
    s = data.frame()
    for(x in urls) {
      
      link = paste0(root, x)
      file = paste0("raw_se", x)
      
      if(!file.exists(file))
        try(download.file(link, file, mode = "wb", quiet = TRUE))
            
      if(!file.info(file)$size) {
        
        cat("Failed to download senator:", x, "\n")
        file.remove(file)
        
      } else {
        
        # cat("Senator", x)
        html = htmlParse(file, encoding = "UTF-8")
        
        civ = str_clean(xpathSApply(html, "//dd", xmlValue))
        civ = civ[ grepl("Né(e)? le", civ) ]
        
        born = str_extract(civ, "[0-9]{4}")
        sex = ifelse(grepl("Née le", civ), "F", "M")
        
        nom = gsub("(Anciens sénateurs Vème République : )|( - Sénat)", "", 
                   sapply(xpathSApply(html, "//title"), xmlValue))
        
        nom = gsub("Jean Louis", "Jean-Louis", nom) # fix missing dash
        ndf = unlist(strsplit(nom, " "))
        ndf = paste0(ndf[ -length(ndf) ], collapse = " ") # family name
        pre = rev(unlist(strsplit(nom, " ")))[1] # first name
        nom = paste(pre, ndf) # reordered full name
        
        text = xpathSApply(html, "//ul[@class='list-type-03']/li", xmlValue)
        
        from = text [ grepl("Elu[e]? le|Sénat(eur|rice) le", text) ]
        from = str_extract(from[1], "[0-9]{4}") # earliest start of mandate
        
        to = gsub("(\\D+) le (.*)", "\\2", text [ grepl("(Démission|Fin de mandat) le", text) ])
        
        if(ancien & length(to))
          to = str_extract(to[1], "[0-9]{4}") # should always be unique
        else
          to = "2014"
        
        nyears = seq(as.numeric(from), as.numeric(to))
        l = unique(cut(nyears, breaks = c(legs, 2014), labels = names(legs)), include.lowest = TRUE)
        nyears = paste0(nyears, collapse = ";")
        
        circo = xpathSApply(html, "//h2[@class='subtitle-02']", xmlValue)
        circo = circo[ grepl("(S|s)énat", circo) ]
        circo = gsub("(Ancien )?(S|s)énat(eur|rice)( de | de l'| d'| des | de la | du | représentant les )", 
                     "", circo)
        circo = str_trim(gsub("\\s+", " ", circo))
        
        if(ancien) {
          
          text = gsub("\\n", " ", text)
          party = text [ grepl("(d|a)u\\s+(G|g)roupe", text) ]
          
        } else {
          
          party = xpathSApply(html, "//ul[@class='list-type-01']/li", xmlValue)
          party = gsub("\\n", " ", party)
          party = party[ grepl("(d|a)u\\s+(G|g)roupe", party) ]
          
        }
        
        s = rbind(s, data.frame(url = paste0(root, x), legislature = as.numeric(l),
                                fullname = nom, family_name = ndf,
                                born, sex, nyears, constituency = circo,
                                party = ifelse(!length(party), NA, party),
                                stringsAsFactors = FALSE))
        
      }
            
    }
    
    sen = rbind(sen, s)
  
  }
  
  # years in office before start of legislature
  for(i in 1:nrow(sen)) {
    x = sen$nyears[ i ]
    x = as.numeric(unlist(strsplit(x, ";")))
    sen$nyears[ i ] = sum(x < legs[ as.character(sen$legislature[ i ]) ])
  }
  
  sen = subset(sen, legislature %in% sessions)
    
  # download photos
  sen$photo = 0
  
  for(i in unique(sen$url)) {
    
    j = gsub("html$", "jpg", gsub("http://www.senat.fr/senateur/", "", i))
    photo = paste0("photos_se/", j)
    
    if(!file.exists(photo))
      try(download.file(paste0("http://www.senat.fr/senimg/", j),
                        photo, mode = "wb", quiet = TRUE), silent = TRUE)
    
    if(!file.info(photo)$size)
      file.remove(photo)
    
    if(file.exists(photo))
      sen$photo[ sen$url == i ] = 1

  }
  
  sen$legislature = as.numeric(sen$legislature)
  sen$url = gsub("http://www.senat.fr/senateur/|\\.html$", "", sen$url)
  
  # clean names
  sen$name = clean_names(sen$fullname)
  sen$family_name = clean_names(sen$family_name)
  
  # fix homonym
  sen[ with(sen, grepl("JEAN BOYER", name) & constituency == "Isère"), "name"] = "JEAN BOYER ISERE"
  
  # fix constituency
  sen[ with(sen, grepl("JEAN PIERRE BEL", name) & constituency != "Ariège Midi-Pyrénées"), "constituency"] = "Ariège Midi-Pyrénées"

  d = summarise(group_by(sen, name), n = n_distinct(url))
  u = d$n > 1
  if(sum(u)) {
    warning(paste(sum(u), "homonyms"))
    print(sen[ sen$name %in% d$name[ u ], ])
  }
  
  # assign cross-legislature parliamentary groups
  # source: groups from http://senat.fr/anciens-senateurs-5eme-republique/
  y = rep(NA, nrow(sen))
  y[ grepl("Rassemblement Démocratique et Européen", sen$party) ] = "RDE"
  y[ grepl("Rassemblement Démocratique et Social Européen", sen$party) ] = "RDSE"
  y[ grepl("Républicains et( des)? Indépendants", sen$party) ] = "RI"
  y[ grepl("Républicains Indépendants d'Action Sociale", sen$party) ] = "RIAS"
  y[ grepl("Union Centriste des Démocrates de Progrès", sen$party) ] = "UCDP"
  y[ grepl("Union Centriste et Républicaine", sen$party) ] = "UCR"
  y[ grepl("Union (C|c)entriste", sen$party) ] = "UC-UDF"
  y[ grepl("Union des Démocrates pour la République", sen$party) ] = "UDR"
  y[ grepl("Union des( )?Démocrates et Indépendants", sen$party) ] = "UDI"
  y[ grepl("Rassemblement pour la République", sen$party) ] = "RPR"
  y[ grepl("Centre Républicain d'Action Rurale et Sociale", sen$party) ] = "CRARS"
  y[ grepl("Union pour la Nouvelle République", sen$party) ] = "UNR"
  y[ grepl("Union pour un( )?Mouvement Populaire", sen$party) ] = "UMP"
  y[ grepl("Communiste", sen$party) ] = "C"
  y[ grepl("[C|c]ommuniste [R|r]épublicain et [C|c]itoyen", sen$party) ] = "CRC"
  y[ grepl("Centre National des Indépendants et Paysans", sen$party) ] = "CNIP"
  y[ grepl("Centre Démocratique", sen$party) ] = "CD"
  y[ grepl("Gauche Démocratique", sen$party) ] = "GD"
  y[ grepl("Républicain[s]? Populaire[s]?", sen$party) ] = "MRP"
  y[ grepl("écologiste", sen$party) ] = "ECO"
  y[ grepl("[S|s]ocialiste", sen$party) ] = "SOC" # + apparentés
  y[ grepl("Partide Gauche", sen$party) ] = "PG" # hors liste
  y[ is.na(y) ] = "SE" # residuals
  
  stopifnot(all(!is.na(y)))
  
  # generic party group names
  y[ y %in% c("C", "PG", "CRC") ] = "COM"
  y[ y %in% c("GD", "RDE", "RDSE") ] = "RAD"
  y[ y %in% c("UC-UDF", "UDI", "CD", "UCR", "UCDP", "CNIP", "CRARS", "RI", "RIAS") ] = "CEN"
  y[ y %in% c("UNR", "UDR", "MRP", "UMP", "RPR") ] = "DRO"
  y[ y %in% c("NI", "SE") ] = "IND"
  
  sen$party = y
  table(sen$party, exclude = NULL)
  
  # clean up extra content in constituencies
  sen$constituency = gsub("\\(|\\)|&nbsp|puis| du | de la | des |Ancien sénateur|indéterminé", 
                   "", sen$constituency)
  
  # geocode constituencies
  geo = "data/geocodes-se.csv"
  
  if(!file.exists(geo))
    write.csv(parse_geo(sen$constituency), geo, row.names = FALSE)
  
  sen = merge(sen, read.csv(geo, stringsAsFactors = FALSE),
                    by = "constituency", all.x = TRUE)
  
  # remove regions from constituencies
  sen$constituency = gsub("\\s+(Rhône-Alpes|Picardie|Auvergne|Provence-Alpes-Côte d'Azur|Champagne-Ardenne|Languedoc-Roussillon|Midi-Pyrénées|Alsace|Basse-Normandie|Poitou-Charentes|Centre|Limousin|Haute-Corse|Bourgogne|Bretagne|Côtes-d'Armor|Aquitaine|Franche-Comté|Ile-de-France|Haute-Normandie|Guadeloupe|Guyane|Mayotte|Martinique|Iles Wallis et Futuna|Doubs|PaysLoire|Alsace|Lorraine|Nouvelle-Calédonie|Nord-Pas-de-Calais|Polynésie française|Saint-Barthélemy|Saint-Pierre-et-Miquelon|Hauts-de-Seine|de Paris|Val-de-Marne|Seine-Maritime|La Réunion|Corse)", "", 
                          sen$constituency)
  sen$constituency[ sen$constituency == "Corse" ] = "Haute-Corse"
  sen$constituency[ sen$constituency == "Iles Wallis et Futuna" ] = "Wallis et Futuna"
  sen$constituency[ sen$constituency == "Côtes-du-Nord" ] = "Côtes-d'Armor"
  sen$constituency[ sen$constituency == "Alpes de Haute-Provence" ] = "Alpes-de-Haute-Provence"
  sen$constituency[ sen$constituency == "Seine-Inférieure" ] = "Seine-Maritime"
  sen$constituency = gsub("\\s", "_", sen$constituency)
  
  j = c("legislature", "fullname", "name", "family_name", "sex", "born",
        "party", "constituency", "nyears", "lon", "lat", "url", "photo")

  # save master sponsor dataset
  write.csv(sen[, j], sponsors, row.names = FALSE)
  
}

s = read.csv(sponsors, stringsAsFactors = FALSE)
