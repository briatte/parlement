sponsors = "data/sponsors-se.csv"

if(!file.exists(sponsors)) {
  
  sen = data_frame()
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
    s = data_frame()
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
        
        nom = gsub("(.*)M(lle|me|\\.) (.*),(.*)", "\\3",
                   xpathSApply(html, "//meta[@name='Description']/@content"))

        text = c(xpathSApply(html, "//ul[@class='list-type-01']/li", xmlValue),
                 xpathSApply(html, "//ul[@class='list-type-03']/li", xmlValue))
        
        text = str_clean(text)
        
        from = text[ grepl("Elu[e]? le|Sénat(eur|rice) le", text) ]
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
        
        party = text[ grepl("(Membre du|Président(e)? (délégué(e)? )?du|au)\\s+(G|g)roupe\\s|aucun groupe", 
                            text) ]
        
        # assign cross-legislature parliamentary groups
        # source: groups from http://senat.fr/anciens-senateurs-5eme-republique/
        party[ grepl("Rassemblement Démocratique et Européen", party) ] = "RDE"
        party[ grepl("Rassemblement Démocratique et Social Européen", party) ] = "RDSE"
        party[ grepl("Républicains et( des)? Indépendants", party) ] = "RI"
        party[ grepl("Républicains Indépendants d'Action Sociale", party) ] = "RIAS"
        party[ grepl("Union Centriste des Démocrates de Progrès", party) ] = "UCDP"
        party[ grepl("Union Centriste et Républicaine", party) ] = "UCR"
        party[ grepl("Union (C|c)entriste", party) ] = "UC-UDF"
        party[ grepl("Union des Démocrates pour la République", party) ] = "UDR"
        party[ grepl("Union des( )?Démocrates et Indépendants", party) ] = "UDI"
        party[ grepl("Rassemblement pour la République", party) ] = "RPR"
        party[ grepl("Centre Républicain d'Action Rurale et Sociale", party) ] = "CRARS"
        party[ grepl("Union pour la Nouvelle République", party) ] = "UNR"
        party[ grepl("Union pour un( )?Mouvement Populaire|Les Républicains", party) ] = "UMP"
        party[ grepl("Communiste", party) ] = "C"
        party[ grepl("[C|c]ommuniste [R|r]épublicain et [C|c]itoyen", party) ] = "CRC"
        party[ grepl("Centre National des Indépendants et Paysans", party) ] = "CNIP"
        party[ grepl("Centre Démocratique", party) ] = "CD"
        party[ grepl("Gauche Démocratique", party) ] = "GD"
        party[ grepl("Républicain[s]? Populaire[s]?", party) ] = "MRP"
        party[ grepl("écologiste", party) ] = "ECO"
        party[ grepl("[S|s]ocialiste", party) ] = "SOC" # + apparentés
        party[ grepl("Partide Gauche", party) ] = "PG" # hors liste
        party[ grepl("aucun groupe", party) ] = "NI" # non inscrits
        
        # generic partparty group names
        party[ party %in% c("C", "PG", "CRC") ] = "COM"
        party[ party %in% c("GD", "RDE", "RDSE") ] = "RAD"
        party[ party %in% c("UC-UDF", "UDI", "CD", "UCR", "UCDP", "CNIP", "CRARS", "RI", "RIAS") ] = "CEN"
        party[ party %in% c("UNR", "UDR", "MRP", "UMP", "RPR") ] = "DRO"
        party[ party == "NI" ] = "IND"
        
        party = unique(party[ nchar(party) == 3 ])
        
        # Henri de Raincourt: legislatures 7-14, CEN before 2002, DRO afterwards (fix below)
        if(x == "/senateur/de_raincourt_henri86044g.html")
          party = "DRO"
        
        stopifnot(length(party) == 1)
        
        s = rbind(s, data.frame(
          url = paste0(root, x), legislature = as.numeric(l),
          fullname = as.character(nom),
          born, sex, nyears, constituency = circo,
          party = party,
          stringsAsFactors = FALSE))
        
        # cat(tail(s, 1)$fullname, "\n")
        
      }
            
    }
    
    sen = rbind(sen, s)
  
  }
  
  # Henri de Raincourt: legislatures 7-14, CEN before 2002, DRO afterwards (final fix)
  sen$party[ grepl("de_raincourt_henri86044g", sen$url) & sen$legislature < 12 ] = "CEN"
  
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
  
  # picture is not a JPG portrait
  sen$photo[ sen$url == "roger_coupin_maryse07002g" ] = 0
  
  # clean names
  sen$name = clean_names(sen$fullname)

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
  
  # clean up extra content in constituencies
  sen$constituency = gsub("\\(|\\)|&nbsp|puis| du | de la | des |Ancien sénateur|indéterminé", 
                   "", sen$constituency)
  
  # geocode constituencies
  geo = "data/geocodes-se.csv"
  
  ## uncomment to geocode the constituencies (requires ggmap)
  ## if(!file.exists(geo))
  ##   write.csv(parse_geo(sen$constituency), geo, row.names = FALSE)
  
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
  
  j = c("legislature", "fullname", "name", "sex", "born",
        "party", "constituency", "nyears", "lon", "lat", "url", "photo")

  # save master sponsor dataset
  write.csv(sen[, j], sponsors, row.names = FALSE)
  
}

s = read.csv(sponsors, stringsAsFactors = FALSE)
