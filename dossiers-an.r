root = "http://www.assemblee-nationale.fr"
bills = "data/bills-an.csv"

if (!file.exists(bills)) {
  
  doc = data_frame()

  for (x in sessions) {
    
    cat("Parsing index", sprintf("%2.0f", x))
    
    file = paste0("raw_an/bills-", x, ".html")
    
    if (!file.exists(file))
      download.file(paste0(root, "/", x, "/documents/index-proposition.asp"),
                    file, mode = "wb", quiet = TRUE)
    
    # parse
    h = htmlParse(file, encoding = "UTF-8")
    h = xpathSApply(h, "//a[contains(@href, '/dossiers/')]/@href")
    
    data = data_frame(legislature = x, url = gsub("\\.asp(.*)", ".asp", h))
    data = unique(data)
    
    # all bills come from the right legislature
    data = subset(data, legislature == gsub("/(\\d+)/(.*)", "\\1", url))
    
    # no duplicates in the bill URLs
    stopifnot(!length(data$url[ duplicated(data$url) ]))
    
    cat(":", nrow(data), "bills\n")
    
    data$title = NA
    data$au = data$au_url = NA
    data$co = data$co_url = NA
    
    # download files
    for (i in rev(data$url)) {
      
      file = paste0("raw_an/bills/",
                    gsub("/(\\d+)/(.*)", "\\1", i), "-", # session
                    gsub("\\.asp$", ".html", str_extract(i, "(\\w|-)+\\.asp")))
      
      if (!file.exists(file))
        try(download.file(paste0(root, i), file, mode = "wb", quiet = TRUE), silent = TRUE)
      
      if (!file.exists(file)) {
        
        cat(sprintf("%4.0f", which(data$url == i)), ": no bill at", paste0(root, i), "\n")

      } else if (file.exists(file) && !file.info(file)$size) {
        
        cat(sprintf("%4.0f", which(data$url == i)), ": empty page at", paste0(root, i), "\n")
        file.remove(file)
        
      } else {
        
        h = htmlParse(file, encoding = "UTF-8")
        
        r = str_clean(xpathSApply(h, "//div[contains(@align, 'left')][2]", xmlValue))
        r = str_extract(r, "(.*?), déposé[e]{0,1}\\s+le\\s+(\\d+|er)+\\s+(\\w+)\\s+(\\d+)")
        data$title[ data$url == i ] = ifelse(!length(r), NA, r)
        
        au = xpathSApply(h, "//div[contains(@align, 'left')][2]/a[contains(@href, 'tribun') or contains(@href, 'senateur') or contains(@href, 'senfic')]", xmlValue)
        au_url = xpathSApply(h, "//div[contains(@align, 'left')][2]/a[contains(@href, 'tribun') or contains(@href, 'senateur') or contains(@href, 'senfic')]/@href")
        
        if (!length(r) | !length(au)) {
          
          cat(file, ": no sponsor information\n")
          next
          
        }
        
        au = gsub("(\\(|\\))", "\\\\\\1", au) # protect brackets in regex
        aa = unlist(str_extract_all(r, paste0(au, collapse = "|"))) # select sponsors
        au_url = au_url[ au %in% aa ]
        au = au[ au %in% aa ]
        
        data$au[ data$url == i ] = paste0(au, collapse = ";")
        data$au_url[ data$url == i ] = paste0(au_url, collapse = ";")
        
        co = xpathSApply(h, "//div[contains(@align, 'left')][2]/a[contains(@href, 'cosignataires')]/@href")
        if (length(co)) {
          
          co = gsub("javascript:ouvre_popup\\('(.*)'\\)", "-\\1", co)
          file = gsub("\\.html", co, file)
          
          if (!file.exists(file))
            try(download.file(paste0(root, gsub("/(\\d+)/(.*)", "/\\1/", i), gsub("-", "dossiers/", co)),
                              file, mode = "wb", quiet = TRUE), silent = TRUE)
          
          if (!file.info(file)$size) {
            
            cat(sprintf("%4.0f", which(data$url == i)), ": no cosponsor(s) at", paste0(root, i), "\n")        
            file.remove(file)
            
          } else {
            
            h = htmlParse(file)
            
            co = xpathSApply(h, "//a", xmlValue)
            co_url = xpathSApply(h, "//a/@href")
            
            stopifnot(length(co) == length(co_url))
            sig = !grepl("membres", co) & grepl("tribun|senat", co_url)
            
            data$co[ data$url == i ] = paste0(co[ sig ], collapse = ";")
            data$co_url[ data$url == i ] = paste0(co_url[ sig ], collapse = ";")
            
          }
          
        }
        
      }
      
    }
    
    doc = rbind(doc, data)
    
  }
  
  # subset bills
  
  doc = subset(doc, !is.na(title)) # lose 328 bills with no sponsor listings
  doc = subset(doc, grepl("Proposition de loi", title)) # lose one gov. bill
  
  doc = subset(doc, !is.na(au)) # lose 3 bills
  doc = subset(doc, au != "") # lose 33 empty
  
  write.csv(doc, bills, row.names = FALSE)
  
  # diagnose missing sponsors
  
  a = data_frame(name = unlist(strsplit(doc$au, ";")),
                 url = unlist(strsplit(doc$au_url, ";")))
  a = rbind(a, data_frame(name = unlist(strsplit(doc$co, ";")),
                          url = unlist(strsplit(doc$co_url, ";"))))
  a = subset(a, !is.na(name))
  a$name = gsub("(M\\.|Mme)\\s", "", a$name)
  a$name = gsub("(.*)\\((.*)", "\\1", a$name)
  a$name = str_trim(a$name)
  a$name = clean_names(a$name)
  a$found = a$name %in% unique(dep$name)
  t = table(a$name[ !a$found & grepl("tribun", a$url) ])
  cat(length(t), "missing sponsors\n") # a few recent MPs from by-elections, plus one from l. 7
    
}

doc = read.csv(bills, stringsAsFactors = FALSE)

# count sponsors

doc$n_au = 1 + str_count(doc$au, ";")
doc$n_co = 1 + str_count(doc$co, ";")

doc$co[ doc$co == "" ] = NA
doc$n_co[ is.na(doc$co) ] = 0

doc$n_co_url = 1 + str_count(doc$co_url, ";")
doc$co_url[ doc$co_url == "" ] = NA
doc$n_co_url[ is.na(doc$co_url) ] = 0

doc$n_a = doc$n_au + doc$n_co

# preprocessed sponsorships
table(doc$n_a > 1, doc$legislature)

cat("Finding sponsors for", nrow(doc), "bills\n")
doc$sponsors = NA
for (i in nrow(doc):1) {
  ##cat(i)
  n = unlist(strsplit(doc$au[i], ";"))
  u = unlist(strsplit(doc$au_url[i], ";"))
  if (doc$n_co[i] > 0) {
    n = c(n, unlist(strsplit(doc$co[i], ";")))
    u = c(u, unlist(strsplit(doc$co_url[i], ";")))
  }
  if (!grepl("tribun", u[1])) {
    ##cat(": senator bill\n")
    doc$sponsors[i] = NA
  } else {
    n = n[ grepl("tribun", u) ]
    n = gsub("(M\\.|Mme)\\s", "", gsub("(.*)\\((.*)", "\\1", n))
    n = str_trim(clean_names(n))
    n = n[ n %in% unique(dep$name) ]
    ##cat(":", doc$n_a[i], "sponsors", length(n), "recognised\n")
    doc$sponsors[i] = paste0(n, collapse = ";")
  }
}

# 646 bills first-authored by senators (7%), 236 of which cosponsored (2%)
table(doc$n_a[ is.na(doc$sponsors) ] > 1)

doc$n_s = 1 + str_count(doc$sponsors, ";")
doc$n_s[ is.na(doc$sponsors) ] = NA

# randomly distributed differences in bill counts
table(doc$n_a > 1, doc$legislature, exclude = NULL)
table(doc$n_s > 1, doc$legislature, exclude = NULL)

# lose senator-authored bills and buggy legislature
doc = subset(doc, !is.na(sponsors) & legislature != 10)
doc$date = str_sub(doc$title, start = -4)

# postprocessed sponsorships
doc = doc[, c("legislature", "date", "sponsors") ]
doc$n_au = 1 + str_count(doc$sponsors, ";")
