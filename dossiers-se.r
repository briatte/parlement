# set generic SQL settings
sql = c(user = "opendata",
        password = "",
        host = "localhost",
        port = 5432, drv = "PostgreSQL") # CREATE USER opendata;
sql = as.list(c(dbname = "dosleg", sql)) # CREATE DATABASE dosleg;

file = "data/bi_se.rda"
bills = "data/bills-se.csv"

if(!file.exists(bills)) {
  
  # URL match variable
  s$autmat = toupper(str_extract(s$url, "[0-9]+(.*)"))
  
  # sponsor UID variable
  sen = get_sql("select autcod, autfct, nomuse, prenom, autmat from auteur", sql)
  sen$nom = clean_names(paste(sen$prenom, sen$nomuse))
  
  # process problematic names
  sen$nom[sen$nom == "LOUIS FERDINAND ROCCA SERRA" ] = "FERDINAND ROCCA SERRA LOUIS"
  sen$nom[sen$nom == "BERNARD MICHEL HUGO" ] = "MICHEL HUGO BERNARD"
  sen$nom[sen$nom == "SOSEFO MAKAPE PAPILIO" ] = "MAKAPE PAPILIO SOSEFO"
  
  sen$autmat[sen$nom == "JACKY DARNE"] = "nnnn" # not a senator
  sen$autmat = toupper(sen$autmat)
  
  match = with(sen, nom %in% unique(s$name))
  group = with(sen, is.na(prenom) | nchar(prenom) < 3 | prenom == "groupe")
  
  missing = sen$nom[ !match & sen$autmat %in% unique(s$autmat) ]
  if(length(missing))
    cat("Missing:", paste0(missing, collapse = ", "))
  
  sen = sen[ !group & match, ]
  
  sp = merge(s, sen[, c("autcod", "autmat", "autfct", "nom") ],
             by = "autmat", all.x = TRUE)
  
  nul = is.na(sp$autcod)
  if(sum(nul)) {
    
    # unrecognized authors
    dnk = grepl("s(é|e)nateur", sp$autfct[ nul ], ignore.case = TRUE)
    
    # warning if author status is senator
    if(any(dnk))
      cat("Dropping:", sum(dnk), "senators (no sponsorships)\n")
    
    # drop senators for very short periods
    cat("Dropping:", n_distinct(sp$name[ nul ][ !dnk ]),
        "sponsors (not senators)\n")
    
    sp = sp[ !is.na(sp$autcod), ]
    
  }
  
  s = get_sql("select texcod, autcod, ecrnumtri from ecr where typedoc = 'T'", sql)
  
  s = merge(s[, c("texcod", "autcod", "ecrnumtri") ],
            unique(sp[, c("autcod", "autfct", "name", "party", "url") ]),
            by = "autcod", all.x = TRUE)
  sp$autcod = NULL        
  
  nul = is.na(s$name) | is.na(s$autcod)
  if(sum(nul)) {
    
    # unrecognized authors
    dnk = grepl("s(é|e)nateur", s$autfct[ nul ], ignore.case = TRUE)
    
    # warning if author status is senator
    if(any(dnk))
      cat("Dropping:", sum(dnk), "unmatched senator(s)\n")
    
    cat("Dropping:", n_distinct(s$autcod[ nul ][ !dnk ]), "unmatched sponsor(s)\n")
    
    cat("Dropping:", nrow(s[ nul, ]), "unmatched sponsorship(s)",
        round(100 * nrow(s[ nul, ]) / nrow(s), 2), "% of total\n")
    
    s = s[ !nul, ]
    
  }
  
  # sample only bills where a senator is first author
  sample = unique(with(s, texcod[ ecrnumtri < 2 ]))
  sample = s$texcod %in% sample
  sample = sample & !any(with(s, is.na(name), is.na(ecrnumtri)))
  
  # out-of-sample
  if(any(!sample)) {
    
    cat("Dropping:", nrow(s[ !sample, ]), "bills (not by a senator)\n")
    
    s = s[ sample, ]
    
  }
  
  s$status = ifelse(s$ecrnumtri == 1, "author", "cosponsor")
  names(s)[ names(s) == "texcod" ] = "uid"
  names(s)[ names(s) == "url" ] = "sponsors"
  
  # select Senator bills
  b = get_sql("select texcod, sesann, txtoritxtdat, texnum, texurl from texte where sesann > 1985 and typtxtcod = '1'", sql)
  
  b = subset(b, texcod %in% unique(s$uid))
  
  cat("Parsing:", n_distinct(b$texcod), "bills")
  
  names(b)[ names(b) == "texnum" ] = "texte"
  names(b)[ names(b) == "texcod" ] = "uid"
  names(b)[ names(b) == "texurl" ] = "url"
  
  # identifiers
  b$url = str_trim(b$url)
  b$government = 0
  
  # time of introduction
  b$date = as.Date(b$txtoritxtdat)
  
  # legislature number (based on examination date)
  b$legislature = parse_legislature(b$date, subset = sessions)
  
  b = merge(b, s[ s$ecrnumtri == 1, c("uid", "sponsors") ], by = "uid", all.x = TRUE)
  
  b = merge(b, aggregate(sponsors ~ uid, paste0, collapse = ";", data = subset(s, status == "cosponsor")),
            by = "uid", all.x = TRUE)
  
  b$sponsors = paste0(b$sponsors.x, ";", b$sponsors.y)
  b$sponsors = gsub(";NA$", "", b$sponsors)
  
  b = b[, c("legislature", "uid", "texte", "url", "date", "sponsors") ]
  b$n_au = 1 + str_count(b$sponsors, ";")
  
  table(b$n_au > 1, b$legislature, exclude = NULL)
  
  write.csv(b, bills, row.names = FALSE)
  
}

doc = read.csv(bills, stringsAsFactors = FALSE)
