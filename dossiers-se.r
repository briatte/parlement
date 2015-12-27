bills = c("auteur", "ecr", "texte")
bills = paste0("data/dosleg-", bills, ".csv")
if (any(!file.exists(bills)))
  stop(paste0("This script relies on data exported from the Dosleg database.\n",
              "Run psql.sh and see the README for detailed instructions."))

bills = "data/bills-se.csv"

if (!file.exists(bills)) {
  
  # URL match variable
  s$autmat = toupper(str_extract(s$url, "[0-9]+[a-z]?"))
  
  # load sponsor UIDs
  sen = read.csv("data/dosleg-auteur.csv", stringsAsFactors = FALSE)
  sen$nom = clean_names(paste(sen$prenom, sen$nomuse))
  
  # process problematic names
  # sen$nom[ sen$nom == "LOUIS FERDINAND ROCCA SERRA" ] = "FERDINAND ROCCA SERRA LOUIS"
  sen$nom[ sen$nom == "BERNARD MICHEL HUGO" ] = "BERNARD HUGO"
  # sen$nom[ sen$nom == "SOSEFO MAKAPE PAPILIO" ] = "MAKAPE PAPILIO SOSEFO"
  
  sen$autmat[ sen$nom == "JACKY DARNE" ] = "nnn" # not a senator
  sen$autmat = toupper(sen$autmat)
  
  match = with(sen, nom %in% unique(s$name))
  group = with(sen, is.na(prenom) | nchar(prenom) < 3 | prenom == "groupe")
  
  missing = sen$nom[ !match & sen$autmat %in% unique(s$autmat) ]
  if (length(missing))
    cat("Missing:", paste0(missing, collapse = ", "), "\n")
  
  sen = sen[ !group & match, ]
  
  sp = merge(s, sen[, c("autcod", "autmat", "autfct", "nom") ],
             by = "autmat", all.x = TRUE)
  
  nul = is.na(sp$autcod)
  if (sum(nul)) {
    
    # unrecognized authors
    dnk = grepl("s(é|e)nateur", sp$autfct[ nul ], ignore.case = TRUE)
    
    # warning if author status is senator
    if (any(dnk))
      cat("Dropping:", sum(dnk), "senators (no sponsorships)\n")
    
    # drop senators for very short periods
    cat("Dropping:", n_distinct(sp$name[ nul ][ !dnk ]),
        "sponsors (short-time mandates)\n")
    
    sp = sp[ !is.na(sp$autcod), ]
    
  }
  
  # load cross-table identifiers
  s = read.csv("data/dosleg-ecr.csv", stringsAsFactors = FALSE)
  s = merge(s, unique(sp[, c("autcod", "autfct", "name", "party", "url") ]),
            by = "autcod", all.x = TRUE)
  
  sp$autcod = NULL        
  
  nul = is.na(s$name) | is.na(s$autcod)
  if (sum(nul)) {
    
    # unrecognized authors
    dnk = grepl("s(é|e)nateur", s$autfct[ nul ], ignore.case = TRUE)
    
    # warning if author status is senator
    if (any(dnk))
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
  if (any(!sample)) {
    
    cat("Dropping:", nrow(s[ !sample, ]), "bills (not by a senator)\n")
    
    s = s[ sample, ]
    
  }
  
  s$status = ifelse(s$ecrnumtri == 1, "author", "cosponsor")
  names(s)[ names(s) == "texcod" ] = "uid"
  names(s)[ names(s) == "url" ] = "sponsors"
  
  # load Senator bills
  b = read.csv("data/dosleg-texte.csv", stringsAsFactors = FALSE)
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
  
  b = merge(b,
            aggregate(sponsors ~ uid, paste0, collapse = ";", data = subset(s, status == "cosponsor")),
            by = "uid", all.x = TRUE)
  
  b$sponsors = paste0(b$sponsors.x, ";", b$sponsors.y)
  b$sponsors = gsub(";NA$", "", b$sponsors)

  b = b[, c("legislature", "uid", "texte", "url", "date", "sponsors") ]
  b$n_au = 1 + str_count(b$sponsors, ";")
  
  table(b$n_au > 1, b$legislature, exclude = NULL)
  
  write.csv(b, bills, row.names = FALSE)
  
}

doc = read.csv(bills, stringsAsFactors = FALSE)
