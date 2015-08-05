# party colors

colors = c(
  "COM" = "#E41A1C", # COM/GDR            -- red
  "ECO" = "#4DAF4A", # Verts              -- green
  "SOC" = "#F781BF", # PS                 -- pink
  "RAD" = "#FFFF33", # RAD/RRDP/RDSE      -- yellow
  "CEN" = "#FF7F00", # UDF/MODEM/UDI/etc. -- orange
  "DRO" = "#377EB8", # RPR/UMP/etc.       -- blue
  "FN"  = "#A65628", #                    -- brown
  "IND" = "#AAAAAA"  # SE                 -- light grey
)

groups = c(
  "COM" = "Communists",
  "ECO" = "Ecologists",
  "SOC" = "Socialists",
  "RAD" = "Radicals",
  "CEN" = "Centrists",
  "DRO" = "Conservatives",
  "FN"  = "Front national",
  "IND" = "Independents"
)

# ParlGov Left/Right scores

scores = c(
  "COM" = 1.4, # PCF
  "ECO" = 3.2, # Verts
  "SOC" = 3.2, # PS
  "RAD" = 4.1, # PRG
  "CEN" = 6.1, # UDF
  "DRO" = 7.5, # RPR/UMP
  "FN"  = 9.7, # FN
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
