This repository contains code to build cosponsorship networks from bills passed in the [lower][an] and [upper][se] chambers of the French Parliament.

- [interactive demo](http://f.briatte.org/parlviz/parlement)
- [static plots](http://f.briatte.org/parlviz/parlement/plots.html)
- [more countries](https://github.com/briatte/parlnet)

[an]: http://www.assemblee-nationale.fr/
[se]: http://www.senat.fr/

The code is a simplified version of the [`neta`](https://github.com/briatte/neta) repository, which handles more data in more ways.

# HOWTO

Replicate by running `psql.sh` to export some tables from PostgreSQL for the French Senate, and then by running `make.r` in R. __You will need a PostgreSQL installation to run the scripts:__ see below for further instructions.

The `sponsors-an.r` and `dossiers-an.r` scripts deal with the lower house; it will scrape all sponsors for legislatures 8-14 and all dossiers for the same legislatures. Legislature 10 is then excluded from the network building routine in `build-an.r`, as cosponsors are not reported for bills registered with the National Assembly during that period.

The `sponsors-se.r`, `dossiers-se.r` and `build-se.r` scripts carry the same operations as above for the upper house, for the same time period. Because Senate bills come from a PostgreSQL dump downloaded from its [open data portal][ds], the code expects to find three CSV files exported from the [Dosleg][dosleg] database in the `data` folder.

After installing [PostgreSQL](http://www.postgresql.org/), just run the [`psql.sh`](psql.sh) shell script to download the Dosleg database, import it into PostgreSQL and export the relevant data:

```sh
sh psql.sh
```

The script will clean up by removing the Dosleg database from your PostgreSQL installation, but it will keep a zipped copy of the original dump in the `data` folder.

[ds]: http://data.senat.fr/
[dosleg]: http://data.senat.fr/dosleg/

## Additional functions

An extra set of routines applied to both houses is coded into `functions-fr.r`: sponsor names are simplified by stripping some particles and punctuation, sponsor constituencies are geocoded, and bills are split according to the elections dates of the National Assembly.

# DATA

## Bills

Assemblée nationale:

- `legislature` -- legislature number
- `url` -- bill URL
- `title` -- short title, including precise date of introduction
- `au` -- semicolon-separated bill sponsors, as names (some from the Senate)
- `au_url` -- semicolon-separated URLs of bill sponsors (some from the Senate)
- `co` -- semicolon-separated bill cosponsors, as names (some from the Senate)
- `co_url` -- semicolon-separated URLs of bill cosponsors (some from the Senate)

Sénat:

- `legislature` -- legislature number
- `url` -- bill URL
- `uid` -- unique identifier (`texcod` in the SQL tables)
- `texte` -- bill number
- `date` -- date of examination by chamber
- `sponsors` -- semicolon-separated bill sponsors, as shortened URLs
- `n_au` -- toal number of sponsors

## Sponsors

The sponsors data has one row per sponsor-legislature. Both houses hold the same variables (although constituencies are regional for senators, and seniority is computed over more possible election dates).

- `legislature` -- legislature of activity
- `fullname` -- original name
- `name` -- simplified name, uppercase
- `sex` -- gender (M/F), imputed from birth information ("Né/e")
- `born` -- year of birth (stored as character)
- `party` -- political party, abbreviated (see below)
- `constituency` -- constituency, stored as the string to its Wikipedia Francophone entry
- `nyears` -- number of years in office before start of legislature
- `lon` -- constituency longitude
- `lat` -- constituency latitude
- `url` -- sponsor URL, shortened to unique identifier
- `photo` -- numeric dummy coding for the presence of a photo

The constituency coordinates are provided as CSV files in the `data` folder of the repository, and can be rebuilt by calling the `geocode` function of the `ggmap` package after uncommenting a few lines in the code.

## Parties

Political parties in France fall into a few broad categories, with many different renamings and alliances to form parliamentary groups. The party names were simplified to the following categories:

- `COM` -- Communists (PCF, GDR, CRC)
- `ECO` -- Ecologists (Verts)
- `SOC` -- Socialists (PS)
- `RAD` -- Radicals, including:
	- the RCV "gauche plurielle" group of legislature 12 (1997-2002) in the lower house
	- the RRDP and RDSE groups in the upper house
- `CEN` -- Centrists (UDF, MODEM, UDI and various small parties that formed the UDF)
- `DRO` -- Conservatives (RPR, DL and then UMP/LR)
- `FN` -- Front national (lower house during legislature 8, 1986-1988)
- `IND` -- Independents ("sans étiquette" and "non inscrits"), often from the centre-right ("DVD")

# THANKS

* [Authueil][authueil] and [Benjamin][roux] from [Regards Citoyens][rc] for comments
* Sébastien Dubourg and the rest of the [Data Sénat](http://data.senat.fr/) team

[authueil]: https://twitter.com/Authueil
[rc]: http://www.regardscitoyens.org/
[roux]: http://www.medialab.sciences-po.fr/people/benjamin-ooghe-tabanou/
