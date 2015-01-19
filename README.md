This repository contains code to build cosponsorship networks from bills passed in the [lower][an] and [upper][se] chambers of the French Parliament.

- [interactive demo](http://briatte.org/parlement)
- [static plots](http://briatte.org/parlement/plots.html)

[an]: http://www.assemblee-nationale.fr/
[se]: http://www.senat.fr/

The code is a simplified version of the [`neta`](https://github.com/briatte/neta) repository, which handles more data in more ways.

The Senate data comes from their [open data portal][ds]. The code was tested on bills and sponsors up to the senatorial election of September 2014.

[ds]: http://data.senat.fr/

# HOWTO

Replicate by running `make.r` in R. Due to a [small bug](https://github.com/hadley/lubridate/issues/194) that got fixed in R 3.0.3, the code needs to run in R >= 3.0.3 to handle French dates properly.

The `sponsors-an.r` and `dossiers-an.r` scripts deal with the lower house; it will scrape all sponsors for legislatures 8-14 and all dossiers for the same legislatures. Legislature 10 is then excluded from the network building routine in `build-an.r`, as cosponsors are not reported for bills registered with the National Assembly during that period.

The `sponsors-se.r`, `dossiers-se.r` and `build-se.r` scripts carry the same operations as above for the upper house, for the same time period. Because Senate bills come from a PostgreSQL dump downloaded from its [open data portal][ds], the code expects the user `opendata` to find the `dosleg` database on port 5432. The basic steps in SQL are as follows:

```{SQL}
CREATE USER opendata;
CREATE DATABASE dosleg;
```

The [Dosleg](http://data.senat.fr/dosleg/) database should then be imported in full. The code will use the functions in `functions-pgsql.r` to query the database.

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
- `family_name` -- family name
- `sex` -- gender (M/F), imputed from birth information ("Né/e")
- `born` -- year of birth (stored as character)
- `party` -- political party, abbreviated (see below)
- `constituency` -- constituency, stored as the string to its Wikipedia Francophone entry
- `nyears` -- number of years in office before start of legislature
- `lon` -- constituency longitude
- `lat` -- constituency latitude
- `url` -- sponsor URL, shortened to unique identifier
- `photo` -- numeric dummy coding for the presence of a photo

## Parties

Political parties in France fall into a few broad categories, with many different renamings and alliances to form parliamentary groups. The party names were simplified to the following categories:

- `COM` -- Communists (PCF, GDR, CRC)
- `ECO` -- Ecologists (Verts)
- `SOC` -- Socialists (PS)
- `RAD` -- Radicals, including:
	- the RCV "gauche plurielle" group of legislature 12 (1997-2002) in the lower house
	- the RRDP and RDSE groups in the upper house
- `CEN` -- Centrists (UDF, MODEM, UDI and various small parties that formed the UDF)
- `DRO` -- Conservatives (RPR, DL and then UMP)
- `FN` -- Front national (lower house during legislature 8, 1986-1988)
- `IND` -- Independents ("sans étiquette" and "non inscrits"), often from the centre-right ("DVD")

# THANKS

* [Authueil][authueil] and [Benjamin][roux] from [Regards Citoyens][rc] for comments
* Erik Gregory for [PostgreSQL R functions](http://anrprogrammer.wordpress.com/2013/07/27/easier-database-querying-with-r/)
* Sébastien Dubourg and the rest of the [Data Sénat](http://data.senat.fr/) team

[authueil]: https://twitter.com/Authueil
[rc]: http://www.regardscitoyens.org/
[roux]: http://www.medialab.sciences-po.fr/people/benjamin-ooghe-tabanou/
