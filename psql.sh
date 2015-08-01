#
# 1. Download Dosleg database and unzip to root directory
#

# download dosleg.zip
curl -o data/dosleg.zip http://data.senat.fr/data/dosleg/dosleg.zip
# unzip dosleg.sql
unzip -o data/dosleg.zip

#
# 2. Create `opendata` user `dosleg` database
#

# create user 'opendata'
psql -c "create role opendata login"
# create database 'dosleg'
psql -c "create database dosleg"
# import PostgreSQL dump
psql -d dosleg -U opendata -f dosleg.sql

#
# 3. Export selected data from tables `auteur`, `ecr` and `texte`
#

# export selected columns from table 'auteur'
psql -c "\copy (select autcod, autfct, nomuse, prenom, autmat from auteur) to data/dosleg-auteur.csv csv header" dosleg opendata
# export selected columns from table 'ecr'
psql -c "\copy (select texcod, autcod, ecrnumtri from ecr where typedoc = 'T') to data/dosleg-ecr.csv csv header" dosleg opendata
# export selected columns from table 'texte'
psql -c "\copy (select texcod, sesann, txtoritxtdat, texnum, texurl from texte where sesann > 1985 and typtxtcod = '1') to data/dosleg-texte.csv csv header" dosleg opendata

#
# 4. Revert PostgreSQL installation to initial state
#

# remove database 'dosleg'
psql -c "drop database dosleg"
# remove user 'opendata'
psql -c "drop role opendata"

#
# 5. Remove `dosleg.sql` from the root directory
#

# remove dosleg.sql
rm dosleg.sql
