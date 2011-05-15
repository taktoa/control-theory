./Main | sed 's/),(/\n/g; s/,/ /g; s/)\]//g; s/\[(//g;' > ./temp.csv
./script
