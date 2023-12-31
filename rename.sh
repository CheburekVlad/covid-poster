#!/bin/bash

directory="/home/cheburek/Desktop/csse_covid_19_daily_reports"

cd "$directory" || exit

for file in *.csv; do
    date_part=$(echo "$file" | grep -oP '\d{2}-\d{2}-\d{4}')
    IFS="-"
    read -r month day year <<< "$date_part"
    IFS=" "

    new="$year$month${day}.csv"

    mv $file $new
done