#!/bin/bash

input_file="../dades/temperatures-catalunya.csv"
output_file="../dades/temperatures_catalunya_filtered.csv"

# Write the header line to the output file
head -n 1 "$input_file" > "$output_file"

# Select lines with dates from 2021, specific CODI_ESTACIO values and with the CODI_VARIABLE of 32 (temperature).
# Append lines o the output file.
grep -E '(^|,)ZB|(^|,)Z5' "$input_file" | grep -E ',[0-9]{2}/[0-9]{2}/2021' | grep -E '[A-Z0-9]{14},[A-Z][A-Z0-9]\,32,.*' >> "$output_file"