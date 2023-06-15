#!/bin/bash

input_file="../data/xema/temperatures-catalunya_exemple.csv"
output_file="../data/xema/temperatures-catalunya_filtered_exemple.csv"

# Write the header line to the output file
head -n 1 "$input_file" > "$output_file"

# Select lines with dates from 2021, specific CODI_ESTACIO values and with the CODI_VARIABLE of 32 (temperature).
# Append lines o the output file.
grep -E '(^|,)D2|(^|,)ZD' "$input_file" | grep -E ',[0-9]{2}/[0-9]{2}/2021' | grep '.*,.*,32,.*' >> "$output_file"

