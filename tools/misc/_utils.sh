#!/bin/bash

# Function to compute the number of seconds between two dates
# Example usage
# compute_seconds "2023-06-12 08:30:00" "2023-06-13 14:45:30"
date_diff_seconds() {
    local date1="$1"
    local date2="$2"

    # Convert dates to Unix timestamps
    local timestamp1=$(date -d "$date1" +%s)
    local timestamp2=$(date -d "$date2" +%s)

    # Compute the difference in seconds
    local seconds=$((timestamp2 - timestamp1))

    echo "$seconds"
}


