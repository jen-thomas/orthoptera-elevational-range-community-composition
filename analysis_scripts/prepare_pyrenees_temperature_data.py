import csv


def get_data(file):
    """Open the data file."""

    with open(file, newline='') as csvfile:
        data = csv.reader(csvfile, delimiter=' ', quotechar='|')
        for row in data:
            print(', '.join(row))

    return data