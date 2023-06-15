import pandas


def get_data(file):
    """Read data from CSV file into a dataframe.

    Return dataframe."""

    df = pandas.read_csv(file, header=0)

    return df


def get_summary_stats(df):
    """Get summary statistics about dataframe for specific field.

    Return summary stats."""

    df_stats = df['VALOR_LECTURA'].describe()

    return df_stats


def main():

    # Get data and summarise for sites at 2400 m
    data_file_2400 = "../data/xema/temperatures_catalunya_filtered.csv"
    df_2400 = get_data(data_file_2400)

    df_stats_2400 = get_summary_stats(df_2400)
    print(df_stats_2400)


if __name__ == '__main__':
    main()
