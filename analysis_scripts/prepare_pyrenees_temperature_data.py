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


def count_rows(df):
    """Count the number of rows for each value of CODI_ESTACIO.

    Return summary of the number of rows."""

    value_counts = df['CODI_ESTACIO'].value_counts()

    return value_counts


def create_distinct_dfs(df, column_name):
    distinct_values = df[column_name].unique()
    distinct_dfs = {}

    for value in distinct_values:
        distinct_dfs[value] = df[df[column_name] == value]

    return distinct_dfs


def main():

    # Get data and summarise for sites at 2400 m
    data_file_2400 = "../data/xema/temperatures_catalunya_filtered.csv"
    df_2400 = get_data(data_file_2400)

    # Create distinct dataframes for each of the met stations
    distinct_dfs_estacions_2400 = create_distinct_dfs(df_2400, 'CODI_ESTACIO')

    # Get the summary data for each of these met stations
    for value, distinct_df in distinct_dfs_estacions_2400.items():
        print(f"Summary stats for '{value}':")
        print(get_summary_stats(distinct_df))
        print("--------")


if __name__ == '__main__':
    main()
