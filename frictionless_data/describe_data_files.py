from frictionless import describe


def describe_data_file(data_file, schema_file_name):
    """Describe a data file using the frictionless schema. Use JSON (rather than YAML).

    Return the JSON schema."""

    package = describe(data_file, type="package")
    json_package = package.to_json()

    return json_package


describe_data_file("../data/observations.csv", "schema_observations.json")
