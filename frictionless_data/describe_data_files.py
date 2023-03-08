from frictionless import describe, validate
from pprint import pprint

def describe_data_file(data_file):
    """Describe a data file using the frictionless schema.

    Return the resource description."""

    resource = describe(data_file, type="resource")
    pprint(resource)

    return resource


def resource_to_json(resource, json_schema):
    """Convert the resource description to JSON.

    Output into the specified file."""

    resource.to_json(json_schema)


def validate_data_file(resource_schema):
    """Validate the data file against the resource schema, checking for errors."""

    report = validate('resource_schema')
    pprint(report.flatten(["rowNumber", "fieldNumber", "type"]))


def observations_schema(resource, schema_json_file):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("specimen_label").title = "Specimen label"
    resource.schema.get_field("specimen_label").description = "Unique label given to specimen"

    resource_to_json(resource, schema_json_file)


def main():

    observations_resource = describe_data_file("data/observations.csv")
    observations_schema(observations_resource, "frictionless_data/schema_observations.json")
    validate_data_file("frictionless_data/schema_observations.json")


if __name__ == '__main__':
    main()