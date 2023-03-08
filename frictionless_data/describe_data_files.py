from frictionless import describe, validate
from pprint import pprint

def describe_data_file(data_file):
    """Describe a data file using the frictionless schema.

    Return the resource description."""

    resource = describe(data_file, type="resource")

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
    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field("date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("method").title = "Capture method"
    resource.schema.get_field("method").description = "Method by which insect was captured"
    resource.schema.get_field("method_repeat").title = "Capture method repeat"
    resource.schema.get_field("method_repeat").description = "Integer determining repeat number of capture method during" \
                                                             " survey"
    resource.schema.get_field("sex").title = "Sex"
    resource.schema.get_field("sex").description = "Sex of insect"
    resource.schema.get_field("stage").title = "Stage"
    resource.schema.get_field("stage").description = "Development stage of insect"
    resource.schema.get_field("id_confidence").title = "ID confidence"
    resource.schema.get_field("id_confidence").description = "Identifier's confidence in identification of insect"
    resource.schema.get_field("suborder").title = "Suborder"
    resource.schema.get_field("suborder").description = "Suborder"
    resource.schema.get_field("family").title = "Family"
    resource.schema.get_field("family").description = "Family"
    resource.schema.get_field("subfamily").title = "Subfamily"
    resource.schema.get_field("subfamily").description = "Subfamily"
    resource.schema.get_field("genus").title = "Genus"
    resource.schema.get_field("genus").description = "Genus"
    resource.schema.get_field("species").title = "Species"
    resource.schema.get_field("species").description = "Species"

    resource_to_json(resource, schema_json_file)


def main():

    observations_resource = describe_data_file("data/observations.csv")
    observations_schema(observations_resource, "frictionless_data/schema_observations.json")
    validate_data_file("frictionless_data/schema_observations.json")


if __name__ == '__main__':
    main()