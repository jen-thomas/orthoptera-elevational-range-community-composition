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


def sites_schema(resource, schema_json_file):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("area").title = "Area"
    resource.schema.get_field("area").description = "Name of study area"
    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("elevational_band_m").title = "Elevational band (m)"
    resource.schema.get_field("elevational_band_m").description = "Elevational band in which study site was located (m)"
    resource.schema.get_field("latitude_start_n").title = "Start latitude (N)"
    resource.schema.get_field("latitude_start_n").description = "Latitude at start of transect (decimal degrees N)"
    resource.schema.get_field("longitude_start_e").title = "Start longitude (E)"
    resource.schema.get_field("longitude_start_e").description = "Longitude at start of transect (decimal degrees E)"
    resource.schema.get_field("elevation_start_m").title = "Start elevation (m)"
    resource.schema.get_field("elevation_start_m").description = "Elevation at start of transect (m)"
    resource.schema.get_field("latitude_end_n").title = "End latitude (N)"
    resource.schema.get_field("latitude_end_n").description = "Latitude at end of transect (decimal degrees N)"
    resource.schema.get_field("longitude_end_e").title = "End longitude (E)"
    resource.schema.get_field("longitude_end_e").description = "Longitude at end of transect (decimal degrees E)"
    resource.schema.get_field("elevation_end_m").title = "End elevation (m)"
    resource.schema.get_field("elevation_end_m").description = "Elevation at end of transect (m)"
    resource.schema.get_field("transect_length_m").title = "Transect length (m)"
    resource.schema.get_field("transect_length_m").description = "Length of survey transect (m)"

    resource_to_json(resource, schema_json_file)


def surveys_schema(resource, schema_json_file):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field("date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("start_time_cest").title = "Start time (CEST)"
    resource.schema.get_field("start_time_cest").description = "Start time of survey (CEST)"
    resource.schema.get_field("end_time_cest").title = "End time (CEST)"
    resource.schema.get_field("end_time_cest").description = "End time of survey (CEST)"
    resource.schema.get_field("method").title = "Capture method"
    resource.schema.get_field("method").description = "Method by which insect was captured"
    resource.schema.get_field("method_repeat").title = "Capture method repeat"
    resource.schema.get_field("method_repeat").description = "Integer determining repeat number of capture method during" \
                                                             " survey"
    resource.schema.get_field("cloud_coverage_start").title = "Start cloud coverage"
    resource.schema.get_field("cloud_coverage_start").description = "Cloud coverage at start of survey"
    resource.schema.get_field("wind_start").title = "Start wind"
    resource.schema.get_field("wind_start").description = "Wind at start of survey"
    resource.schema.get_field("rain_start").title = "Start rain"
    resource.schema.get_field("rain_start").description = "Rain at start of survey"
    resource.schema.get_field("cloud_coverage_end").title = "End cloud coverage"
    resource.schema.get_field("cloud_coverage_end").description = "Cloud coverage at end of survey"
    resource.schema.get_field("wind_end").title = "End wind"
    resource.schema.get_field("wind_end").description = "Wind at end of survey"
    resource.schema.get_field("rain_end").title = "End rain"
    resource.schema.get_field("rain_end").description = "Rain at end of survey"

    resource_to_json(resource, schema_json_file)


def vegetation_plots_schema(resource, schema_json_file):
    """Add further information to the metadata schema.

        Return the schema in JSON format."""

    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field("date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("plot_distance_from_start_m").title = "Plot distance from start (m)"
    resource.schema.get_field("plot_distance_from_start_m").description = "Distance of vegetation survey plot from start" \
                                                                          " of survey transect (m)"
    resource.schema.get_field("percentage_vegetation_cover").title = "Percentage vegetation cover"
    resource.schema.get_field("percentage_vegetation_cover").description = "Percentage of plot covered by vegetation"
    resource.schema.get_field("percentage_bare_ground").title = "Percentage bare ground"
    resource.schema.get_field("percentage_bare_ground").description = "Percentage of plot covered by bare ground"
    resource.schema.get_field("percentage_rock").title = "Percentage rock cover"
    resource.schema.get_field("percentage_rock").description = "Percentage of plot covered by rock"
    resource.schema.get_field("height_75percent").title = "Height 75 percent of vegetation (cm)"
    resource.schema.get_field("height_75percent").description = "Height of 75 percent of vegetation in plot (cm)"
    resource.schema.get_field("max_height").title = "Maximum height of vegetation (cm)"
    resource.schema.get_field("max_height").description = "Maximum height of vegetation in plot (cm)"
    resource.schema.get_field("density_01").title = "Vegetation density point 1"
    resource.schema.get_field("density_01").description = "Vegetation density at point 1 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_02").title = "Vegetation density point 2"
    resource.schema.get_field("density_02").description = "Vegetation density at point 2 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_03").title = "Vegetation density point 3"
    resource.schema.get_field("density_03").description = "Vegetation density at point 3 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_04").title = "Vegetation density point 4"
    resource.schema.get_field("density_04").description = "Vegetation density at point 4 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_05").title = "Vegetation density point 5"
    resource.schema.get_field("density_05").description = "Vegetation density at point 5 (number of times vegetation " \
                                                          "touches stick placed in middle of plot)"

    resource_to_json(resource, schema_json_file)


def main():

    observations_resource = describe_data_file("data/observations.csv")
    observations_schema(observations_resource, "frictionless_data/schema_observations.json")
    validate_data_file("frictionless_data/schema_observations.json")

    sites_resource = describe_data_file("data/sites.csv")
    sites_schema(sites_resource, "frictionless_data/schema_sites.json")
    validate_data_file("frictionless_data/schema_sites.json")

    surveys_resource = describe_data_file("data/surveys.csv")
    surveys_schema(surveys_resource, "frictionless_data/schema_surveys.json")
    validate_data_file("frictionless_data/schema_surveys.json")

    vegetation_plots_resource = describe_data_file("data/vegetation_plots.csv")
    vegetation_plots_schema(vegetation_plots_resource, "frictionless_data/schema_vegetation_plots.json")
    validate_data_file("frictionless_data/schema_vegetation_plots.json")


if __name__ == '__main__':
    main()