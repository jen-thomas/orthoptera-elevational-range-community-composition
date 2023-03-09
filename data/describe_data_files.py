from frictionless import describe, validate, Package, Field
from pprint import pprint


def create_package(list_of_resources):
    """Create package from a list of resources.

    Return package."""

    package = Package(list_of_resources)

    return package


def describe_package(package):
    """Describe package with additional metadata fields.

    Return package."""

    package.name = "orthoptera_pyrenees_2021"
    package.title = "Orthoptera elevational range, species richness and community composition in the Pyrenees"
    package.description = "Data and metadata for the article, 'Species richness of Orthoptera declines with " \
                          "elevation while elevational range of individual species peaks at mid elevation'"
    package.keywords = ["Orthoptera", "Rapoport's Rule", "species richness", "elevational gradient",
                        "elevational range", "community composition"]
    package.licenses = [{"name": "CC BY 4.0",
                         "path": "https://creativecommons.org/licenses/by/4.0/",
                         "title": "Creative Commons Attribution 4.0 International (CC BY 4.0)"}]
    package.profile = "data-package"
    package.contributors = [{
        "title": "Jen Thomas",
        "email": "jen@falciot.net",
        "path": "https://falciot.net",
        "role": "author"
    }]
    package.version = "1.0.0"

    return package


def package_to_json(package, json_package):
    """Convert the package description to JSON.

    Output into the specified file."""

    package.to_json(json_package)


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


def validate_schema(resource_schema):
    """Validate the data file against the resource schema, checking for errors."""

    report = validate(resource_schema)
    print(report)

    return report


def describe_observations_resource(resource):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("specimen_label").title = "Specimen label"
    resource.schema.get_field("specimen_label").description = "Unique label given to specimen"
    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("site_name").constraints["pattern"] = "[A-Z]{3}[0-9]{2}"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field(
        "date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("method").title = "Capture method"
    resource.schema.get_field("method").description = "Method by which insect was captured"
    resource.schema.get_field("method").constraints["enum"] = ["Hand", "Net"]
    resource.schema.get_field("method_repeat").title = "Capture method repeat"
    resource.schema.get_field(
        "method_repeat").description = "Integer determining repeat number of capture method during survey"
    resource.schema.get_field("method_repeat").constraints["enum"] = [1, 2]
    resource.schema.get_field("sex").title = "Sex"
    resource.schema.get_field("sex").description = "Sex of insect"
    resource.schema.get_field("sex").constraints["enum"] = ["Male", "Female", "Unknown"]
    resource.schema.get_field("stage").title = "Stage"
    resource.schema.get_field("stage").description = "Development stage of insect"
    resource.schema.get_field("stage").constraints["enum"] = ["Adult", "Nymph", "Unknown"]
    resource.schema.get_field("id_confidence").title = "ID confidence"
    resource.schema.get_field("id_confidence").description = "Identifier's confidence in identification of insect"
    resource.schema.get_field("id_confidence").constraints["enum"] = ["Confirmed", "Finalised"]
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

    # resource_to_json(resource, schema_json_file)
    return resource


def describe_sites_resource(resource):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("area").title = "Area"
    resource.schema.get_field("area").description = "Name of study area"
    resource.schema.get_field("area").constraints["enum"] = ["La Molinassa", "Besan", "Bordes de Viros", "Tor",
                                                             "Tavascan"]
    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("site_name").constraints["pattern"] = "[A-Z]{3}[0-9]{2}"
    resource.schema.get_field("elevational_band_m").title = "Elevational band (m)"
    resource.schema.get_field("elevational_band_m").description = "Elevational band in which study site was located (m)"
    resource.schema.get_field("elevational_band_m").constraints["minimum"] = 0
    resource.schema.get_field("elevational_band_m").constraints["maximum"] = 3000
    resource.schema.get_field("latitude_start_n").title = "Start latitude (N)"
    resource.schema.get_field("latitude_start_n").description = "Latitude at start of transect (decimal degrees N)"
    resource.schema.get_field("latitude_start_n").constraints["minimum"] = 0
    resource.schema.get_field("latitude_start_n").constraints["maximum"] = 90
    resource.schema.get_field("longitude_start_e").title = "Start longitude (E)"
    resource.schema.get_field("longitude_start_e").description = "Longitude at start of transect (decimal degrees E)"
    resource.schema.get_field("longitude_start_e").constraints["minimum"] = -180
    resource.schema.get_field("longitude_start_e").constraints["maximum"] = 180
    resource.schema.get_field("elevation_start_m").title = "Start elevation (m)"
    resource.schema.get_field("elevation_start_m").description = "Elevation at start of transect (m)"
    resource.schema.get_field("elevation_start_m").constraints["minimum"] = 0
    resource.schema.get_field("elevation_start_m").constraints["maximum"] = 3000
    resource.schema.get_field("latitude_end_n").title = "End latitude (N)"
    resource.schema.get_field("latitude_end_n").description = "Latitude at end of transect (decimal degrees N)"
    resource.schema.get_field("latitude_end_n").constraints["minimum"] = 0
    resource.schema.get_field("latitude_end_n").constraints["maximum"] = 90
    resource.schema.get_field("longitude_end_e").title = "End longitude (E)"
    resource.schema.get_field("longitude_end_e").description = "Longitude at end of transect (decimal degrees E)"
    resource.schema.get_field("longitude_end_e").constraints["minimum"] = -180
    resource.schema.get_field("longitude_end_e").constraints["maximum"] = 180
    resource.schema.get_field("elevation_end_m").title = "End elevation (m)"
    resource.schema.get_field("elevation_end_m").description = "Elevation at end of transect (m)"
    resource.schema.get_field("elevation_end_m").constraints["minimum"] = 0
    resource.schema.get_field("elevation_end_m").constraints["maximum"] = 3000
    resource.schema.get_field("transect_length_m").title = "Transect length (m)"
    resource.schema.get_field("transect_length_m").description = "Length of survey transect (m)"
    resource.schema.get_field("transect_length_m").constraints["minimum"] = 90
    resource.schema.get_field("transect_length_m").constraints["maximum"] = 110

    return resource

def describe_surveys_resource(resource):
    """Add further information to the metadata schema.

    Return the schema in JSON format."""

    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("site_name").constraints["pattern"] = "[A-Z]{3}[0-9]{2}"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field("date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("start_time_cest").title = "Start time (CEST)"
    resource.schema.get_field("start_time_cest").description = "Start time of survey (CEST)"
    resource.schema.get_field("end_time_cest").title = "End time (CEST)"
    resource.schema.get_field("end_time_cest").description = "End time of survey (CEST)"
    resource.schema.get_field("method").title = "Capture method"
    resource.schema.get_field("method").description = "Method by which insect was captured"
    resource.schema.get_field("method").constraints["enum"] = ["Hand", "Net"]
    resource.schema.get_field("method_repeat").title = "Capture method repeat"
    resource.schema.get_field(
        "method_repeat").description = "Integer determining repeat number of capture method during" \
                                       " survey"
    resource.schema.get_field("method_repeat").constraints["enum"] = [1, 2]
    resource.schema.get_field("cloud_coverage_start").title = "Start cloud coverage"
    resource.schema.get_field("cloud_coverage_start").description = "Cloud coverage at start of survey"
    resource.schema.get_field("cloud_coverage_start").constraints["minimum"] = 0
    resource.schema.get_field("cloud_coverage_start").constraints["maximum"] = 8
    resource.schema.get_field("wind_start").title = "Start wind"
    resource.schema.get_field("wind_start").description = "Wind at start of survey"
    resource.schema.get_field("wind_start").constraints["minimum"] = 0
    resource.schema.get_field("rain_start").title = "Start rain"
    resource.schema.get_field("rain_start").description = "Rain at start of survey"
    resource.schema.get_field("rain_start").constraints["minimum"] = 0
    resource.schema.get_field("cloud_coverage_end").title = "End cloud coverage"
    resource.schema.get_field("cloud_coverage_end").description = "Cloud coverage at end of survey"
    resource.schema.get_field("cloud_coverage_end").constraints["minimum"] = 0
    resource.schema.get_field("cloud_coverage_end").constraints["maximum"] = 8
    resource.schema.get_field("wind_end").title = "End wind"
    resource.schema.get_field("wind_end").description = "Wind at end of survey"
    resource.schema.get_field("wind_end").constraints["minimum"] = 0
    resource.schema.get_field("rain_end").title = "End rain"
    resource.schema.get_field("rain_end").description = "Rain at end of survey"
    resource.schema.get_field("rain_end").constraints["minimum"] = 0

    return resource

def describe_vegetation_plots_resource(resource):
    """Add further information to the metadata schema.

        Return the schema in JSON format."""

    resource.schema.get_field("site_name").title = "Site name"
    resource.schema.get_field("site_name").description = "Name of study site at which insect was captured"
    resource.schema.get_field("site_name").constraints["pattern"] = "[A-Z]{3}[0-9]{2}"
    resource.schema.get_field("date_cest").title = "Date (CEST)"
    resource.schema.get_field("date_cest").description = "Date on which insect was captured (CEST)"
    resource.schema.get_field("plot_distance_from_start_m").title = "Plot distance from start (m)"
    resource.schema.get_field(
        "plot_distance_from_start_m").description = "Distance of vegetation survey plot from start" \
                                                    " of survey transect (m)"
    resource.schema.get_field("plot_distance_from_start_m").constraints["minimum"] = 0
    resource.schema.get_field("plot_distance_from_start_m").constraints["maximum"] = 100
    resource.schema.get_field("percentage_vegetation_cover").title = "Percentage vegetation cover"
    resource.schema.get_field("percentage_vegetation_cover").description = "Percentage of plot covered by vegetation"
    resource.schema.get_field("percentage_vegetation_cover").constraints["minimum"] = 0
    resource.schema.get_field("percentage_vegetation_cover").constraints["maximum"] = 100
    resource.schema.get_field("percentage_bare_ground").title = "Percentage bare ground"
    resource.schema.get_field("percentage_bare_ground").description = "Percentage of plot covered by bare ground"
    resource.schema.get_field("percentage_bare_ground").constraints["minimum"] = 0
    resource.schema.get_field("percentage_bare_ground").constraints["maximum"] = 100
    resource.schema.get_field("percentage_rock").title = "Percentage rock cover"
    resource.schema.get_field("percentage_rock").description = "Percentage of plot covered by rock"
    resource.schema.get_field("percentage_rock").constraints["minimum"] = 0
    resource.schema.get_field("percentage_rock").constraints["maximum"] = 100
    resource.schema.get_field("height_75percent").title = "Height 75 percent of vegetation (cm)"
    resource.schema.get_field("height_75percent").description = "Height of 75 percent of vegetation in plot (cm)"
    resource.schema.get_field("height_75percent").constraints["minimum"] = 0
    resource.schema.get_field("max_height").title = "Maximum height of vegetation (cm)"
    resource.schema.get_field("max_height").description = "Maximum height of vegetation in plot (cm)"
    resource.schema.get_field("max_height").constraints["minimum"] = 0
    resource.schema.get_field("density_01").title = "Vegetation density point 1"
    resource.schema.get_field("density_01").description = "Vegetation density at point 1 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_01").constraints["minimum"] = 0
    resource.schema.get_field("density_02").title = "Vegetation density point 2"
    resource.schema.get_field("density_02").description = "Vegetation density at point 2 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_02").constraints["minimum"] = 0
    resource.schema.get_field("density_03").title = "Vegetation density point 3"
    resource.schema.get_field("density_03").description = "Vegetation density at point 3 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_03").constraints["minimum"] = 0
    resource.schema.get_field("density_04").title = "Vegetation density point 4"
    resource.schema.get_field("density_04").description = "Vegetation density at point 4 (number of times vegetation " \
                                                          "touches stick placed in plot corner)"
    resource.schema.get_field("density_04").constraints["minimum"] = 0
    resource.schema.get_field("density_05").title = "Vegetation density point 5"
    resource.schema.get_field("density_05").description = "Vegetation density at point 5 (number of times vegetation " \
                                                          "touches stick placed in middle of plot)"
    resource.schema.get_field("density_05").constraints["minimum"] = 0

    return resource


def main():

    # Create and describe package
    package = create_package(["observations.csv", "sites.csv", "surveys.csv", "vegetation_plots.csv"])
    package = describe_package(package)

    # Create resource descriptions and validate
    observations_resource = describe_data_file("observations.csv")
    observations_resource = describe_observations_resource(observations_resource)
    validate_schema(observations_resource)

    sites_resource = describe_data_file("sites.csv")
    sites_resource = describe_sites_resource(sites_resource)
    validate_schema(sites_resource)

    surveys_resource = describe_data_file("surveys.csv")
    surveys_resource = describe_surveys_resource(surveys_resource)
    validate_schema(surveys_resource)

    vegetation_plots_resource = describe_data_file("vegetation_plots.csv")
    vegetation_plots_resource = describe_vegetation_plots_resource(vegetation_plots_resource)
    validate_schema(vegetation_plots_resource)

    # Add resource descriptions to package resources
    package.get_resource("observations").schema = observations_resource.schema
    package.get_resource("observations").encoding = "utf-8"

    package.get_resource("sites").schema = sites_resource.schema
    package.get_resource("sites").encoding = "utf-8"

    package.get_resource("surveys").schema = surveys_resource.schema
    package.get_resource("surveys").encoding = "utf-8"

    package.get_resource("vegetation_plots").schema = vegetation_plots_resource.schema
    package.get_resource("vegetation_plots").encoding = "utf-8"

    package_to_json(package, "schema_package.json")


if __name__ == '__main__':
    main()
