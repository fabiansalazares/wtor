<!-- badges: start -->
[![R-CMD-check](https://github.com/fabiansalazares/wtor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fabiansalazares/wtor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# wtor - A R-language client for the World Trade Organization (WTO) API

wtor is a R-language client for the World Trade Organization data APIs. You can find more information about the API, as well as get your API key here: https://apiportal.wto.org/ 

By Miguel Fabián Salazar - miguel@fabiansalazar.es - [fabiansalazar.es](https://fabiansalazar.es) - GPL v3

# Install

1. In .Renviron, set environment variable WTO_R_API_KEY to the value of your API key. Run `usethis::edit_r_environ()`, and then add a line such as `WTO_R_API_KEY=xxxxxxxxxxx`, where xxxxxxxxxx corresponds to your private WTO API key.
2. Run the following: `remotes::install_github("fabiansalazares/wtor")`

# To-do

* Select reporting and partner economies by ISO code.
* Implement unimplemented APIs. As of 6/8/2024, only the Timeseries API is fully working.

# Timeseries API

## Implemented endpoints

Endpoint|Implemented
---|-----
Datapoints (GET) | No
Datapoints (POST) | Yes
Datacount | Yes 
Metadata | Yes
Topics | Yes 
Frequencies | Yes 
Periods | Yes 
Units | Yes 
Indicator categories | Yes 
Indicators | Yes
Geographical regions | Yes 
Economic groups | Yes 
Reporting economies | Yes 
Partner economies | Yes 
Classifications | Yes  
Product/sectors | Yes
Years | Yes 
Value flags | Yes 



## Examples

#### Retrieve the list of all available indicators:

`wtor::get_timeseries_available_indicators()`

### Bilateral trade between United States of America and Spain, by HS6 product code:

`wtor::get_timeseries_data(code = "HS_M_0010", reporting_economies="United States of America", partner_economies = "Spain", products_or_sectors = "HS6")`

### Countries belonging to each group of countries

```

groups_of_countries <- wtor::get_groups()

lapply(
X=groups_of_countries |> _$code, # retrieve groups of countries from the API and extract the "code" column that will be iterated on
FUN=function(.group_code) {
  message("Querying group: ", .group_code)
  Sys.sleep(1) # do not request more than one query per second
  wtor::get_reporting_economies(gp=.group_code) |> 
    dplyr::mutate(
      group_code = .group_code,
      group_name = groups_of_countries |> dplyr::filter(code == .group_code) |> _$name
    )

}
) |> 
dplyr::bind_rows()

```

# ePing

## Implemented endpoints

## Examples

# Quantitative Restrictions (QR)

## Implemented endpoints

Endpoint|Implemented
---|-----
HS Versions | Yes
Member | Yes 
Notification | Yes
Product | Yes 
QR details | Yes 
QR list | Yes 


## Examples

# Trade Facilitation Agreement Database (TFAD)

## Implemented endpoints

## Examples 

