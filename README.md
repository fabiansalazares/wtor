# wtor - A R-language client for the World Trade Organization (WTO) API

wtor is a R-language client for the World Trade Organization data APIs. You can find more information about the API, as well as get your API key here: https://apiportal.wto.org/ 

By Miguel Fabián Salazar - miguel@fabiansalazar.es - [fabiansalazar.es](https://fabiansalazar.es) - GPL v3

# Install

Run the following on your R terminal: `remotes::install_github("fabiansalazares/wtor")`

# To-do

* Request pagination for large amounts of datapoints.
* Reporting and partner economies by ISO code.
* Implement unimplemented APIs. Currently only the Timeseries API is fully working.

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

### Bilateral trade between two countries, by HS6 product code:

`wtor::get_timeseries_data(code = "HS_M_0100", reporting_economies="United States of America", partner_economies = "Spain", products_or_sectors = "HS6")`


# ePing

## Implemented endpoints

# Quantitative Restrictions (QR)

## Implemented endpoints

# Trade Facilitation Agreement Database (TFAD)

## Implemented endpoints

