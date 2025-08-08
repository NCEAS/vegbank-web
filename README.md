## VegBank Web: A Web App for Vegetation Plot Data
- **Authors**: Gill, Darian; Regetz, Jim (0009-0008-2666-6229);
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on GitHub](https://github.com/NCEAS/vegbank-web)
- [**Submit Bugs and feature requests**](https://github.com/NCEAS/vegbank-web/issues)


This package is an web access point for VegBank, the vegetation plot database of the Ecological Society of America's Panel on Vegetation Classification, hosted by the National Center for Ecological Analysis and Synthesis (NCEAS). VegBank contains vegetation plot data, community types recognized by the U.S. National Vegetation Classification and others, and all ITIS/USDA plant taxa along with other taxa recorded in plot records. As a VegBank API client, the 'vegbankweb' package supports browsing, searching, inspecting, and downloading vegetation plot records and other supporting information to and from the VegBank database.

VegBank in general, and the vegbankweb package in particular, are open source, community projects. We [welcome contributions](./CONTRIBUTING.md) in many forms, including code, data, documentation, bug reports, testing, etc. Use the VegBank discussions to discuss these contributions with us.


## Documentation

Documentation is a work in progress, and can be found ...

## Development build

This is a [Shiny App](https://shiny.posit.co/) built using the [Shiny R package](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/). 

The app is currently configured to read from inst/cached_data RDS files by default, or hit an api running on kubernetes for data if you set the use_api parameter for load_data_type in server.R to TRUE.

To run the app locally, run the following commands your IDE terminal: 

```
~ % R

~ % devtools::load_all()

~ % vegbankweb::run_app()
```

## License
```
Copyright [2025] [Regents of the University of California]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

## Acknowledgements
Work on this package was supported by:

- The National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)
