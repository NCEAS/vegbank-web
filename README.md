## VegBank Web: A Shiny Web App for Vegetation Plot Data

- **License**: [Apache 2](https://opensource.org/licenses/Apache-2.0)
- [Package source code on GitHub](https://github.com/NCEAS/vegbank-web)
- [**Submit Bugs and feature requests**](https://github.com/NCEAS/vegbank-web/issues)
- Contact us: help@vegbank.org

## Citation

> Darian Gill, Jim Regetz, Matthew B. Jones, Michael T. Lee, Matthew Brooke, Robert K. Peet (2026) **VegBank Web: A Shiny Web App for Vegetation Plot Data**. Version 1.0.2. VegBank. [doi:10.82902/J1H59R](https://doi.org/10.82902/J1H59R)

## Overview

This package is a web access point for VegBank, the vegetation plot database of the [Ecological Society of America's Panel on Vegetation Classification](https://esa.org/vegpanel/), hosted by the [National Center for Ecological Analysis and Synthesis](https://www.nceas.ucsb.edu/) (NCEAS). You can find it running at [vegbank.org](https://vegbank.org/).

VegBank contains vegetation plot and community data that supports the development of the [U.S. National Vegetation Classification](https://usnvc.org/explore-classification/). As a [VegBank API](https://nceas.github.io/vegbank2/api/) client, the 'vegbankweb' package supports browsing, searching, and inspecting vegetation plot records and other supporting information in the VegBank database.

VegBank in general, and the vegbankweb package in particular, are open source, community projects. We [welcome contributions](./CONTRIBUTING.md) in many forms, including code, data, documentation, bug reports, testing, etc. Use the [VegBank Discussions](https://github.com/NCEAS/vegbank2/discussions), or email help@vegbank.org to discuss these contributions with us.

The data shown in vegbankweb are also available for access with R via the [vegbankr R package](https://nceas.github.io/vegbankr/), and in other languages via the [VegBank REST API](https://nceas.github.io/vegbank2/api/). To upload data, request contributor permissions from the ESA Vegetation Classification Panel. Once your contributor role is granted, you will be able to upload new plot data with the vegbankr R package.



## Running Locally

This is a [Shiny App](https://shiny.posit.co/) built using the [Shiny R package](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/). 

The app is currently configured to use the [vegbankr](https://github.com/NCEAS/vegbankr) package and underlying [vegbank2 api](https://github.com/NCEAS/vegbank2) for data.

To run the app locally, you'll need to [install R](https://www.r-project.org/) and [download this repository](https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github).

By default the app uses the `inst/config.yml` bundled with the package (sourced via `system.file`). To override this, create a `.Renviron` file in the root directory of the repository and set the `R_CONFIG_FILE` environment variable to the absolute path of a [valid config.yml](https://rstudio.github.io/config/articles/introduction.html#embedding-r-code-inside-the-yaml-file) file. You can also select a named configuration block by setting the `R_CONFIG_ACTIVE` environment variable (defaults to `"default"` if not set).

For example, to select the production configuration in a custom config file, add something like:

```
# vegbank-web/.Renviron
R_CONFIG_FILE="/Users/yourUserName/git/vegbank-web/inst/config.yml"
R_CONFIG_ACTIVE="production"
```

Finally run the following commands in your IDE terminal: 

```
~ % R

# or use `devtools::load_all()` if you prefer
~ remotes::install_github("NCEAS/vegbank-web")

~ % vegbankweb::run_app()
```

## Current Contributors

- Darian Gill (dgill@nceas.ucsb.edu): ORCID: [0009-0005-7848-2163](https://orcid.org/0009-0005-7848-2163)
- Jim Regetz (regetz@nceas.ucsb.edu): ORCID: [0009-0008-2666-6229](https://orcid.org/0009-0008-2666-6229)
- Matthew B. Jones (jones@nceas.ucsb.edu): ORCID: [0000-0003-0077-4738](https://orcid.org/0000-0003-0077-4738)
- Michael T. Lee (michael.lee@unc.edu): ORCID: [0009-0003-3874-8604](https://orcid.org/0009-0003-3874-8604)
- Matthew Brooke (brooke@nceas.ucsb.edu): ORCID: [0000-0002-1472-913X](https://orcid.org/0000-0002-1472-913X)
- Robert K. Peet (peet@unc.edu): ORCID: [0000-0003-2823-6587](https://orcid.org/0000-0003-2823-6587)

## License
```
Copyright [2026] [Regents of the University of California]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

## Acknowledgements
Work on this package was supported by:

- California Department of Fish and Wildlife
- The ESA Panel on Vegetation Classification

Additional support was provided for collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)
