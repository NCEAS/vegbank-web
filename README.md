## Vegbank Web: A Web App for Vegatative Plot Data

- **Authors**: Gill, Darian; Regetz, Jim (0009-0008-2666-6229);
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on GitHub](https://github.com/NCEAS/vegbank-web)
- [**Submit Bugs and feature requests**](https://github.com/NCEAS/vegbank-web/issues)
- Contact us: support@dataone.org
- [DataONE discussions](https://github.com/DataONEorg/dataone/discussions)

Vegbank is a database of vegetation plot data. This app is intended to provide a web access point for browsing, searching, inspecting, and downloading that plot data.

DataONE in general, and HashStore in particular, are open source, community projects.  We [welcome contributions](./CONTRIBUTING.md) in many forms, including code, graphics, documentation, bug reports, testing, etc.  Use the [DataONE discussions](https://github.com/DataONEorg/dataone/discussions) to discuss these contributions with us.


## Documentation

Documentation is a work in progress, and can be found ...

## Development build

This is a [Shiny App](https://shiny.posit.co/) built using the [Shiny R package](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/). 

The app is currently configured to read from a local file or hit an api running on kubernetes for data. If you don't have access to kubernetes you ask a team member for the all_states_plot_obs.json file, place it in your www directory, and can revise the `server.R` code read its path.

To run the app locally with full access to the api, you'll need to connect to the `dev-vegbank` kubernetes context and port forward the vegbankdb pod with a rotating name (not the postgresql pod) as follows: 

*Note: your pod name will be different from this example command.*
```
~ % kubectl config use-context dev-vegbank

~ % kubectl get pods
(Sample pod name output)
NAME                         READY   STATUS    RESTARTS   AGE
vegbankdb-5d796654c4-wrdzf   1/1     Running   0          28h
vegbankdb-postgresql-0       1/1     Running   0          28h

~ % kubectl port-forward vegbankdb-5d796654c4-wrdzf 28015:80
```

Once the API pod is port-forwarded you can run the app in the R environment of your choice (RStudio, VsCode with the Shiny extension, and  seem to be popular options).

## License
```
Copyright [2024] [Regents of the University of California]

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

- DataONE Network
- Arctic Data Center: NSF-PLR grant #2042102 to M. B. Jones, A. Budden, M. Schildhauer, and J. Dozier

Additional support was provided for collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)
