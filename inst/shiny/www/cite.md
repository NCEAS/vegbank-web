# Cite or Link to VegBank

## Why Cite or Link?

One of VegBank's strengths is that its data are permanently archived. This means that you can cite a dataset or plot in VegBank and others will be able to follow your citation hyperlink to see the same data you saw at the time of citation. This is critical for analytical transparency. In addition, to give credit to the VegBank system, you might consider citing the VegBank software as a key tool in your workflows. Below we provide recommended citations for the various things you might cite in VegBank.

* * * 

## Citing a Dataset in VegBank with a DOI

Researchers can create a Vegbank Dataset (a specific named collection of plot observations) using the [create_dataset()](https://nceas.r-universe.dev/vegbankr/doc/manual.html#vb_create_dataset) API function, which in turn mints a citation-friendly DOI for the dataset.

To cite a dataset that is created in VegBank, it is best to use the DOI url for the dataset, which you can get from the 'Copy Citation' button in the web interface. The will produce a citation of the form:

> Kyle Palmquist (2013): VegBank plot observations: Carolina Vegetation Survey Longleaf Analysis. VegBank. Dataset. [doi:10.82902/ZZZZZZZZ](https://doi.org/10.82902/ZZZZZZZZ).

Once you know the DOI, You can resolve the dataset using any of the following URL patterns:

- https://doi.org/doi:10.82902/ZZZZZZZZ
- https://vegbank.org/cite/doi:10.82902/ZZZZZZZZ
- https://identifiers.org/vegbank:doi:10.82902/ZZZZZZZZ

* * * 

## Linking to records in VegBank with a persistent URL

In addition to datasets, VegBank produces transparent permalinks to other types of records in the system, such as a single plot observation, community concept, or plant concept record. Each of these types of record has a unique **VegBank Code** that can be used to fashion a URI to cite that record. You can find the **VegBank Code** (the unique identifier for VegBank data) on the table and detail views of different entities.

Once you know the VegBank Code, use the following pattern to create a URL:

```
https://vegbank.org/cite/[VegBankCode]
```

Where `[VegBankCode]` is the VegBank Code of the plot or other data to cite.

### Examples

| Entity | Citation URL |
|------|--------------|
| Plot Observation | `https://vegbank.org/cite/ob.3736` |
| Community Concept | `https://vegbank.org/cite/cc.5728` |
| Plant Concept | `https://vegbank.org/cite/pc.48560` |

* * *

## Citing the VegBank Database and API

To cite the VegBank database and API service, use:

> Jim Regetz, Robert Shelton, Darian Gill, Matthew B. Jones, Dou Mok, Matthew Brooke, Rushiraj Nenuji, Jeanette Clark, Maggie Klope, Michael T. Lee, Robert K. Peet. 2026. **VegBank: the open-access vegetation plot database and API of the Ecological Society of America’s Panel on Vegetation Classification.** Version 2.1.1. *VegBank*. [doi:10.82902/J1WC7G](https://doi.org/10.82902/J1WC7G).

* * * 

## Citing the `vegbankr` client libary

If you made use of the R tooling in the `vegbankr` package, you can cite the software package as: 

> Jim Regetz, Matthew B. Jones, Rushiraj Nenuji, Jeanette Clark, Maggie Klope. 2026. **vegbankr: An R API package for the VegBank data system**. Version 1.0.0. *VegBank*. [doi:10.82902/J1MW28](https://doi.org/10.82902/J1MW28)

* * *

## Citing the VegBank Web application

To cite the web application that is used to search and display VegBank data, use: 

> Darian Gill, Jim Regetz, Matthew B. Jones, Michael T. Lee, Matthew Brooke, Robert K. Peet (2026) **VegBank Web: A Shiny Web App for Vegetation Plot Data**. Version 1.0.0. *VegBank*. [doi:10.82902/J1H59R](https://doi.org/10.82902/J1H59R)

* * * 

## Citing a Description of VegBank

To cite the original paper describing VegBank, please use the following citation:

> Peet, R.K., M.T. Lee, M.D. Jennings, & D. Faber-Langendoen. 2012. **VegBank - a permanent, open-access archive for vegetation-plot data.** *Biodiversity and Ecology* 4: 233-241.

