# VegBank FAQ {#topoffaq}

Frequently Asked Questions about the VegBank database and website.

Please select from the following questions, or browse through this file:  

> [What is VegBank?](#whatisvegbank)  
> [Who runs VegBank?](#whoruns)  
> [What is a plot?](#whatisplot)  
> [What is a community?](#whatcommunity)  
> [What is a plant concept? What is a community concept?](#whatconcept)  
> [Where does VegBank get its plots and other data?](#whereplotsfrom)  
> [What is your standard list of species/communities?](#standardtaxa)  
> [How do I find plots in VegBank?](#howsearch)  
> [How do I download VegBank data to my computer?](#howdownload)  
> [What features does VegBank offer for analysis of plot data?](#analysis)  
> [What is metadata?](#metadata)  
> [How do I cite VegBank?](#cite)  
> [What is a VegBank Code?](#vegbankcode)  
> [How is VegBank financed? How can I help VegBank operate financially?](#donate)
> [How can I get help with VegBank? Is there a tutorial?](#tutorial)  
> [What if I find something not working in VegBank?](#bug)  

* * *

##### What is VegBank? {#whatisvegbank}

> VegBank is the vegetation plot database of the Ecological Society of America's [Panel on Vegetation Classification](https://esa.org/vegpanel/). VegBank stores data about vegetation plots and necessary supplemental data, such as a plant taxon database and community type database. VegBank's purpose is to allow plant ecologists to submit and share data to allow permanent documentation of plot data, which will provide a permanent record of plots which define communities.

[back to top...](#topoffaq)

* * *

##### Who runs VegBank? {#whoruns}

> VegBank is operated by the [National Center for Ecological Analysis and Synthesis](http://www.nceas.ucsb.edu/) in cooperation with the Ecological Society of America's [Vegetation Panel](https://esa.org/vegpanel/).

[back to top...](#topoffaq)

* * *

##### What is a plot? {#whatisplot}

> The purpose of plots is to record the vegetation and its environmental context. A plot may be contained by a single bounded area, such as a 50m x 20m rectangle. It could also consist of smaller subplots which sample vegetation over a large range. While we encourage plots to have boundaries and definite size, VegBank also accepts relevés that have no bounds as valid plots. An observation is a one-time record of vegetation present in a plot. So multiple observations of a plot could be made at different times, producing different values for both vegetation (i.e., which taxa are present, and their abundance if recorded) and environmental variables (e.g., soil characteristics).

[back to top...](#topoffaq)

* * *

##### What is a community? {#whatcommunity}

> In overly simple terms, a community is a set of plant taxa that live adjacent to one another within a set of physical conditions. This community of co-occurring plants may be seen to repeat itself in a predictable pattern. One may classify (or interpret) a plot as belonging to a certain community type.  
>   
> The [Guidelines of the NVC (National Vegetation Classification) Standards document (version 3)](http://vegbank.org/vegdocs/panel/NVC_guidelines_v3.pdf) provides more full definitions for two different levels of communities: association and alliance. It defines an **association** "as the basic unit of vegetation: A vegetation classification unit defined on the bases of a characteristic range of species composition, diagnostic species occurrence, habitat conditions and physiognomy." (pg. 21)  
> The broader **alliance** it defines as "A vegetation classification unit containing one or more associations, and defined by a characteristic range of species composition, habitat conditions, physiognomy, and diagnostic species, typically at least one of which is found in the uppermost or dominant stratum of the vegetation." (pg.23)  
>   
> Communities are Concept-Based in VegBank, meaning they are defined by a combination of a name and a reference. See the [What is a Concept?](#whatconcept) FAQ Topic for more information on Concepts.

[back to top...](#topoffaq)

* * *

##### What is a plant concept? What is a community concept? {#whatconcept}

> Generally, people use only a plant name to refer to a particular plant taxon. However, names may mean different things to different people or at different times. When users view a plant in VegBank, they need to know what is meant by a plant name, hence only a name is not enough. A reference, or context, must be provided which gives the name meaning and defines what the taxon encompasses. When we mention a "plant concept" in VegBank, we mean this unique combination of name and reference. For example, one might mention "Carya ovata" according to (reference) "Radford, 1968." Each plant concept and community concept in VegBank has a VegBank Code which provides unique identification for the plant concept. For more information, please see the [VegBank Plant Taxonomy Overview](http://vegbank.org/vegdocs/design/planttaxaoverview.html).  
> Community Concepts function in exactly the same way, except that Community Concepts describe a community type, not a plant taxon.

[back to top...](#topoffaq)

* * *

##### Where does VegBank get its plots and other data? {#whereplotsfrom}

> Plots in VegBank come from the users who decide to **contribute their plots**. VegBank does not fund plot collection, but is a means for those who do collect plots to share their data. There are two major implications of this:  
> 1) We do not guarantee the accurateness of the data in VegBank. Users are allowed to annotate (make notes and reinterpret plants and plots) to share with other users their opinions about the data already in VegBank. \[Some of the annotation features are still in development.\]  
> 2) We need users to share their data. There are many different reasons users share their data. Some are required to share their data by their grants, but don't want to deal with the hassle of setting up computer servers and software to manage this. Some use VegBank as a means of collaborating with others in data analysis. Others have finished analysis and are eager to find a use for their hard work other than being filed away to collect dust. Still others see the foresight in adding their data to the greater body of knowledge on a public site like VegBank.  
>   
> Other data, like plants and communities, come from VegBank's users, but rely heavily on VegBank users like the organizations [USDA Plants](http://plants.usda.gov/) and [NatureServe](http://natureserve.org/) to provide this data.

[back to top...](#topoffaq)

* * *

##### What is your standard list of species/communities? {#standardtaxa}

> VegBank does not have a standard list of taxa or communities. In VegBank, plants and communities have a Party Perspective which defines whether a plant or community is standard or not and what names may be used to describe it. VegBank has no Party Perspective of its own. However, USDA PLANTS 2002 data and NatureServe communities were initially loaded into the database and users are strongly encouraged to map any new plants or communities onto USDA's or NatureServe's standards. This allows other users, who may know nothing of your own views, to view information according to the USDA or NatureServe standards.

[back to top...](#topoffaq)

* * *

##### How do I find plots in VegBank? {#howsearch}

> You can search for plots on the plot tab:  
>   
> In the top right of the plot table is a very simple search box, where you can specify one or more of the following: plant found on the plot, community to which the plot is interpreted, state/province in which the plot is located, the code assigned by the author to the plot.  
>   
> A more complex query is in the works...  
>   

[back to top...](#topoffaq)

* * *

##### How do I download VegBank data? {#howdownload}

> Data can be downloaded from the [vegbankr](https://github.com/NCEAS/vegbankr) R package, which underpins this website and serves as an interface to the VegBank API.

[back to top...](#topoffaq)

* * *

##### What features does VegBank offer for analysis of plot data? {#analysis}

> VegBank does not have analysis tools. Other groups have done a good job making tools for analysis, and we encourage users to find data in VegBank, then export into a format of your choice, then work on analysis with a program like PC-Ord.

[back to top...](#topoffaq)

* * *

##### What is metadata? {#metadata}

> Metadata is plot data that defines not what was measured, but how it was measured. Cover Method and Stratum Method are examples of metadata. Plot size and stem sampling method might also be considered metadata. These data are often considered secondary in importance, but are essential when many different techniques are used to collect plots, as is the case with VegBank data.

[back to top...](#topoffaq)

* * *

##### How do I cite VegBank? {#cite}

> You may cite VegBank, as a whole or one or more elements, with the instructions on the [Cite page](?tab=Cite) in the About menu. That page also shows how to cite a description of VegBank.

[back to top...](#topoffaq)

* * *

##### What is a VegBank Code? {#vegbankcode}

> A VegBank Code is a unique combination of letters, numbers, and a period which uniquely identify something in VegBank. VegBank Codes are used to identify plots and observations, as well as methods, plant taxa, and community types. This allows communication about these elements to take place so that each party knows that they are mentioning the same thing. VegBank Codes are important when vegbankr and VegBank communicate, because this helps the two align the different types of data, rather than repeating redundant information.  
>   
> For those who want to know how a VegBank Code works (this is not necessary!), it is broken into two parts, each divided by a period (.). An example VB Code is that of Quercus alba, with reference of USDA 2002 : pc.48413. The first part is the table identifier (i.e. pc for plant concepts), then the record number (i.e., 48413).

[back to top...](#topoffaq)

* * *

##### How is VegBank financed? How can I help VegBank operate financially? {#donate}

> We are currently funded by the **California Department of Fish and Wildlife (CDFW)**. We gratefully acknowledge the financial support of CDFW.
>   
> Eventually, we'd like to have a feature that allows you to donate to our efforts. Until then, if you have a funding source for VegBank, please contact us at help@vegbank.org. We'd be more than happy to hear about it.

[back to top...](#topoffaq)

* * *

## Learning More about VegBank {#catglearnmore}

##### How can I get help with VegBank? Is there a tutorial? {#tutorial}

> Not yet. We're working on an instructions page to help with this.

[back to top...](#topoffaq)

* * *

## Contacting VegBank Developers {#catgcontact}

##### What if I find something not working in VegBank? {#bug}

> You should tell us about what wasn't working. Just send an email to help@vegbank.org and explain what it is that isn't working. Please be as specific as possible, i.e. what the URL was when the error occurred, what the search criteria were if you were searching for something, the time and date the error occurred, etc. Users who report errors help us improve the VegBank experience for everyone. Thank you!

[back to top...](#topoffaq)

* * *

