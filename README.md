# A framework for capturing indirect impacts in site-level screening for biodiversity risks

Divya Narain<sup>a</sup>, Jacob Bedford<sup>b</sup>, Elspeth
Grace<sup>b</sup>, Alfred Muge<sup>b</sup>, Aime Rankin<sup>b</sup>,
Matt Jones<sup>b</sup>, Sebastian Dunnett<sup>b</sup>

<sup>a</sup>Smith School of Enterprise and the Environment, University
of Oxford

<sup>b</sup>UN Environment Programme World Conservation Monitoring
Centre, Cambridge, UK

> Site-based industrial operations such as mining, oil and gas
> extraction, and renewable energy development are associated with many
> direct and indirect impacts on biodiversity. Consideration of the full
> range of these impacts when selecting the Area of Influence (AoI) of a
> project is critical for effective biodiversity risk screening.
> Indirect impacts, however, are elusive despite often being more
> extensive than direct ones, both temporally and spatially. There is
> also limited clarity on the distinction between direct and indirect
> impacts, leading to the latter either being missed from screening
> analyses, or misclassified as direct impacts. Here we propose a
> definition of indirect impacts and a framework for incorporating them
> in risk and impact analyses. We define indirect impacts as those that
> are triggered by wider socio-economic and demographic changes induced
> by the project and not directly by project operations. Indirect
> impacts manifest through three pathways: increased access to intact
> ecosystems, increased in-migration and settlement, and increased
> viability of other economic activity. Literature on the exact spatial
> extent to which indirect impacts manifest is sparse. A range of
> factors, however, can act as predictors of the likelihood and
> intensity of indirect impacts and form the basis of a decision-making
> framework to select an AoI that captures them effectively.

This repository contains supporting code for a manuscript in review at
*Methods in Ecology and Evolution*. A link to this paper will be added
when/if accepted and published.

The processing script, *aoi\_decision\_layer.R* takes six data inputs to
produce a raster screening layer that supports the assessment of project
indirect impacts. To reproduce the analysis, researchers must first
acquire the six input datasets from source:

<table>
<colgroup>
<col style="width: 15%" />
<col style="width: 48%" />
<col style="width: 20%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr>
<th>Dataset</th>
<th>Link</th>
<th>Variant used</th>
<th>Copyright</th>
</tr>
</thead>
<tbody>
<tr>
<td>Global roadless areas</td>
<td><a
href="https://roadless.online/data/">https://roadless.online/data/</a></td>
<td>Global shapefile</td>
<td>Open Data Commons Open Database Licence (ODbL)</td>
</tr>
<tr>
<td>Global Human Settlement Model grid (GHS-SMOD)</td>
<td><a
href="https://human-settlement.emergency.copernicus.eu/download.php?ds=smod">https://human-settlement.emergency.copernicus.eu/download.php?ds=smod</a></td>
<td>1km in Mollweide projection</td>
<td>CC BY</td>
</tr>
<tr>
<td>Global gridded GDP data</td>
<td><a
href="https://zenodo.org/records/16741980">https://zenodo.org/records/16741980</a></td>
<td>2020 total GDP at 30arcsecs</td>
<td>CC BY</td>
</tr>
<tr>
<td>Travel time to cities</td>
<td><a
href="https://doi.org/10.6084/m9.figshare.7638134.v4">https://doi.org/10.6084/m9.figshare.7638134.v4</a></td>
<td>“travel_time_to_cities_9.tif”, i.e. settlements over 50,000</td>
<td>CC BY</td>
</tr>
<tr>
<td>Global human modification dataset</td>
<td><a
href="https://doi.org/10.5281/zenodo.14502572">https://doi.org/10.5281/zenodo.14502572</a></td>
<td>“HMv20240801_2022s_AA_300.tif”, i.e. all threats combined</td>
<td>CC BY</td>
</tr>
<tr>
<td>Critical Habitat</td>
<td><a
href="https://doi.org/10.34892/snwv-a025">https://doi.org/10.34892/snwv-a025</a></td>
<td>Basic</td>
<td>CC BY</td>
</tr>
</tbody>
</table>

The accompanying script, *aoi\_plot.R*, allows researchers to recreate
figures in the manuscript.

Output data from the manuscript are on Zenodo
[here](https://doi.org/10.5281/zenodo.16997584).
