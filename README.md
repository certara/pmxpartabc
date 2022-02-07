[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)

# pmxpartabc

An R package for parameter estimate tables.

------------------------------------------------------------------------

## Installation

This package is not yet on CRAN. To install the latest development
version directly from GitHub:

    require(remotes)
    devtools::install_github("certara/pmxpartabc")

## Usage

    library(pmxpartabc)

    output <- parframe2setup(run_dir = file.path(system.file(package = "pmxpartabc"), "examples"), 
                          run_prefix = "run", 
                          runno = "6", 
                          conf.level = 0.95, 
                          min_suc = TRUE, 
                          yaml.file = TRUE, 
                          yaml.file.name = file.path(system.file(package = "pmxpartabc"), "meta.yaml"))

    tab <- parframe(out=output[[1]], meta=output[[2]])  

    tab1 <- pmxpartab(tab, output[[2]], columns=c(value="Estimate", rse="RSE%", ci95="95%CI", shrinkage="Shrinkage"))

    print(tab1)

<table>
<thead>
<tr>
<th>
Parameter
</th>
<th>
Estimate
</th>
<th>
RSE%
</th>
<th>
95%CI
</th>
<th>
Shrinkage
</th>
</tr>
</thead>
<tbody>
<tr>
<td class="paramsectionheading">
Typical Values
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
</tr>
<tr>
<td class="paramlabelindent">
Central volume (L)
</td>
<td>
24.7
</td>
<td>
8.29
</td>
<td>
20.7 – 28.7
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Clearance (L/h)
</td>
<td>
0.659
</td>
<td>
249
</td>
<td>
-2.56 – 3.87
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Peripheral volume (L)
</td>
<td>
76.5
</td>
<td>
195
</td>
<td>
-216 – 369
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Peripheral clearance (L/h)
</td>
<td>
1.83
</td>
<td>
34.7
</td>
<td>
0.587 – 3.08
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Absorption rate constant (h<sup>-1</sup>)
</td>
<td>
0.653
</td>
<td>
52.7
</td>
<td>
-0.0211 – 1.33
</td>
<td>
</td>
</tr>
<tr>
<td class="paramsectionheading">
Between Subject Variability
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
</tr>
<tr>
<td class="paramlabelindent">
On Central volume
</td>
<td>
0.169
</td>
<td>
22.2
</td>
<td>
0.0955 – 0.242
</td>
<td>
0.300%
</td>
</tr>
<tr>
<td class="paramlabelindent">
Correlation on Clearance, on Central Volume
</td>
<td>
0.140
</td>
<td>
199
</td>
<td>
-0.406 – 0.687
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
On Clearance
</td>
<td>
0.358
</td>
<td>
282
</td>
<td>
-1.62 – 2.34
</td>
<td>
1.30%
</td>
</tr>
<tr>
<td class="paramsectionheading">
Residual Error
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
<td class="paramsectionheading">
</td>
</tr>
<tr>
<td class="paramlabelindent">
Proportional Error (-)
</td>
<td>
0.102
</td>
<td>
147
</td>
<td>
-0.191 – 0.396
</td>
<td>
20.1%
</td>
</tr>
</tbody>
</table>
