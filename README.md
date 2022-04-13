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
                          runno = "5",
                          bootstrap = TRUE,
                          run_dir.boot = file.path(system.file(package = "pmxpartabc"), "examples", "bootstrap_dir2"),
                          runno.boot = "5boot",
                          conf.level = 0.95, 
                          min_suc = TRUE)

    df <- parframe(out=output[[1]], meta=output[[2]], bootstrap = TRUE)  

    table <- pmxpartab(df, 
                       meta = output[[2]],
                       columns=c(value="Estimate", boot.median = "Bootstrap Estimate", rse="RSE%", ci95="95%CI", shrinkage="Shrinkage")
                       )

    print(table)

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
Bootstrap Estimate
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
<td class="paramsectionheading">
</td>
</tr>
<tr>
<td class="paramlabelindent">
Central Volume (L)
</td>
<td>
24.7
</td>
<td>
24.9
</td>
<td>
8.29
</td>
<td>
21.7 – 26.8
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
0.669
</td>
<td>
249
</td>
<td>
0.303 – 0.759
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Peripheral Volume (L)
</td>
<td>
76.5
</td>
<td>
79.4
</td>
<td>
195
</td>
<td>
68.4 – 115
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Intercompartimental Clearance (L/h)
</td>
<td>
1.83
</td>
<td>
1.89
</td>
<td>
34.7
</td>
<td>
1.83 – 1.95
</td>
<td>
</td>
</tr>
<tr>
<td class="paramlabelindent">
Absorption Rate (h-1)
</td>
<td>
0.653
</td>
<td>
0.636
</td>
<td>
52.7
</td>
<td>
0.559 – 0.790
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
<td class="paramsectionheading">
</td>
</tr>
<tr>
<td class="paramlabelindent">
On Volume
</td>
<td>
0.169
</td>
<td>
0.169
</td>
<td>
22.2
</td>
<td>
0.0999 – 0.219
</td>
<td>
0.300%
</td>
</tr>
<tr>
<td class="paramlabelindent">
Correlation On Clearance,On Volume
</td>
<td>
0.140
</td>
<td>
0.139
</td>
<td>
199
</td>
<td>
0.0928 – 0.336
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
0.368
</td>
<td>
282
</td>
<td>
0.234 – 1.62
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
0.104
</td>
<td>
147
</td>
<td>
0.0977 – 0.115
</td>
<td>
20.1%
</td>
</tr>
</tbody>
</table>
