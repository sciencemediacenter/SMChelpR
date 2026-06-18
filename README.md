<div id="header" align="center">
  <img src="https://media.sciencemediacenter.de/static/img/logos/smc/smc-logo-typo-bw-big.png" width="300"/>

  <div id="badges" style="padding-top: 20px">
    <a href="https://www.sciencemediacenter.de">
      <img src="https://img.shields.io/badge/Website-orange?style=plastic" alt="Website Science Media Center"/>
    </a>
    <a href="https://lab.sciencemediacenter.de">
      <img src="https://img.shields.io/badge/Website (SMC Lab)-grey?style=plastic" alt="Website Science Media Center Lab"/>
    </a>
    <a href="https://twitter.com/smc_germany_lab">
      <img src="https://img.shields.io/badge/Twitter-blue?style=plastic&logo=twitter&logoColor=white" alt="Twitter SMC Lab"/>
    </a>
  </div>
</div>

# SMChelpR - Help functions in R for the Data Reports of the SMC. 

The Science Media Center's [Data Reports](https://www.sciencemediacenter.de/angebote?story_type=Data+%26+Facts) are produced as a Quarto document containing code as well as all text. For the projects written in R, key functions have been bundled into this R package. 

The package primarily contains two types of functions:

- Functions to retrieve the [data API](https://github.com/sciencemediacenter/DataCollection) of the SMC.
- Functions to create the graphics included in the report.

## Installation

Install from GitHub with [pak](https://pak.r-lib.org/):

```r
# default: hard dependencies only
pak::pak("sciencemediacenter/SMChelpR")

# also install suggested packages
pak::pak("sciencemediacenter/SMChelpR", dependencies = TRUE)
```

The default install covers the core functionality. The suggested packages
(`echarts4r`, `chromote`, `rsvg`) are only needed for `export_echart_png_html_svg()`
and are pulled in by `dependencies = TRUE`. Without them, that function stops
with a message telling you what to install (`chromote` needs a headless
Chrome/Chromium, `rsvg` needs the system library `librsvg`).
