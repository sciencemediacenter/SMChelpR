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

The Science Media Center's [Data Reports](https://www.sciencemediacenter.de/alle-angebote/suchergebnis/?tx_solr%5Bfilter%5D%5B1%5D=type%3AData+Report) are produced as a Quarto document containing code as well as all text. For the projects written in R, key functions have been bundled into this R package. 

The package primarily contains two types of functions:

- Functions to retrieve the [data API](https://github.com/sciencemediacenter/DataCollection) of the SMC.
- Functions to create the graphics included in the report.

The package can be easily installed using `devtools`:

`devtools::install_github("sciencemediacenter/SMChelpR")`
