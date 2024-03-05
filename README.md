[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/U2FK1DPE)
# DST 490, Day 14: More work using shiny

Today we are working on options for extending your knowledge on incorporating interactive data graphics with shiny.

Want to learn more about shiny and beyond? 
- [Mastering Shiny](https://mastering-shiny.org/)
- [htmlwidgets](https://www.htmlwidgets.org/index.html)
- [htmlwidgets gallery](https://gallery.htmlwidgets.org/)
- [quarto dashboards](https://quarto.org/docs/dashboards/)
- [quarto dashboards examples](https://quarto.org/docs/dashboards/examples/)
- [quarto websites](https://quarto.org/docs/websites/)

The YOUR TURN today will have you cloning a repo, of which you will add collaborators.  Information on how to do that is here [LINK]https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-access-to-your-personal-repositories/inviting-collaborators-to-a-personal-repository#inviting-a-collaborator-to-a-personal-repository).

## Examples in this repository


### `app.R`
Example shiny app deployment through github.  In order to run this you need:

`shiny::runGitHub(repo = "DST490-day14",username = "AugsburgDST490")`

**NOTE:** When you create your own github shiny server:

- Repository visibility needs to be set as public.
- Main file needs to be named `app.R`.

Other examples of sharing work with shiny: [LINK](https://shiny.posit.co/r/getstarted/shiny-basics/lesson7/)

### `day14-shiny-example1.R`
Minimal example of Beatles name plot using `plotly`.

- [Information about plotly](https://plotly.com/r/)
- [Using plotly with R (book)](https://plotly-r.com/)

### `day14-shiny-example2.R`
Minimal example of Beatles name plot using `dygraphs`

- [Information about dygraphs](https://rstudio.github.io/dygraphs/index.html)
- [Information about dygraphs and shiny](https://rstudio.github.io/dygraphs/shiny.html)

### `day14-shiny-example3.qmd`
Minimal example of Hennepin County zipcodes with `leaflet`

 - [Show / hide layers](https://rstudio.github.io/leaflet/articles/showhide.html)
 - [Using leaflet with shiny](https://rstudio.github.io/leaflet/articles/shiny.html)

