# Dashboard

A partir do diretório `covid19unicamp/dashboard`.

Instalar pacotes necessários.

```bash
Rscript install_packages.R
```

- dplyr
- tidyr
- ggplot2
- highcharter
- shiny
- flexdashboard
- brazilmaps
- sf
- leaflet
- geobr
- devtools
- [datacovidbr](https://github.com/freguglia/datacovidbr)

Iniciar aplicação Shiny.

```bash
R -e "rmarkdown::run(shiny_args = list(port = 8241))"
```

<http://localhost:8241>

Para mais informações veja <https://shiny.rstudio.com/articles/running.html>.

Publicar aplicação em <https://www.shinyapps.io>.

```bash
# TODO
```