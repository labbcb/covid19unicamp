# Dashboard

A partir do diretório `covid19unicamp/dashboard`.

Instalar pacotes necessários.

```bash
Rscript install_packages.R
```

- dplyr
- tidyr
- ggplot2
- shiny
- shinydashboard
- devtools
- [datacovidbr](https://github.com/freguglia/datacovidbr)

Iniciar aplicação Shiny.

```bash
R -e "shiny::runApp('.')"
```

Para mais informações veja <https://shiny.rstudio.com/articles/running.html>.

Publicar aplicação em <https://www.shinyapps.io>.

```bash
# TODO
```