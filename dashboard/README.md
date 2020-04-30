# Dashboard

A partir do diretório `covid19unicamp/dashboard`.

Instalar pacotes necessários.

```bash
Rscript install_packages.R
```

Iniciar aplicação Shiny.

```bash
R -e "rmarkdown::run(shiny_args = list(port = 8241))"
```

<http://localhost:8241>

Para mais informações veja <https://shiny.rstudio.com/articles/running.html>.