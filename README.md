COVID-19 en España
================

<!-- README.md es generado desde README.Rmd. Por favor edita ese archivo -->

Gráfico no interactivo que muestra la evolución de los casos de COVID-19
en España y sus provincias, basado en el trabajo realizado por [Rami
Krispin](https://ramikrispin.github.io/coronavirus/) y [Antoine
Soetewey](https://www.statsandr.com/blog/how-to-create-a-simple-coronavirus-dashboard-specific-to-your-country-in-r/).

Puedes ver el resultado en: <https://ubgus.github.io/covid19_es/>

## Github Pages

Para generar el archivo html a utilizar en Github Pages, puedes usar la
función `rmarkdown::render_site()`.

## Fuentes

Datos obtenidos de las publicaciones oficiales de:

  - Centro de Coordinación de Alertas y Emergencias Sanitarias
    ([CCAES](https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm)).
    <br>
  - Instituto de Salud Carlos III ([ISCIII](https://www.isciii.es)) y su
    portal sobre el [coronavirus](https://covid19.isciii.es/). <br>
  - Johns Hopkins University Center for Systems Science and Engineering
    ([JHU CCSE](https://github.com/CSSEGISandData/COVID-19)). <br>
  - El proyecto [Our World in
    Data](https://ourworldindata.org/coronavirus).

### Aclaratorias:

  - **29/04/2020**: Inicialmente recogía los datos de España desde las
    actualizaciones diarias del Ministerio de Sanidad en formato `.pdf`,
    sin embargo, dada la alta variabilidad en el formato de los
    archivos, además de la variabilidad de los datos por las correciones
    historicas que realiza frecuentemente el ministerio, decide buscar
    otras fuentes.<br> Gracias a los compañeros de
    [DATADISTA.COM](https://github.com/datadista/datasets), me di cuenta
    de que el portal del ISCIII ahora se encargaba de publicar el
    histórico corregido, por lo que ahora los datos son descargados
    directamente desde dicho
    [repositorio](https://covid19.isciii.es/).<br> Finalmente, también
    decidí incorporar información del grupo Our World in Data, que a su
    vez recoguen datos de distintas fuentes, y principalmente del
    [European
    CDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide).
