# CARING 2 · Coincidencias en encuestas

Aplicación web, hecha en R con Shiny, que convierte una base de datos en una red
interactiva de coincidencias. Es la nueva versión de
[caring.usal.es](https://caring.usal.es), del
[proyecto netCoin](https://sociocav.usal.es/blog/nca/) (Universidad de Salamanca).

En un mismo proyecto se puede:

1. **Datos.** Subir una base de datos tal como esté (SPSS, Stata, R, CSV, Excel o SAS),
   y renombrar, recodificar, marcar perdidos, cambiar tipos, crear variables con
   fórmulas y filtrar casos. El archivo original nunca se modifica: cada cambio queda
   en un historial que se puede deshacer y exportar como script de R.
2. **Explorar.** Ver tablas, gráficos y pruebas estadísticas de una o dos variables,
   con ponderación opcional.
3. **Redes.** Hacer redes de coincidencias, correlaciones, mapas factoriales, modelos
   log-lineales, regresión en red y respuesta múltiple, todo con
   [netCoin](https://CRAN.R-project.org/package=netCoin).
4. **Informes.** Montar diapositivas o informes con los resultados, en lienzo libre o
   por bloques, y exportarlos a PowerPoint, Word, PDF o HTML (con las redes
   interactivas).

Tiene cuentas de usuario (correo y contraseña), un modo invitado sin registro,
proyectos compartidos (lector/editor) y un panel de administración.

## Probarlo en tu ordenador

Necesitas **R ≥ 4.1** y, para exportar a PDF y poner las redes como imagen en
PowerPoint y Word, **Google Chrome** o Chromium.

Descarga el código con `git clone` o con el botón **Code → Download ZIP** de esta
página (y descomprímelo). Después, en una terminal:

```bash
git clone https://github.com/cristinacalvolopez/caring2.git
cd caring2
Rscript instalar.R          # instala los paquetes que falten (solo la primera vez)
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

O, desde la consola de R (RStudio, VS Code…), sitúate en la carpeta descargada y
ejecuta `iniciar.R`, que instala lo que falte y abre la aplicación en el navegador:

```r
setwd("ruta/donde/esta/caring2")   # la carpeta que has descargado
source("iniciar.R")
```

Se abre el navegador con la página de acceso. Para probar sin registrarte, pulsa
**Probar sin cuenta** y después **Usar datos de demostración**.

## Dónde se guardan los datos

Todo se guarda en la carpeta `storage/`, que no se sube a GitHub:

| Qué | Dónde |
|---|---|
| Usuarios, sesiones, proyectos, permisos y ajustes | `storage/caring.sqlite` |
| Archivos de cada proyecto | `storage/users/<usuario>/projects/<proyecto>/` |
| Correos (en local no se envían) | `storage/correo/` |

Las contraseñas se guardan cifradas con Argon2 (nunca en claro). Los proyectos de
invitado se borran solos a las 24 horas.

### Configuración (variables de entorno)

| Variable | Para qué | Por defecto |
|---|---|---|
| `CARING_STORAGE` | carpeta de datos | `./storage` |
| `CARING_DB` | archivo de la base de datos | `<storage>/caring.sqlite` |
| `CARING_MAX_MB` | tamaño máximo por archivo (MB) hasta que se cambie en Administración | `200` |
| `CARING_SECURE` | `true` si se sirve por https | `false` |
| `CARING_MAIL` | `console` guarda los correos en archivos; `smtp` los envía | `console` |
| `CARING_SMTP_SERVER` | servidor SMTP, p. ej. `smtps://smtp.usal.es:465` | — |
| `CARING_SMTP_USER` / `CARING_SMTP_PASSWORD` | credenciales SMTP (si hacen falta) | — |
| `CARING_MAIL_FROM` | remitente de los correos | — |
| `CARING_SMTP_SSL` | `force`, `try` o `no` | `force` |

## Pruebas automáticas

```bash
Rscript -e 'pkgload::load_all(quiet = TRUE); testthat::test_dir("tests/testthat")'
```

Las pruebas cubren usuarios y sesiones, importación de todos los formatos y
codificaciones (UTF-8, Latin-1/Windows, BOM, `;` con coma decimal, ñ y tildes en
nombres y categorías), el historial de cambios, el análisis exploratorio, los seis
tipos de red y la exportación a PowerPoint, Word, HTML y PDF.

## Estructura

```
R/             código de la aplicación (un archivo por módulo)
inst/app/www/  estilos, tipografía Roboto, logo y JavaScript (editor de informes)
inst/app/templates/  plantilla de PowerPoint 16:9
inst/extdata/  datos de demostración
tests/         pruebas automáticas
app.R          punto de entrada (shiny::runApp)
```

## Equipo

CARING es parte del [proyecto netCoin](https://sociocav.usal.es/blog/nca/)
(Análisis reticular de coincidencias) de la Universidad de Salamanca. Las redes se
calculan con el paquete [netCoin](https://CRAN.R-project.org/package=netCoin), de
Modesto Escobar, David Barrios, Carlos Prieto, Luis Martínez-Uribe, Pablo
Cabrera-Álvarez y Cristina Calvo-López.

**Contacto:** [gas@usal.es](mailto:gas@usal.es) ·
**Más sobre el proyecto:** https://sociocav.usal.es/blog/nca/

El CARING original contó con financiación de la Universidad de Salamanca, la Junta de
Castilla y León, el Fondo Social Europeo y el Ministerio de Economía y Empresa.

## Licencia

GPL-3.
