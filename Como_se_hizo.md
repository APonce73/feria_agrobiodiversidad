**¿Qué y cómo se hizo?**

*Diseño de la encuesta*

El impacto en la conservación de la agrobiodiversidad en la Feria Estatal de Agrobiodiversidad de Oaxaca, se abordó mediante dos preguntas principales:
¿Cuál es la diversidad de plantas manejadas y productos transformados que llevan los campesinos a la feria? (encuesta de agrobiodiversidad)
¿Cuántos intercambios de semillas realizan los campesinos durante la feria, así como en ferias pasadas? (encuesta de intercambios)
Dichas preguntas fueron el eje rector para elaborar y ejecutar encuestas semiabiertas a uno o dos campesinos o campesinas por comunidad. También, se levantó información general de: comunidad, municipio, estado, edad del encuestado y variedades de maíces, chiles, calabazas y frijoles. Las encuestas se diseñaron en las plataformas de libre acceso KoBoToolbox (https://www.kobotoolbox.org/) y KoBo-Conabio (https://kobo.conabio.gob.mx) La mayoría de las preguntas que se elaboraron fueron de texto abierto y ya con la información se hizo un proceso de limpieza de los datos, posteriormente se fueron generando vocabularios controlados que se fueron adaptando en las encuestas de los años posteriores; esta modificación ayudó a obtener información más clara y facilitar el trabajo de la persona realizando la encuesta. 

*Capacitación para el levantamiento de las encuestas*

Para la aplicación de las encuestas se capacitó a estudiantes de nivel medio y superior del Instituto Tecnológico Superior de Valles Centrales y del Centro Regional Universitario Sur de la Universidad Autónoma de Chapingo en el uso de las plataformas mencionadas y los objetivos de la encuesta. Los y las estudiantes instalaron la aplicación de KoBo Collect en sus tabletas o teléfonos con sistema ‘Android’ y cargaron las encuestas. A quienes no tenían teléfono propio se les asignó una tableta de CONABIO. Esta capacitación fue de un día, previo al evento de la feria de agrobiodiversidad.
Aplicación de las encuestas en la feria en la feria de agrobiodiversidad
Las encuestas fueron aplicadas en las Ferias anuales del 2016 al 2019 con un promedio de 130 personas, de las comunidades campesinas, encuestadas por feria (de una a dos personas por comunidad). La encuesta de agrobiodiversidad en la feria se empezó a levantar alrededor de las 11:00 hr. cuando los campesinos y campesinas ya estaban establecidos en la feria. Por otro lado, la encuesta de intercambios se realizó antes de la comida (alrededor de las 13:00 hr.), dado que después los y las campesinos se dispersan a lo largo de la feria dificultando la aplicación de la encuesta.

*Limpieza, análisis y visualización de las encuestas*
Los resultados de las encuestas de cada año se bajaron de las plataformas de KoBoToolbox o KoBo-Conabio en formato de archivo de Excel. Se hizo una limpieza de los datos previo a su análisis con los paquetes de R (R Core Team, 2019): ‘readxl’ (Wickham y Bryan, 2019), ‘tidyverse’ (Wickham et al., 2019), ‘dplyr’ (Wickham et al., 2021), ‘tidyr’ (Wickham, 2021) y ‘stringr’ (Wickham, 2019). Para la visualización de los datos se utilizó ‘ggplot2’ (Wickham, 2016), ‘ggrepel’ (Slowikowski, 2021), ‘ggthemes’ (Arnold, 2021). Toda la limpieza, análisis y visualización se realizó en R. Los scripts están disponibles en https://github.com/APonce73/feria_agrobiodiversidad.

Referencias 
Arnold, J.B. 2021. ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.4. https://CRAN.R-project.org/package=ggthemes
R Core Team. 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
Slowikowski, K. 2021. ggrepel: Automatically Position Non-Overlapping Text Labels with  'ggplot2'. R package version 0.9.1. https://CRAN.R-project.org/package=ggrepel
Wickham, H. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
Wickham, H., J. Bryan. 2019. readxl: Read Excel Files. R package version 1.3.1. https://CRAN.R-project.org/package=readxl
Wickham et al. 2019. Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686.
Wickham, H. François R., Henry L. y K. Müller. 2021. dplyr: A Grammar of Data Manipulation. R package version 1.0.5. https://CRAN.R-project.org/package=dplyr.
Wickham H. 2021. tidyr: Tidy Messy Data. R package version 1.1.3. https://CRAN.R-project.org/package=tidyr.
Wickham H. 2019. stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr 
