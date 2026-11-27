# Mise en place d’une chaîne de production de séries cvs-cjo

``` r

library("rjd3production")
```

Le package {rjd3production} est utile pour mettre en place une chaîne de
production de séries désaisonnalisées.

## Initialisation d’un environnement de travail

Avant de créer une chaîne de production, il faut créer un environnement
de travail sour forme de projet R. La fonction
[`init_env()`](https://inseefr.github.io/rjd3production/reference/init_env.md)
crée la structure par défaut suivante :

- un dossier `data/` : les données brutes
- un dossier `Workspaces/` : les workspaces
- un dossier `output/` : les séries, tableaux et graphiques en sortie
- un dossier `specs/` : les spécifications propres au workspace
  (régresseurs de calendrier, outliers…)
- un dossier `BQ/` : les bilans qualité et fichiers de décisions
- un fichier DESCRIPTION pour gérer les dépendances de notre projet
- un fichier `.lintr` pour faire l’analyse statique du code (bonnes
  pratiques de formatage)
- un fichier README.md pour décrire le processus et la structure du
  projet
- une structure de projet Git

``` r

path_project <- tempfile(pattern = "my_sa_project")
init_env(path = path_project)
```

Dans cette vignette, nous allons créer une chaîne de production à partir
du jeu de données ABS du package {rjd3toolkit}.

Le jeu de données se trouve aussi sous la forme du fichier `ABS.csv`
sous /home/runner/work/\_temp/Library/rjd3providers/extdata/ABS.csv dans
le package {rjd3providers}.

``` r

library("rjd3toolkit")
path_ABS <- system.file("extdata", "ABS.csv", package = "rjd3providers")
my_data <- ABS[, seq_len(3L)]
colnames(my_data) <- substr(colnames(my_data), start = 2L, stop = 12L)
```

## Sélection des régresseurs de calendrier

Dans le cas où les séries sont sensibles aux effets de calendrier, on
peut corriger ces effets avec des régresseurs de calendrier.

Pour cela, consulter la vignette `td-selection` expliquant comment
selectionner le jeu de regresseur optimal pour chaque série et comment
produire la table `td` décrivant la correspondance entre les séries et
le jeu de régresseurs de calendrier selectionné.

``` r

td <- select_td(my_data)
#> 
#> Série 0.2.09.10.M en cours... 1/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
#> 
#> Série 0.2.08.10.M en cours... 2/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
#> 
#> Série 0.2.07.10.M en cours... 3/3 
#> Computing spec No_TD ...Done !
#> Computing spec REG1 ...Done !
#> Computing spec REG2 ...Done !
#> Computing spec REG3 ...Done !
#> Computing spec REG5 ...Done !
#> Computing spec REG6 ...Done !
#> Computing spec LY ...Done !
#> Computing spec REG1_LY ...Done !
#> Computing spec REG2_LY ...Done !
#> Computing spec REG3_LY ...Done !
#> Computing spec REG5_LY ...Done !
#> Computing spec REG6_LY ...Done !
```

## Création d’un workspace

Nous utiliserons le package {rjd3workspace} pour les fonctions de
création et manipulation des workspaces.

``` r

library("rjd3workspace")
```

Pour créer un nouvel workspace, vous pouvez soit en créer un vide et le
remplir, soit le créer directement à partir d’ ensemble de données
brutes.

Si vous utilisez des variables externes, des régresseurs ou un
calendrier personnalisé, veillez à les placer tous dans un
`modelling context`.

À l’INSEE, nous utilisons la fonction
[`create_insee_context()`](https://inseefr.github.io/rjd3production/reference/insee_modelling.md)
pour créer nos `contextes` :

``` r

my_context <- create_insee_context(s = my_data[, 1L])
```

Nous utiliserons le package {rjd3x13} pour créer les spécifications X13.

``` r

library("rjd3x13")
```

- Option 1 : créer un workspace vide et le remplir

``` r

jws <- jws_new(modelling_context = my_context)
jsap <- jws_sap_new(jws, "Nouveau SAP")
add_sa_item(
    jsap = jsap,
    name = "Première série",
    x = my_data[, 1L],
    spec = x13_spec()
)
add_sa_item(
    jsap = jsap,
    name = "Seconde série",
    x = my_data[, 2L],
    spec = x13_spec()
)
#... avec autant de commandes que de séries
```

- Option 2 : Créer un workspace à partir d’un dataset

``` r

jws <- create_ws_from_data(my_data)
set_context(jws, create_insee_context(s = my_data))
```

Si on souhaite utiliser des régresseurs de calendrier, il faut assigner
le jeu sélectionné au préalable à chaque série :

``` r

jws_compute(jws)
assign_td(td = td, jws = jws, spec_type = c("Estimation", "Reference"))
#> Série 0.2.09.10.M, 1/3
#> Série 0.2.08.10.M, 2/3
#> Série 0.2.07.10.M, 3/3
```

Ci-dessus les regresseurs sont assignés dans la spécification de
reference (Reference Spec) et dans la spécification estimation
(Estimation Spec). Ces concepts sont expliqués
[ici](https://prod.jdemetra.org/p1-overview#sec-concept-specification)
et [ici](https://prod.jdemetra.org/p4-appendices#comp-specs).

Il ne faut pas oublier de mettre à jour les metadonnées du workspace
avec le chemin vers les données brutes :

``` r

add_raw_data_path(jws, path_ABS, delimiter = "COMMA")
```

Enfin, nous pouvons sauvegarder notre workspace !

Celui-ci va cesser d’être juste un objet java virtuel pour devenir un
ensemble de fichiers (xml).

``` r

path_ws <- file.path(path_project, "Workspaces", "workspace_travail", "my_ws.xml")
save_workspace(jws, path_ws, replace = TRUE)
```

## Utilisation du Cruncher pour l’estimation et la pproduction de l’output

Lorsque l’on utilise une structure de workspace (interêt : avoir des
données lisibles avec l’interface graphique), le
[Cruncher](https://doc.jdemetra.org/t-production-tools-cruncher-qr)
permet d’automatiser deux opérations importantes :

1.  Mettre à jour les données brutes à partir du fichier de données

2.  Actualiser les données désaisonalisées en tenant compte des nouveaux
    points brutes et/ou de la révision du passé récent. On peut choisir
    une politique de rafraichissement pour mettre à jour le modèle
    d’ajustement saisonnier de manière plus ou moins contrainte. Pour
    plus de détails voir la
    [documentation](https://doc.jdemetra.org/a-rev-policies).
