# linproj: Linear Projection Methods

## Installation

The package can be installed directly from GitHub using `devtools`:
```R
if( require(devtools) == FALSE ) install.packages( "devtools" )
devtools::install_github( "ArtemSokolov/linproj" )
```
After the installation, the package can be loaded with a simple `library( linproj )` command.

## Linear Discriminant Analysis

For this example, we will work with the built-in `iris` dataset.
```R
head( iris )
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
```

There are several ways to train an LDA model. The `LDA()` function understands matrices, data.frames and formula objects. All of the following are equivalent ways to train an LDA model on the `iris` dataset. For more information, see `?LDA`.

```R
model <- LDA( iris, "Species" )
model <- LDA( iris, 5 )
model <- LDA( iris[,1:4], iris[,5] )
model <- LDA( as.matrix( iris[,1:4] ), iris[,5] )
model <- LDA( Species ~ ., iris )
model <- LDA( Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris )
```

Downstream analysis of the model can be performed using [tidy verbs](https://cran.r-project.org/web/packages/broom/vignettes/broom.html) from the `broom` package. Specifically, `linproj` implements the verbs `tidy`, `glance`, and `augment` for `LDA()` output. Use `tidy` to retrieve the loadings of LDA components

```R
broom::tidy( model )
#        Feature      LDA1        LDA2
# 1 Sepal.Length  0.626970 -0.01449191
# 2  Sepal.Width  1.181041 -2.14917074
# 3 Petal.Length -2.135565  0.52600987
# 4  Petal.Width -1.828151 -1.89227871
```
and `glance` to retrieve the corresponding eigenvalues:
```R
broom::glance( model )
#        VQ1       VQ2
# 1 23.42386 0.2167796
```

New data can be projected onto the LDA components using `augment`:
```R
head( broom::augment( model, iris ) )
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species     LDA1      LDA2
# 1          5.1         3.5          1.4         0.2  setosa 3.975771 -7.238048
# 2          4.9         3.0          1.4         0.2  setosa 3.259856 -6.160564
# 3          4.7         3.2          1.3         0.2  setosa 3.584227 -6.640101
# 4          4.6         3.1          1.5         0.2  setosa 2.976313 -6.318533
# 5          5.0         3.6          1.4         0.2  setosa 4.031178 -7.451516
# 6          5.4         3.9          1.7         0.4  setosa 3.629979 -8.322717
```
