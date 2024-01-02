# tidymmm

Tidy tools and extensions for MMM (Media/Marketing Mix Modeling)

## Motivation

This package has two main objectives

1.  Extend the tidymodels modeling framework with processioning steps required for MMM
    1.  Adstock & Carryover Transformations
    2.  Saturation
2.  Provide pre/post modeling tools for MMM/MMO projects.
    1.  Model Evaluation
    2.  Model Tuning
    3.  Media/Marketing Optimization
    4.  Visualizations

## Why Tidymodels and Tidymmm

Tidymmm takes inspiration from

-   A few great projects that have been developed in the recent past (see related work below).

    -   Modeling methodology and design from these projects (and research behind them) are used as inspiration, however there is no objective of re-implementing thier exact functionality.

    -   The biggest difference is that tidymmm aims to provide modelers with felexible tools (a la tidymodels philosophy) and not lock users into a specific method. The disadvantage is that tidymmm is less automated out the box, but on the other hand has all the flexibility that comes with tidymodels.

-   The Tidymodels infrastructure in the form of packages like `parsnip` (modeling), `recipes` (data prepossessing), `tune` (hyper-parameter optimization), `workflows` (combining everything together).

    -   The aim of tidymmm is to expend and tap into these tools + provide MMM specific tools implemented the tidy-way.

## Related Work

-   [LightweightMMM by Google](https://github.com/google/lightweight_mmm/tree/main?tab=readme-ov-file)

-   [Robyn by Meta](https://github.com/facebookexperimental/Robyn)

-   [PyMC-Marketing by PyMC Labs](https://www.pymc-marketing.io/en/stable/guide/mmm/comparison.html)
