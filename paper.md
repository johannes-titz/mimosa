---
title: 'mimosa: A Modern Graphical User Interface for 2-level Mixed Models'
tags:
  - mixed models
  - hierarchical linear models
  - shiny
  - R
authors:
  - name: Johannes Titz
    orcid: 0000-0002-1102-5719
    affiliation: 1
affiliations:
 - name: Department of Psychology, TU Chemnitz, Germany
   index: 1
date: 10 February 2020
bibliography: My Library.bib
---

# Summary
``Mimosa``, the mixed models special agent, is a shiny app for 2-level mixed models. Mixed models are rapidly becoming the gold standard of statistical analysis techniques in the behavioral sciences. At the same time there exist few user-friendly software to conduct mixed model analyses. The most common tools often lack a graphical user interface, are proprietary, and involve a tedious process of getting data in and publication-ready tables out. ``Mimosa`` is supposed to offer an alternative that is free, open source, intuitive, and runs in a browser, making it easily accessible.

``Mimosa`` is targeted at behavioral scientists who frequently use 2-level mixed models and want a tailored solution for this specific use case. For instance, researchers studying groups (e.g. students clustered in schools, individuals clustered in work groups) and researchers employing within-subjects designs almost exclusively analyze their data with 2-level mixed models. In contrast to other software, ``mimosa`` was designed for this use case. It helps the analyst by automatically detecting potential grouping variables and categorizing variables in level 1 and level 2. Furthermore, ``mimosa`` is researcher-oriented because it produces a single summary table via sjPlot [@ludecke2019] that can be published in a scientific journal without any modifications.

To my knowledge, there are only few free, open source, software packages for mixed models that have a graphical user interface. For a slightly outdated comparison of different mixed model software (including proprietary and command-line software) see @west2012. ``LMMgui`` focuses on within-subjects-designs and as such can be seen as a direct competitor of ``mimosa``. ``LMMgui`` offers diagnostic plots and can compare two models, which is missing in ``mimosa``. Disadvantages of ``LMMgui`` are that it is only available for Windows, while ``mimosa`` runs on any platform because it is a browser application. Furthermore, ``mimosa`` has a cleaner interface, is easier to use and produces a publication-ready output.

The ``GAMLj`` [@gallucci2020] module for ``jamovi`` [@thejamoviproject2019] is a more comprehensive tool for mixed model analysis. It is possible to model more than two levels and specify more complex models in general. ``GAMLj``'s functionality is close to the command line interface of ``lme4`` [@Bates2015]. This is a clear benefit, but it also comes with a downside: the interface of ``GAMLj`` is less clean than the interface of ``mimosa``. ``GAMLj`` has many dialoges and the specification of a model is not as intuitive as in ``mimosa``. Still, ``GAMLj`` is an almost perfect software for mixed models in general. On the other hand ``mimosa`` follows the idea to do one type of analysis well that many behavioral reseachers do most of the time: 2-level mixed models.

In contrast to software packages I am aware of (including the ones discussed by @west2012), ``mimosa`` is the only one running in a browser. While this might be seen as only a minor advantage, it has clear benefits. There is no need to install or update ``mimosa`` or its dependencies and the application can be used at any place with internect connection---for instance at a colleague's office who does not have ``jamovi`` and ``GAMLj`` installed, in the classroom, or at a conference presentation for a live analysis.

# Acknowledgements
I want to sincerely thank Markus Burkhardt, Karin Matko, Thomas Schäfer, Peter Sedlmeier, and Isabell Winkler for testing mimosa and giving helpful comments on the documentation.

# References
