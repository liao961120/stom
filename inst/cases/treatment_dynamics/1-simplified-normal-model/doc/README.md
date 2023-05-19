---
title: "A Growth Curve Model of Latent Constructs"
author: Yongfu Liao
format: 
   gfm:
      output-file: "index"
      output-ext: "md"
      variant: +yaml_metadata_block+raw_html
      df-print: tibble
bibliography: ref.bib
---

<!--
```{r setup, include=FALSE}
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	#results = 'hold',
	out.width = "100%",
	fig.align = 'center',
	comment = "",
	fig.dim = c(10, 5.5),
   dev='svglite',
   dev.args = list(bg = "transparent"),
   NULL
)
```
-->

Goal
-----

Studies in psychology and the social sciences in general often deploy
questionaires and surveys to measure latent constructs. Under these
circumstances, item response models should be adopted. Instead of simply summing
the item scores, item response models explicitly deal with the uncertainty in
the participants' responses and hence consider measurement errors during the
process. The estimated effects of the latent variables from IRT models are thus
much more rigorous than those overconfident raw sum scores of the items.

IRT models are well-developed but overly neglected in the field. This largely
arises from most researchers not being aware of the possiblity of incorporating
IRT models into a larger network of causes and effects. It is hard to achieve
this and not even possible in most existing statistical software packages.
A Bayesian framework is required. Below, I demonstrate one such application of
incorporating IRT models into a longitudinal growth curve model, in the context
of substance abuse treament.


Background
----------

The model constructed here is largely inspired by the study of @bowen2014 and a
secondary analysis on the data collected by the former [@moniz-lewis2022]. The
context of the studies is the evaluation of three kinds of treatments on
substance abuse. Participants were recruited and randomly assigned to one of the
three treatment conditions. Before (baseline) and after the treatment,
information of treatment outcome (amount of heavy drinking and drug use) and
self-efficacy on alcohol/drug control (measured through questionary) were
collected. There were four such evaluations for the participants, including the
baseline, thus resulting in a set of longitudinal data of four time points. The
theoretical interest of the second study is to examine the mediating role of
self-efficacy between the treatment and the outcome. In particular, previous
studies have suggested that treatments on substance abuse work partly through
increaing participants' self-efficacy on self-control in alcohol/drug uses.
@moniz-lewis2022's secondary analysis on @bowen2014 aims to examine this.


Model Construction
------------------

The model constructed here does not aim to replicate those presented in
@moniz-lewis2022 and @bowen2014. In addition, since no data are publicly
available from the original studies, many modeling decisions of the authors
remain elusive and unavailable to me. Upon such circumstances, decisions that
seem most reasonable in a real-world setting to me are made. The data are then
simulated and the model constructed according to these decisions. My decisions
are certainly biased, but they are sufficient to surve the purpose of the model
presented here---demonstrating the application of IRT in the field.


### Causal Assumptions

The DAG below constructs the 

![Assumptions of the causal relationships between variables of interest](./dag)

![Assumptions of the causal relationships between variables of interest](./dag-longitudinal)
