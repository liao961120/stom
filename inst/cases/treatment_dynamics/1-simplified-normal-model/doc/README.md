---
title: "Growth Curve Modeling of Latent Variables with Embedded Item Response Models"
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

Consider the scenario of a study aiming to examine the role of some factors that
may be causally related to the clinical intervention and the treatment outcome.
These factors as well as the treatment outcomes are often assessed and measured
indirectly through self-reported questionnaires. In such settings, where
responses of questionnares are collected, it would be desirable to analyze the
data with models of item response theory. In practice, however, raw sum scores
of questionnaire items, instead of latent scores estimated through IRT models,
are often taken directly as the measures of the latent constructs of interest.
The reason for these practices results from researchers not being aware of the
possibility of embedding IRT models into a larger system of regression models.
Doing so would allow the latent constructs and all other parameters of interest
to be *jointly estimated* by the model. This is much more rigorous as the IRT
submodel directly deals with measurement errors associated with self-report
questionnaires. In addition, information can now flow through all modeled
variables, avoiding biases and overconfidence subtlely introduced during
separate stages of modeling, such as when the outputs of a model (e.g.,
estimates of participants' IQ from an IRT model) are taken as inputs to a
subsequent model.

The present document demonstrates one such application of embedding an IRT model
into a growth curve modeling of patient outcomes after receiving treatments of
substance abuse.


Background
----------

The model constructed here is largely inspired by the study of @bowen2014 and a
secondary analysis on the data collected by the former [@moniz-lewis2022]. The
context of the studies is the evaluation of three kinds of treatments on
substance abuse. Participants were recruited and randomly assigned to one of the
three treatment conditions. Before (baseline) and after the treatment, data on
treatment outcomes (amount of heavy drinking and drug use) and self-efficacy on
alcohol/drug control (measured through questionary) were collected. There were
four such evaluations for the participants, including the baseline, thus
resulting in a set of longitudinal data of four time points. The theoretical
interest of @moniz-lewis2022\'s study is to examine the mediating role of
self-efficacy between the treatments and the outcomes. In particular, previous
studies have suggested that treatments on substance abuse work partly through
increaing participants' self-efficacy on the control of alcohol/drug uses.


Causal Assumptions
------------------

Based on the descriptions in the aritcles, the assumed causal relations among
the variables of interest are explicated in the following Directed Acyclic
Graphs (DAGs). Note that these assumed causal relations may depart from those
stated in @bowen2014 and @moniz-lewis2022. No data from both studies are
publicly available. Hence, upon encountering ambiguous descriptions that
couldn't be disambiguated through data, opinionated decisions were made to surve
the need of the current demonstration.

![Assumed causal relations among the variables of interest (time collapsed).](./dag)

The first DAG presented here is a compacted representation of the causal
assumptions underlying our model. The dimension of time is collapsed. Below
explains the variables of interest.

- $E$: participants' self-efficacy on alcohol/drug use control
   
   Since self-efficacy $E$ is not directly observed, it is represented as a
   circled node in the DAG.

- $R$: item responses collected through the self-efficacy questionnaire 

   To measure the unobserved self-efficacy $E$, tools such as a questionnaire
   are required to measure the latent construct. $R$ stands for the responses
   collected through the questionaire. These responses would allow the
   estimation of the variable $E$ for each participant. Note that the item
   parameter $I$ is left out for simplicity. If present, it would point to $R$
   as item, in addition to person, estimates, also affects the responses $R$.

- $A$: participants' age

- $T$: the type of treatment received by a participant

- $D$: the latent treatment outcome 

   $D$ is the theoretical variable the underlies the observable treatment
   outcome. It is latent, and arguably a statistical artifact. It's purpose is
   to serve as an aggregate of all assumed effects on the treatment outcome
   $D^{\ast}$.

- $D^{\ast}$: the treatment outcome, or, the manifest of the latent outcome $D$

The arrows among the variables indicate the directions of influences. So the 
DAG is basically saying that the treatment affects the outcome through two 
pathways. One direct, and the other indirectly through self-efficacy. Age also
has direct influences on self-efficacy and the treatment outcome. The labels on
the edges mark the regression coefficients, which are the parameters of interest
for use in later simulations and model testing.

The second DAG, shown below, adds in the time dimension. To avoid cluttering the
graph, only two, instead of four, time points are shown here. The lower right
subscripts on the variables mark the time points. $t=0$ indicates the baseline
(i.e., the first) evaluation. A caution to note here is that age only *directly*
influences self-efficacy at the baseline ($A \rightarrow E_0$). Self-efficacy at
subsequent time points are only influenced by age *indirectly* through $E_0$.
This slight complication becomes clearer in the following description of the
model (data-generating process).

![Assumed causal relations among the variables of interest (simplified illustration of two time points).](./dag-longitudinal)



Model Specification
-------------------

$$
\begin{aligned}
   & \text{\scriptsize Treatment Outcome Generative Process} \\
   & D^{\ast}_{[Sid,~t]} \sim Normal( D_{[Sid,~t]}, \sigma )                       \\
   & D_{[Sid,~t]} = \beta_{TD [T_{[Sid]}]} t + \beta_{AD} A_{[Sid]} + \beta_{ED} E_{[Sid,~t]}  \\
   \\
   & \text{\scriptsize Item Response Generative Process} \\
   & R_{[Sid,~Iid,~t]} \sim OrderedLogit( \phi_{[Sid,~Iid,~t]}, \kappa )    \\
   & \phi_{[Sid,~Iid,~t]} = E_{[Sid,~t]} + I_{[Iid]}                         \\
   \\
   & \text{\scriptsize Efficacy Generative Process} \\
   & E_{[Sid,~t]} \sim Normal( \mu_{[Sid,~t]}, \tau )                   \\
   & \mu_{[Sid,~t]} = \beta_{AE} A_{[Sid]}  + \beta_{TE [T_{[Sid]}]} t
\end{aligned}
$$


References
----------

