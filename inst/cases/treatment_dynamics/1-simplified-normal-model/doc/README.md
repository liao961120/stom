---
title: Growth Curve Modeling of Latent Constructs with Embedded Item Response Submodels
short-title: Growth Curve Modeling of Latent Constructs
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

Consider the scenario of a study examining potential causal factors that may be
related to the clinical intervention and the outcome. These factors as well as
the treatment outcomes are often assessed and measured indirectly through
self-reported questionnaires. In such settings, where responses to
questionnaires are collected, it would be desirable to analyze the data with
models of item response theory. In practice, however, raw sum scores of
questionnaire items, instead of latent scores estimated through IRT models, are
often taken directly as the measures of the latent constructs of interest.

The reason for these practices results from researchers not being aware of the
possibility of embedding IRT models into a larger system of _interconnected_
regression models. Doing so would allow the latent constructs and all other
parameters of interest to be *jointly estimated* by the model. This is much more
rigorous than raw sum scores as the IRT submodel directly deals with
measurement errors associated with self-reported questionnaires. In addition,
information can now flow through all modeled variables, avoiding biases and
overconfidence that could have been subtly introduced when there are multiple
_disconnected_ regression models. This happens when the parameters of a model,
such as estimates of participants' IQ from an IRT model, are fed in as inputs to
another model.

The present document demonstrates one such application of embedding an IRT model
into a large growth curve model, enabling the joint modeling of all parameters
of interest in a study.


Background
----------

The model constructed here is largely inspired by the study of @bowen2014 and a
secondary analysis of the data collected by the former [@moniz-lewis2022]. The
context of the studies is the evaluation of three different treatments for
substance use disorder. Participants were recruited and randomly assigned to one
of the three treatment conditions. Before (baseline) and after the treatment,
data on treatment outcomes (amount of heavy drinking and drug use) and
self-efficacy on the control of alcohol/drug use (measured through
questionnaires) were collected. There were four such evaluations for the
participants, including the baseline, thus resulting in a set of longitudinal
data of four time points. The theoretical interest of @moniz-lewis2022\'s study
is to examine the mediating role of self-efficacy between the treatments and the
outcomes. In particular, previous studies have suggested that treatments for
substance use problems work partly through the increase of participants'
self-efficacy in the control of substance use.


Causal Assumptions
------------------

Based on the descriptions in the articles, the assumed causal relations among
the variables of interest are explicated in the following Directed Acyclic
Graphs (DAGs). Note that these causal relations may depart from those implied in
@bowen2014 and @moniz-lewis2022. Since there is no publicly available data from
both studies, ambiguous descriptions in the methodology sections could not be
disambiguated through the data. Upon encountering such situations, opinionated
decisions were made to serve the need of the current demonstration.

![Assumed causal relations among the variables of interest (time collapsed).](./dag)

The first DAG presented here is a compacted representation of the causal
assumptions underlying our model. The time dimension is collapsed. Below are the
descriptions of the variables of interest.

$E$

:    Participants' self-efficacy on alcohol/drug use control
      
     Since self-efficacy $E$ is not directly observed, it is represented as a
     circled node in the DAG.

$R$

:   Item responses collected through self-efficacy questionnaires

    To measure the unobserved self-efficacy $E$, tools such as a questionnaire
    are required to measure the latent construct. $R$ stands for the responses
    collected through the questionnaire. These responses would allow the
    estimation of the variable $E$ for each participant. Note that the item
    parameter $I$ is left out for simplicity. If present, it would point to $R$
    as item estimates also affect the responses $R$.

$A$
:   Participants' age

$T$
:   Treatment condition received by a participant

$D$

:   Latent treatment outcome 

    $D$ is the theoretical variable that underlies the observable treatment
    outcome. It is latent, and arguably a statistical artifact. Its purpose is to
    serve as an aggregate of all assumed effects on the treatment outcome
    $D^{\ast}$.

$D^{\ast}$

:   Treatment outcome, or, manifest of the latent treatment outcome $D$

The arrows among the nodes in the DAG indicate the directions of influence. So
the graph is basically saying that the treatment affects the outcome through two
pathways. One direct, and the other indirectly through self-efficacy. Age also
has direct influences on self-efficacy and the treatment outcome. The labels on
the edges mark the regression coefficients, which are the parameters of interest
for use in later simulations and model testing.

![Assumed causal relations among the variables of interest (simplified illustration of two time points).](./dag-longitudinal)

The second DAG adds in the time dimension. To avoid cluttering the graph, only
two, instead of four, time points are shown here. The subscripts on the
variables mark the time points. $t=0$ indicates the baseline (i.e., the first)
evaluation. A caution to note here is that age only *directly* influences
self-efficacy at the baseline ($A \rightarrow E_0$). Self-efficacy at subsequent
time points is influenced by age only *indirectly* through $E_0$. This slight
complication becomes clearer in the following description of the model
(data-generating process).


Model Specification
-------------------

\newcommand{\midx}[2]{ \mathrm{#1},\mathrm{#2} }
\newcommand{\aidx}[3]{ \mathrm{#1},\mathrm{#2},\mathrm{#3} }
\newcommand{\vidx}[1]{ \mathrm{#1} }
\newcommand{\annot}[1]{ \begin{center} {\scriptsize #1} \end{center}}
\newcommand{\bb}[1]{ \beta_{\mathrm{#1}} }
\newcommand{\gm}[1]{ \gamma_{\mathrm{#1}} }

\annot{Treatment Outcome Generative Process}

\begin{align*}
   D^{\ast}_{\midx{Sid}{t}}                       & \sim \text{Normal}( D_{\midx{Sid}{t}}, \sigma )                       \\
   D_{\midx{Sid}{t}}                              & = \alpha + (\alpha_{\vidx{Sid}} + \gm{TD[\vidx{Sid}]} \text{t}) + \bb{TD [T[\vidx{Sid}]]} \text{t} \\
                                                  & \phantom{PP} + \bb{AD} \text{A}_{\vidx{Sid}} + \bb{ED} E_{\midx{Sid}{t}} \\
   \sigma                                         & \sim \text{Exponential}(1) \\
   \begin{bmatrix} \alpha \\ \gamma \end{bmatrix} & \sim \text{MVNormal}(\pmb{0}, \pmb{\Sigma}) \\
   \alpha                                         & \sim \text{Normal}(0, 1.5) \\
   \bb{TD}, \bb{AD}, \bb{ED}                      & \sim \text{Normal}(0, 1) 
\end{align*}

\annot{Item Response Generative Process}

\begin{align*}
   R_{\aidx{Sid}{Iid}{t}}    &\sim \text{OrderedLogit}( \phi_{\aidx{Sid}{Iid}{t}}, \kappa )    \\
   \phi_{\aidx{Sid}{Iid}{t}} &= E_{\midx{Sid}{t}} + I_{\vidx{Iid}}                           \\
   \kappa               &\sim \text{Normal}(0, 1)         \\
   E                    &\sim \text{Normal}(0, 2)         \\
   I                    &\sim \text{Normal}(0, \rho)      \\
   \rho                 &\sim \text{Exponential}(1.5)
\end{align*}

\annot{Efficacy Generative Process}

\begin{align*}
   E_{\midx{Sid}{t}}           & \sim \text{Normal}( \mu_{\midx{Sid}{t}}, \tau )                   \\
   \mu_{\midx{Sid}{t}}         & = \delta + \bb{AE} \text{A}_{\vidx{Sid}}  + \bb{TE [T_{\vidx{Sid}}]} \text{t}  \\
   \tau                   & \sim \text{Exponential}(1)  \\
   \delta                 & \sim \text{Normal}(0, 1)  \\
   \bb{AE}, \bb{TE} & \sim \text{Normal}(0, 1)
\end{align*}
 

References
----------
