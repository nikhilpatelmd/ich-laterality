===========================================

Reviewer Comments

===========================================

Reviewer #1:

Thank you for this timely analysis using the high-quality ATACH-2 and ERICH datasets to examine whether ICH laterality is associated with neurosurgical intervention and 90-day outcomes. The clinical question is important. However, several methodological and statistical issues currently limit interpretability.

Major Comments

Interpretation and causal language
The manuscript frequently frames the laterality–intervention association as evidence of “bias/inequity.” Given the post-hoc observational nature of this analysis, residual confounding and clinical selection factors remain plausible alternative explanations (e.g., more granular measures of eloquence/depth beyond lobar vs deep, mass effect/hydrocephalus, and other determinants of surgical eligibility).
Action: Please temper causal language throughout (especially Abstract/Discussion) and present conclusions as associations unless a stronger causal identification strategy is implemented.

Estimand and adjustment strategy (Total vs Direct effect)
The adjustment strategy for functional outcomes requires clarification. If the clinical goal is the total effect of laterality on outcome (including pathways mediated through surgery), adjusting for neurosurgical intervention would block part of the clinically relevant pathway and may introduce bias.
Action: Please explicitly state the estimand (total vs direct effect) and present total-effect models that do not adjust for the mediator when that estimand is the focus.

Modeling/reporting of non-binary outcomes
Tables report adjusted odds ratios for outcomes that are continuous or count-based (e.g., ventilator days, EQ-VAS). An OR is not an appropriate metric unless outcomes were dichotomized (not clearly stated) or modeled in a way that supports an odds interpretation with explicit justification.
Action: Please clarify how these estimates were derived and use outcome-appropriate models (e.g., Poisson/negative binomial for ventilator days with IRR; linear/robust models for EQ-VAS with mean differences or justified transformations).

Center-level clustering (practice variation)
Neurosurgical decision-making is highly influenced by local practice patterns and resources. A random effect only for “study” (ATACH-2 vs ERICH) is unlikely to capture site-level clustering.
Action: Please attempt to include a random intercept for site/hospital (if available) or provide sensitivity analyses and discuss the likely impact on uncertainty/precision.

Prior sensitivity should be foregrounded
The manuscript describes results as robust, but the sensitivity analysis indicates meaningful dependence on prior specification for the primary endpoint (e.g., under the left-hemisphere prior, the 95% CrI for neurosurgical intervention includes 1.0).
Action: Please highlight this in the main text (not only the supplement) and moderate robustness claims accordingly.

Missing data transparency
Action: Please report missingness for key covariates and 90-day outcomes by laterality group, clarify how missing outcomes were handled (complete-case vs imputation), and provide sensitivity analyses where feasible.

Clinical interpretability (absolute effects)
Action (recommended): In addition to ORs, please report absolute adjusted probabilities/marginal effects for key outcomes to better convey clinical magnitude.

Minor Comments

Please use consistent Bayesian terminology (Credible Intervals, CrI).

Consider standardized mean differences in Table 1 rather than emphasizing p-values.

Clarify in limitations that left/right laterality is an imperfect proxy for hemispheric dominance.

Ensure subgroup conclusions are supported by formal interaction analyses (rather than subgroup comparisons in isolation).


Reviewer #2:

This is a well-designed and thoughtfully executed post-hoc analysis examining the association between hemispheric laterality and neurosurgical decision-making in intracerebral hemorrhage (ICH). Leveraging two large, well-characterized cohorts (ATACH-2 and ERICH) and applying a rigorous Bayesian analytic framework, the authors address an important and underexplored question with clear clinical and ethical relevance. The manuscript is clearly written, methodologically sound, and the results are internally consistent across multiple sensitivity analyses.
The central finding that patients with left hemispheric ICH are less likely to receive aggressive surgical interventions despite demonstrating better 90-day functional outcomes is both compelling and provocative and raises important questions regarding implicit bias in clinical decision-making. Overall, this is a strong contribution to the literature.
Major comments
1. While the authors appropriately acknowledge the observational nature of the analysis, parts of the discussion could further clarify that the findings demonstrate an association rather than causation. Expanding slightly on alternative explanations (e.g., unmeasured clinical nuance, family preferences, language-related prognostic framing) would strengthen the interpretive balance.
2. The discussion would benefit from a brief elaboration on how neurosurgical candidacy was determined in the original studies (to the extent possible), particularly whether institutional or temporal practice variation may have influenced decisions. This would help readers contextualize the observed differences in intervention rates.
3. The finding that left hemispheric ICH patients have better functional and quality-of-life outcomes despite less aggressive care is striking. A short discussion on potential mechanisms (e.g., differential impact of neglect vs aphasia on mRS and EQ-5D domains, or caregiver perception of disability) would enhance the clinical relevance of the results.
Minor comments
Consider briefly justifying the selected threshold for “substantial difference” (aOR >1.2) in the main text, even though this is addressed in the supplement.
The manuscript is otherwise well organized and clearly written, with tables and figures that are easy to interpret.

