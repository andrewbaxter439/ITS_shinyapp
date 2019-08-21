# Using an Interrupted Time Series model to evaluate England's Teenage Pregnancy Strategy

England's Teenage Pregnancy Strategy (TPS) launched in 1999, with the aim of reducing rates of teenage pregnancy by 50% in 10 years.

Interrupted Time Series methods are a way of evaluating the impact of a policy, such as the TPS, by comparing trends before and after the intervention dates. Best-fit straight lines are drawn through the time periods and the differences in level and slope are compared.

In this Shiny App, you can repeat the analyses from my PhD project, using ITS to test whether the Strategy produced a measurable change in rates. The initial graph in the 'Full Plot' tab shows the basic model comparing England alone, before and after the 1999 intervention date. The dashed line represents what hypothetically would have happened had there been no change at that date.

You can use the sidebar to alter the model and test other things:

- Compare other age ranges. Sufficient data prior to 1999 for England alone for other age groups isn't available. England and Wales combined data is loaded for these comparisons.
- Add a comparison/control country using Scotland or Wales data. Trend lines are drawn separately for each country and each period. Dashed Predicted lines show the hypothesised rates that England would have seen, had the rates experienced the same change in trend and level as Scotland/Wales.
- Add a second 'intervention' at 2008. A revised and renovated version of the TPS is hypothesised to have had a different effect size from 2007/2008 onwards. If the line is split again here, does treating this as second intervention provide a better fit?
- Adjust dates of observation and dates of interventions to be tested. You can use this to limit measurements to only years where both countries have data.
- Add confidence interval ribbons to England's fitted trends post-intervention, and to predicted trends. May make the graph unclear, but can give a visual representation of how fitted the model is.
- Add autocorrelation adjustments (advanced)