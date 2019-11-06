# Arational-Belief-Convergence
## Overview
This model considers time-to-convergence on a belief in a population as moderated by two parameters: Probability of Belief Change (PBC) and audience size. The population always converges on a single belief, but there are a number of other interesting patterns to the model: agents with very low PBCs usually, but do not always, have their beliefs adopted by the population. And having the highest value for audience size isn't necessarily best. Users can find here the model and data sets I used for analyzing the model. The related paper is "Arational Belief Convergence" in Synthese (and also available on my website at charleslassiter.weebly.com)

### PBC
PBC is a value < 1.0 indicating the likelihood that an individual agent will abandon her belief in soe interaction. The range in the model is between 0.01 and 0.10 but these can be readily modified. The motivating idea behind PBC is: how likely are folks to change their minds?

### Audience size
This is exactly what it sounds like: the size of an agent's audience. It's defined by a proportion (between 0.01 and 0.10) of the total population. When the population density in the model is set to 0.75, there are ~315 agents. This proportion reflects bumping into people I know at the store (~3 people) to a town hall (~30 people). 

### Results
All runs of the model without dogmatists (i.e. PBC = 0) converge eventually. Higher PBC and audience size values converge faster. When PBC and audience size values are allowed to vary (i.e. selected from a uniform random distribution between 0 and the value for the run), lower PBCs for agents result in a greater likelihood of their belief getting adopted. Similar comments going for higher audience sizes. However, while it was often the case that the agent with the lowest PBC was most often the agent whose belief was adopted by others, it was not the case that the agent with the highest audience size was most often the agent whose belief was adopted by others. That honor was reserved for agents whose audience size was typically in the bottom of the highest quintile of audience size values.
