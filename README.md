
# A toolkit for localisation queries
Implementation of the formal model definition for localisation queries proposed in the paper [A toolkit for localisation queries](Link Coming Soon) submitted to [IMWUT 2023](https://dl.acm.org/journal/imwut)

This is an initial version of the project and an in-depth explanation of the functions and how the tool works can be found in [FunctionDefinitions.Rmd](Notebooks/Definitions/FunctionsDefinitions.Rmd.md)

## Examples
Here are some examples extracted from the paper

### Agents Tracking
> Let us suppose we are interested in following the agent $A_1$ and simply getting the list of rooms they have visited throughout the time series. As the scenario does not include any specific transition, we can just express this as a single flexible state:
 $$ \tag{Agent Tracking Pattern} \langle A_1, * \rangle $$
```r
agent_journey <- start(data) %>%
    add_state(agents(set(a1)), rooms(set(*))) %>%
    end()
```


### Simple Transitions
> The next step is to find all the instances in which the agent $A_1$ moved from room $R_1$ to  room $R_2$. We can express the transition as fuzzy or direct depending on the purpose of the query. A fuzzy transition would also consider sections of the time-series in which the agent visited other rooms before ending in $R_2$, while the direct transition would omit these cases.
> __Direct Transitions__: A direct transition assumes that the system is not in any other state in between the preceding state ane the following one
$$ \tag{Direct Transition} \langle A_1, R_1 \rangle \rightsquigarrow \langle A_1,  R_2 \rangle $$
> __Fuzzy Transitions__: A fuzy transition however, allows for anything to happen in between the states as long as the system eventually appears in the final state
$$ \tag{Fuzzy Transition} \langle A_1, R_1 \rangle \rightarrow \langle A_1,  R_2 \rangle $$
```r
fuzzy_transition <- start(data) %>%
    add_state(agents(set(a1)), rooms(set(r1))) %~>%
    add_state(agents(set(a1)), rooms(set(r2))) %>%
    end()
direct_transition <- start(data) %>%
    add_state(agents(set(a1)), rooms(set(r1))) %->%
    add_state(agents(set(a1)), rooms(set(r2))) %>%
    end()
```

### Groups and Transitions
> A more complex example is to include a group of agents and a transition to a state with different agents or rooms. A real-world scenario could be instances in which a classroom initially only contains the lecturer, eventually fills up with students, and finally it becomes empty. It should be noted that the last transition can be either fuzzy or direct: Since the set of rooms of interest remains the same, and the penultimate transition includes a variable amount of agents in the room and at least one agent, there would not be a case in which the room becomes empty in between. An empty room in between would not be included in neither the first nor the second state, effectively breaking up the trace.
 $$ \langle A_1 [1], R_1 \rangle \rightsquigarrow \langle A_1 [1+],  R_1 \rangle \rightarrow  \langle [0],  R_1 \rangle $$
```r
group_transition <- start(data) %>%
    add_state(agents(set(a1), range(1,1)), rooms(set(r1))) %~>%
    add_state(agents(set(a1), range(1,Inf)), rooms(set(r1))) %->%
    add_state(agents(range(0,0)), rooms(set(r1))) %->%
    end()
```

### Multiple complex Transitions
> Combining the previous examples we could now try to define a more complex pattern that includes some wildcards as well. The pattern we are interested in can be explained as:
  1. Agent $A_1$ was alone in one of the rooms $\{R_1, R_2, R_3\}$
  2. Agent $A_1$ left the room and a new group of agents came in, with at least $[2+]$ agents in the group
  3. Agent $A_1$ eventually joined the group
$$ \langle A_1 [1], \{R_1,R_2,R_3\} \rangle \rightsquigarrow \langle \{!A_1\} [2+],  \{?\} \rangle \rightarrow  \langle A_1 [3+],  \{?\} \rangle $$

```r
group_transition <- start(data) %>%
    add_state(agents(set(a1), range(1,1)), rooms(set(r1))) %~~>%
    add_state(agents(set(a1), range(1,Inf)), rooms(set(r1))) %-->%
    add_state(agents(range(0,0)), rooms(set(r1))) %>%
    end()
```

### Contact Tracing 1
> The contact tracing scenarios are largely based on the need of hospitals of tracking the spreading of viruses and potential contaminating agents across the ward. This type of query became especially relevant in light of the COVID-19 pandemic presenting a serious threat to the global population since 2019. We provide three different examples of possible contact tracing queries. While in the previous example the selection of agents was only numerically bounded, in this example we assume that at least one agent $A_x$ has to be in the selected room(s). We are interested in extracting situations in which the fixed $A_x$ agent has come in contact with any other agent, i.e. we are looking for any other agent being present in the same room as $A_x$ at any given time.
 $$     \langle A_x [2+], *\rangle $$
The R code translation of the above expression is as follows:
```r
contact_tracing1 <- start(data) %>%
    add_state(agents(set(ax), range(2,Inf)), rooms(set(.any))) %>%
    end()

# Code to tabulate the results
found_agents <- contact_tracing1$result$agents
summary <- lapply(found_agents[found_agents != "a1"], function(x){
    contacts <- contact_tracing1[c("ax", x)]
    res <- list(contact=paste0("$A_x + ", x))
    contact_times <- !is.na(contacts$rooms.with.agents)
    consecutive <- rle(contact_times)
    c.true <- consecutive$lengths[which(consecutive$values == TRUE)]
    res["encounters"] <- length(c.true)
    res["max_duration"] <- max(c.true)
    res["total_duration"] <- nrow(contacts[contact_times])
    return(res)
 })
summary_table <- do.call(rbind, lapply(summary, data.frame))
```

# TODO...

## Installation
You can install the development version of semanticLocR from [GitHub](https://github.com/) with:
``` r
# install.packages("devtools")
devtools::install_github("Gabryxx7/semantic-localisation-r")
```
