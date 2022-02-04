# ASIMOV

**ASIMOV** (short for “Algorithm of Selectivity by Incentive and Motivation for Optimized Valuation”) is an agent-based simulation of decision-making processes in foraging. It provides a fundamental framework for developing artificial animal behavior and cognition through step-wise modifications, where pre-existing circuitry is plausibly modified for changing function and tested, as in natural evolutionary exaptation. ASIMOV was built on an earlier simulation, [Cyberslug](https://github.com/Entience/Cyberslug), which is based on the behaviors and neuronal circuitry of the simple predatory sea slug _Pleurobranchaea californica_. In Cyberslug, a forager affectively integrates sensation, motivation (hunger), and learning to make cost-benefit decisions for approach or avoidance of prey. ASIMOV builds upon this with the explicit implementation of reward experience, pain, and homeostatic plasticity, providing insights on the nature and course of the addiction process, as an extreme expression of aesthetic preference. 

For more details please see our publication:
> Gribkova, E. D., Catanho, M., & Gillette, R. (2020). Simple Aesthetic Sense and Addiction Emerge in Neural Relations of Cost-Benefit Decision in Foraging. _Scientific reports_, 10(1), 1-11. https://doi.org/10.1038/s41598-020-66465-0

To run ASIMOV and inspect the code, the multi-agent modeling program NetLogo must be acquired from https://ccl.northwestern.edu/netlogo/.

### Future Developments

ASIMOV is being further developed with the addition of the Feature Association Matrix (FAM), which enables the formation of simple episodic memory, allowing the ASIMOV agent to create spatial maps of its environment that it can use to maximize the rewards that it obtains in foraging. The FAM is an abstraction of physiological circuits implicated in episodic memory, such as the auto- and hetero-associative circuits of the hippocampus.

The FAM essentially chains together pair-wise associations and allows for memorization of overlapping spatio-temporal sequences.
![](https://github.com/KatyaGribkova/KatyaGribkova/blob/main/ASIMOV-FAM_spatiallearning1_lq.mp4?raw=true)

Encoding of additional contexts in the FAM, such as distance and direction of an agent's travel from path integration, enables learning of more complex environments.



