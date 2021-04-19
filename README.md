# Acton

Acton is a real-time tasking executive for Ada applications. It implements the Ravenscar Profile and was design to implement and test the novel Cyclic Task language feature proposed in the PhD thesis *‌[Reducing the Cost of Real-Time Software through a Cyclic Task Abstraction for Ada](https://doi.org/10.25911/5d74e77b72869)* by Pat Bernardi. 

In addition to cyclic tasks, the executive takes the opportunity to explore different implementation ideas, including:

- Protected objects as a scheduling entity.
- Implementing schedulers as task-like entities (an interesting, flexible but inefficient idea).
- Active enforcement of task execution budgets.
- Support for priority servers for aperiodic tasks.

The project contains Acton and two helper tools:

- Acton Analyser: an ASIS tool that lists the tasks and protected objects contained within an application built on top of Acton and their properties.
- Oak Viewer: a command line tool that displays kernel trace data using  the ARM Cortex-M ITM and DWT features.

A detailed description of the design and architecture of Acton can be found in *‌[Reducing the Cost of Real-Time Software through a Cyclic Task Abstraction for Ada](https://doi.org/10.25911/5d74e77b72869)*

This project is not currently been developed.
