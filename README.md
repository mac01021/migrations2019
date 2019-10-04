# Data Migration with Space Constraints

This is the simulation code for [Data Migration in Large Scale Heterogeneous Storage Systems with Space Constraints](www.google.com/).

The program takes no input.  When run, it generates a number of demand graphs (instances of the migration problem) and then
solves each using the four different algorithms described in the paper.

The lengths of the migration schedules produced during the simulation are 

1. Printed to the terminal in a **very unfriendly** textual format, and
2. Plotted in charts by connecting to the [Plotly](https://chart-studio.plot.ly/feed/) service.

Because of #2, to run the simulation, you must either [open a free plotly account and set up your credentials](https://plot.ly/scala/getting-started/#initialization), or else
edit the source code to disable plotly.



### To run the simulation:

1. Ensure your plotly credentials are configured (see the link above).
2. `git clone` this repository
3. Ensure, using your system's package manager, that [SBT](https://www.scala-sbt.org/) is installed.
4. Go, in a terminal, to the newly checked out git repo
5. Run `sbt compile`
6. Run `sbt "runMain MigrationsApp`
7. Wait several hours for the simulation to run.  As it is running, you will see new charts appear in your Plotly dashboard.

