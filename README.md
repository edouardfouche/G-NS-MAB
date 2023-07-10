# Globally Non-Stationary MAB (G-NS-MAB)

[![License AGPL-3.0](https://img.shields.io/badge/License-AGPL--3-brightgreen.svg)](https://github.com/edouardfouche/MCDE-experiments/blob/master/LICENSE.md)

Welcome to the supplementary material for the paper:

- Junpei Komiyama, Edouard Fouché and Junya Honda. 2021. Finite-time Analysis of Globally Nonstationary Multi-Armed
  Bandits (Preprint)

This repository contains the reference implementation of the "Globally Non-Stationary Multi-Armed Bandits" (G-NS-MAB).
In particular, it features the implementation of the so-called ADR-bandit and ADS-bandit. It contains all the
information required to reproduce the experiments in the paper. For this reason, it is partially frozen at the time of
publication.

This repository is released under the AGPLv3 license. Please see the [LICENSE.md](LICENSE.md) file. The data from
the [Bioliq®](https://www.bioliq.de/english/) pyrolisis plant, which we use in the paper, is licensed under
CC-BY-NC-SA-4.0. You can download it from [here](https://www.dropbox.com/s/gyrb62ebtcmvy9h/Bioliq_S-MAB.zip).

If you are using the code or data from this repository, please cite our paper.

## Quick start

### Build it and run it

**Requirements** : ([Oracle JDK 8](https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) or [OpenJDK 8](http://openjdk.java.net/install/)) and [sbt](https://www.scala-sbt.org/1.0/docs/Setup.html)

The project is built with sbt (version 1.2.8). You can compile, package or run the project as follows:

```
sbt compile
sbt package
sbt "run <arguments>"
```

You can also export a "fat" jar, including all dependencies and scala libraries using [`sbt-assembly`](https://github.com/sbt/sbt-assembly):

```
sbt assembly
```

This creates a jar in the folder `target/scala-2.12/` named `G-NS-MAB-<version>.jar`, which can be run from java (no
sbt/scala installation required). The version of the package at the time of the experiments is 1.0.

Once you have built the jar, you can run it as follows:

```
java -jar target/scala-2.12/G-NS-MAB-1.0.jar <arguments>
```

## Reproducing the experiments

In this section, we explain how to reproduce the experiments from our paper. The experiments create about 0.8GB of data
and requires about 6 hours on a server with 64 cores at 4.0GHz and 128GB RAM, using Java Open-JDK 8 and Scala 2.12.8.
Results are saved in the folder `experiments` as .csv files, along with logs.

We provide the original experimental data used in the paper, see `experiments/experiments_data.7z`. If you want to
reproduce all the experiments, you can run

```
sbt "run G-NS-MAB"
```

Or, if you have built the fat jar: `java -jar target/scala-2.12/G-NS-MAB-1.0.jar G-NS-MAB`

See the `src/main/scala/Main.scala` file for more detailed information.

Note that depending on your setting, you might want to increase the memory available for the JVM (using the Xmx option,
for example), or you might run into some `java.lang.OutOfMemoryError: GC overhead limit exceeded` exception.

The data for real-world application are in the folder `data`, the generators for synthetic data are included in the
package.

## Visualize the results

Then, you can use the Jupyter notebooks in folder `visualize` to reproduce the plots from the publication. By the time
of the experiments, we use the following Python packages:

```
# Name                    Version
matplotlib                3.3.2
numpy                     1.19.2
pandas                    1.1.3
seaborn                   0.11.0
```

Each experiments have dedicated notebooks in the folder `visualize`. Plots are in `visualize/plots`

## Contributing

We welcome contributions to the repository and bug reports on GitHub.

For questions and comments, please contact `edouard.fouche@kit.edu`, or open an issue.

## Acknowledgements

- We thank the pyrolysis team of the [Bioliq®](https://www.bioliq.de/english/) process for providing the data for our
  real-world use case.
- This repository contains a preprocessed version of the Zozo data set found
  here: [https://github.com/st-tech/zr-obp](https://github.com/st-tech/zr-obp)