# Unforced Non-Stationary Multi-Armed Bandits (U-NS-MAB)

Welcome to the supplementary material for the paper:

- TODO

This repository contains the reference implementation of the "Unforced Non-Stationary Multi-Armed Bandits" (U-NS-MAB) and all the information required to reproduce the experiments in the paper. For this reason, it is partially frozen at the time of
publication.

This repository is released under the AGPLv3 license. Please see the [LICENSE.md](LICENSE.md) file.
The data from the [Bioliq®](https://www.bioliq.de/english/) pyrolisis plant,
which we use in the paper, is licensed under CC-BY-NC-SA-4.0.
You can download it from [here](https://www.dropbox.com/s/gyrb62ebtcmvy9h/Bioliq_S-MAB.zip).

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

This creates a jar in the folder `target/scala-2.12/` named `S-MAB-<version>.jar`, which can be run from java (no
sbt/scala installation required). The version of the package at the time of the experiments is 1.0.

Once you have built the jar, you can run it as follows:

```
java -jar target/scala-2.12/S-MAB-1.0.jar <arguments>
```

## Reproducing the experiments

In this section, we explain how to reproduce the experiments from our paper. The experiments create about XGB of data
and require about X hours on a server with X cores at X GHz and XGB RAM, using Java Open-JDK 8 and Scala 2.12.8. Results are saved inthe folder experiments as .csv files, along with logs.

Note that depending on your setting, you might want to increase the memory available for the JVM (using the Xmx option, for example), or you might run into some `java.lang.OutOfMemoryError: GC overhead limit exceeded` exception.

### TODO

- precomputed Bioliq data: `data/Bioliq_S-MAB_1wx20_MI_1000_100.csv`


## Visualize the results

Then, you can use the Jupyter notebooks in folder `visualize` to reproduce
the plots from the publication. By the time of the experiments, we use the following Python packages:

```
# Name                    Version
matplotlib                X
numpy                     X
pandas                    X
seaborn                   X
```

Each experiments have dedicated notebooks, in the folder `visualize/`:

`TODO` -> `TODO.ipynb`

## Additional experiments

TODO

## Contributing

We welcome contributions to the repository and bug reports on GitHub.

For questions and comments, please contact `edouard.fouche@kit.edu`, or open an issue.

## Acknowledgements

- The [ELKI project](https://elki-project.github.io/) for the R*-Tree index structure, that we use to accelerate the
nearest neighbors queries for computing Mutual Information.
- We thank the pyrolysis team of the [Bioliq®](https://www.bioliq.de/english/) process for providing the data for our real-world use case.