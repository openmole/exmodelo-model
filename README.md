## Build console

```sh
> cd scala
> sbt
> project console
sbt> run
```

## Build Zombieland

```sh
> cd scala
> sbt
sbt> project zombieland
sbt> buildGUI
```

Then browse:
```sh
scala/zombieland/target/zombies.html
```

## Build sub-model Vigilence

```sh
> cd scala
> sbt
sbt> project vigilence
sbt> buildGUI
```

Then browse:
```sh
scala/vigilence/target/zombies.html
```

## Build sub-model spatialsens

```sh
> cd scala
> sbt
sbt> project spatialsens
sbt> buildGUI
```

Then browse:
```sh
scala/spatialsens/target/zombies.html
```

## Build sub-model ode


## Build Zombieland jar for OpenMOLE

```sh
> cd scala
> sbt
sbt> project model
sbt> osgiBundle
```

The OpenMOLE jar plugin is in:
```sh
scala/model/target/scala-2.12/model_sjs0.6_2.12-0.1.0-SNAPSHOT.jar
```
