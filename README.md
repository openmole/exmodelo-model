## Build console

```sh
> cd scala
> sbt
> project console
sbt> run
```

## Build Zombieland

(you need to install npm to run this version)

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

## Build sub-model Cooperation

```sh
> cd scala
> sbt
sbt> project cooperation
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
```sh
> cd scala
> sbt
sbt> project ode
sbt> osgiBundle
```

The ode jar program is:
scala/ode/target/scala-2.13/ode_2.13-0.1.0-SNAPSHOT.jar

## Build Zombieland jar for OpenMOLE

```sh
> cd scala
> sbt
sbt> project zombies-bundle
sbt> osgiBundle
```

The OpenMOLE jar plugin is in:
```sh
scala/bundle/target/scala-2.12/zombies-bundle_2.12-0.1.0-SNAPSHOT.jar
```
