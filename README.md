# PPS-18-PrologZ

PrologZ is a DSL which imitates the core features of Prolog, to provide a direct support for logic programming in Scala.

This project is made using Scalaz, a library for functional programming that complements the Scala standard library.

What I studied about Scalaz and the details of this project implementation are available in [report](report.pdf).

The code is organized in two packages: [prologz/dsl](src/main/scala/prologz/dsl), which contains the definition of all elements available to users, and [prologz/resolution](src/main/scala/prologz/resolution), containing what is needed to solve Prolog programs.

The [samples package](src/main/scala/samples) holds some simple examples of writing and solving Prolog programs with the DSL.
