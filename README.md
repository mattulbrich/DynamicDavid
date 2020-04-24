# DynamicDavid

Interactive Hilbert Calculus for propositional dynamic logic

This is version 0.1 licensed under the GPL 3.0 and
written by Mattias Ulbrich. (c) 2020 Karlsruhe Institute of Technology

This commandline tool allows you to conduct formal proofs in a Hilbert
calculus for propositional dynamic logic (PDL). 

This is (mainly) for teaching purposes as the calculus is provably
effective, by quite inefficient. It's called "Dynamic David" since
Hilbert's first name is David.

## Running the tool

Make sure you have the Java Development Kit (version 8+)
installed. Clone the repository, change into the repository directory
and execute

    ./gradlew shadowJar
    java -jar ./build/libs/Hilbert-0.1-all.jar -d examples
    
You can then load examples from the examples directory directly. Try,
e.g.

    load "hanoi.dd" .
    
to obtain an example proof.

## Using the prover

The command line interface is pretty rudimentary still.  Commands are
not terminated, but only a terminating `.` at the end of a line will
start the interpreter.

Some help can be obtained by "help ."
