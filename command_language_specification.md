# Language Specification for CoqV

## CoqV - the Coq plug-in for VMDV
The Coq plug-in of VMDV is called CoqV, which runs in interactive mode. 
The input language for CoqV is divided into two parts:

1. Input language for coqtop;
2. Command language that control the visualization process.

Here we focus on the command language.
### Command Language

Each command for CoqV is of the following form:

        :<command-name> [<args>]

`<command-name>` specifies the name of the command, and `<args>` specifies the arguments of the command, which are optional. 
For instance, command `:highlight-node n1 red` indicates that we want to highlight a node in the proof tree with color red.

