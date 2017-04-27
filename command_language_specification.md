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
For instance, command `:select-node n1` indicates that we want print the information related to node `n1`, or visualize node `n1` in VMDV.

**Basic commands:**

1. Select specific nodes.

        :select-node <node-id> <node-id> <node-id> ...

2. Show label of specific nodes.

        :show-label <node-id> <node-id> <node-id> ...

3. Hide specific subtrees.

        :hide-subproof <node-id> <node-id> <node-id> ...

4. Show the children of specific nodes.

        :show-children <node-id> <node-id> <node-id> ...

