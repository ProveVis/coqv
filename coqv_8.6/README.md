# coqv
The purpose of the coqv project is to build an interface for coqtop, and record the whole proof tree while in a proving process. Then, we can send proof trees to the visualization tool [VMDV](https://github.com/terminatorlxj/VMDV), and finally achieve our goal: **proof via visualization!**

## Build by OASIS

    oasis setup
    ./configure
    make

## Run
**Please make sure that the backend of coqv, which is coqtop, is already installed in your Linux/MacOS computer.**

See the help doc of coqv via:

    coqv -help

## Other
Now, this version is built with regard to the xml protocol of coqtop version 8.6.
However, it seems our interface works fine with the 8.7 version of coqtop. 

If you are also interested in this project and have any ideas on either the code or the design of coqv, please please please contact 

    Jian LIU (liujian@ios.ac.cn).
