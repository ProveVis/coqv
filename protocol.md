# The communication protocol between CoqV and VMDV
## Introduction
The communication between coqv and vmdv can be proceed via IPC channels. The IPC channels can be either pipes or sockets, where both ways need a well documented protocol to specify the packing and unpacking of the data structures to be transfered between two tools.

Three parts of a communication protocol need to be specified.
1. Meta information: including the version of the protocol, and status number (error number) of the responses.
2. The application data to be transfered between coqv and vmdv: including requesting data such as highlighting nodes, and responsing data such as the status number (error number).
3. The underlying packing and unpacking format of the data to be transfered: either XML or JSON. 

## Meta information
1. Version number: by dates, such as `20170502`.
2. Status number: by integers, such as
    - 0: `OK`
    - 1: `Protocol_does_not_match`
    - 2: `Node_not_avaliable`
3. Message type: by integers, such as
    - 0: request
    - 1: response 

## Application data
(*encoding of the proof tree, to be determined*)

## Data format
1. XML
2. JSON

## An illustrative example
(*to be determined*)