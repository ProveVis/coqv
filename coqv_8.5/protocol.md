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
1. A node is encoded as a tuple with three subfields: `id`, `label`, and `state`:
    - `id`: a string value specifies the id of a node;
    - `label`: a string value that specifies the formula to be proved at the moment;
    - `state`: a string value that specifies the state of the proved formula, for instance: `Proved`, `Not_proved`, `Assumed`, etc.
2. An edge is encoded as a pair of string values, which are the `id`s of the corresponding two nodes.

## Data format
1. JSON
    The format of JSON data in coqv is a set of Key/Value pairs:
    - Key: `protocol_version`, Value: a string value;
    - Key: `type`, Value: an integer value, where `0` means a request, and `1` means a response;
    - Key: `status`, Value: an integer value;
    - Key: `content`, Value: a string value;
    - Key: `node_id`, Value: a string value;
    - Key: `node_label`, Value: a string value;
    - Key: `node_state`, Value: a string value;
    - Key: `from_id`, Value: a string value;
    - Key: `to_id`, Value: a string value.

2. XML

(*to be determined*)
