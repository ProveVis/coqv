=== Available calls ===

       "Add" :  ((string * int) * (Stateid.t * bool))
             -> (Stateid.t * (((unit, Stateid.t) CSig.union) * string))
   "Edit_at" :  Stateid.t
             -> ((unit, (Stateid.t * (Stateid.t * Stateid.t))) CSig.union)
     "Query" :  (string * Stateid.t)
             -> string
      "Goal" :  unit
             -> (Interface.goals option)
     "Evars" :  unit
             -> ((Interface.evar list) option)
     "Hints" :  unit
             -> (((((string * string) list) list) * ((string * string) list)) option)
    "Status" :  bool
             -> Interface.status
    "Search" :  ((Interface.search_constraint * bool) list)
             -> ((string Interface.coq_object) list)
"GetOptions" :  unit
             -> (((string list) * Interface.option_state) list)
"SetOptions" :  (((string list) * Interface.option_value) list)
             -> unit
   "MkCases" :  string
             -> ((string list) list)
      "Quit" :  unit
             -> unit
     "About" :  unit
             -> Interface.coq_info
      "Init" :  (string option)
             -> Stateid.t
    "Interp" :  ((bool * bool) * string)
             -> (Stateid.t * ((string, string) CSig.union))
"StopWorker" :  string
             -> unit
  "PrintAst" :  Stateid.t
             -> xml
  "Annotate" :  string
             -> xml

=== Calls XML encoding ===

A call "C" carrying input a is encoded as:

<call val="C">a</call>

A response carrying output b can either be:

<value val="good">b</value>

or:

<value val="fail" loc_s="15" loc_e="34">
  <state_id val="1"/>
error&nbsp;message
</value>

where the attributes loc_s and loc_c are optional.

=== Data encoding by examples ===

unit:

<unit/>

bool:

<bool val="true"/>
<bool val="false"/>

string:

<string>hello</string>

int:

<int>256</int>

Stateid.t:

<state_id val="1"/>

(int list):

<list>
  <int>3</int>
  <int>4</int>
  <int>5</int>
</list>

(int option):

<option val="some">
  <int>3</int>
</option>
<option val="none"/>

(bool * int):

<pair>
  <bool val="false"/>
  <int>3</int>
</pair>

((bool, int) CSig.union):

<union val="in_l">
  <bool val="false"/>
</union>

All other types are records represented by a node named like the OCaml
type which contains a flattened n-tuple.  We provide one example.

Interface.option_state:

<option_state>
  <bool val="true"/>
  <bool val="false"/>
  <string>name1</string>
  <option_value val="intvalue">
    <option val="some">
      <int>37</int>
    </option>
  </option_value>
</option_state>

