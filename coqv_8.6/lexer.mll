{
    open Parser
}

let int = ['0'-'9']+
let id = ['a'-'z' 'A' - 'Z'] ['a'-'z' 'A' - 'Z' '0'-'9' '_']*

rule command_token = 
    | ":"       {Colon}
    | id as s   {Id s}
    | int as s  {Int (int_of_string s)}

and coqtop_token = 
    | "."           {Dot}
    | "============================"    {Gamma}
    | ":"           {Colon}
    | "Error"       {Error}
    | "subgoal"     {Subgoal}
    | "subgoals"    {Subgoals}
    | "Lemma"       {Lemma}
    | "Theorem"     {Theorem}
    | "Remark"      {Remark}
    | "Fact"        {Fact}
    | "Corollary"   {Corollary}
    | "Proposition" {Proposition}
    | id as s       {Id s}
    | int as s      {Int (int_of_string s)}
    |

