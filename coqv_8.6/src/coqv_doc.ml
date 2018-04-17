let commands = [
  ("import", "<filename>", "Read each line of the file <filename> and send to coqtop");
  ("export", "<filename>", "Export the commands already input to a file <filename>");
  ("status", "", "Print the status of coqv");
  (* ("history", "", "Print the commands already input"); *)
  ("proof", "[<thmid>]", "Print the proof tree of the theorem <thmid>, if <thmid> is not sepcified, then print the current proof tree");
  ("quit", "", "Close both coqtop and coqv")
]