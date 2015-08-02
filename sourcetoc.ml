#! /usr/bin/env ocaml

(* Copyright © 2015 Johan Grande *)
(* License = GNU GPL version 2 or 3 *)


let backup_suffix = ref "~"
let etc = ref " [...]"
let indent_width = ref 2
let line_length = ref 80
let verbosity = ref 1

type output = File of string | InPlace | Stdout
let output = ref InPlace

type heading_style = Atx | Html | Wiki
let heading_style = ref Wiki

let sourcetoc = Filename.basename Sys.argv.(0)


(* <TOC> *)
(* Utils ...................................................................  55
 *   Logging and failing ...................................................  89
 * Comment patterns  #data ................................................. 111
 * Read source file ........................................................ 147
 * Generate table of contents .............................................. 260
 *   Adjust line numbers and levels ........................................ 261
 *   Generate TOC lines .................................................... 274
 * I/O ..................................................................... 338
 * CLI ..................................................................... 413
 *)
(* </TOC> *)


;;
#warnings "-3" (* deprecated: String.set *)


#load "unix.cma"
#load "str.cma"

open Printf

let push, pop = Queue.push, Queue.pop


type action = Fill | Clear | TocOnly

type comment = BeginEnd of string * string | Line of string

type heading = int * int * string (* line number, level, text *)



(* = Utils = *)

let list_of_queue q =
  Queue.fold (fun l x -> x :: l) [] q

let queue_last q =
  Queue.fold (fun x y -> y) (Queue.peek q) q

let queue_map f q =
  let res = Queue.create () in
  Queue.iter (fun x -> push (f x) res) q;
  res

let with_file_as_rchan file f =
  let rchan = open_in file in
  try
    let res = f rchan in
    close_in rchan;
    res
  with e ->
    close_in_noerr rchan;
    raise e

let with_file_as_wchan file f =
  let wchan = open_out file in
  try
    let res = f wchan in
    close_out wchan;
    res
  with e ->
    close_out_noerr wchan;
    raise e


(* == Logging and failing == *)

let log msg =
  eprintf "%s: %s\n%!" sourcetoc msg

let warn level msg =
  if !verbosity >= level then
    log ("Warning: " ^ msg)


exception FileCrash

(* Stop processing of current file (start working on next file) *)
let fcrash fmt =
  ksprintf (fun s -> log s; raise FileCrash) fmt

(* Exit program *)
let gcrash fmt =
  ksprintf (fun s -> log s; exit 1) fmt



(* = Comment patterns  #data = *)

let extension file =
  let open Str in
  if string_match (regexp "^.+\\.\\([^.]+\\)$") file 0 then
    matched_group 1 file
  else ""

(* Returns a list of comment patterns associated with file extension `ext`.
   The first pattern is used to generate the TOC. *)
let find_comments ext =
  match ext with
  | "c" | "cc" | "cpp" | "cxx" | "c++"
  | "h" | "hh" | "hpp" | "hxx" | "h++"
  | "java" | "js" ->
      [BeginEnd ("/*", "*/"); Line "//"]
  | "hs" ->
      [Line "--"; BeginEnd ("{--", "--}")]
  | "ml" | "mli" | "mll" ->
      [BeginEnd ("(*", "*)")]
  | "mly" ->
      [BeginEnd ("(*", "*)"); BeginEnd ("/*", "*/")]
  | "php" ->
      [BeginEnd ("/*", "*/"); Line "//"; Line "#"]
  | "py" ->
      Line "#" :: (List.map (fun x -> BeginEnd (x, x))
                     ["'"; "\""; "'''"; "\"\"\""] )
  | "scm" ->
      [Line ";;"; Line ";"; BeginEnd ("#|", "|#")]
  | "sh" | "mk" ->
      [Line "#"]
  | _ ->
      fcrash "Unknown file extension"



(* = Read source file = *)

(* Regexp `s` in comment pattern `c` *)
let comment_regexp c s =
  let open Str in
  match c with
  | BeginEnd (b, e) ->
      regexp (sprintf "^%s *%s *%s *$" (quote b) s (quote e))
  | Line b ->
      regexp (sprintf "^%s *%s *$" (quote b) s)

let comment_regexps comments s =
  List.map (fun c -> comment_regexp c s) comments  

(* Does `line` match against at least one of `regexps`? *)
let line_match regexps line =
  List.exists (fun re -> Str.string_match re line 0) regexps


(* Takes a list of comment patterns and a line of text.
   Returns Some _ if the line contains a heading, None otherwise. *)
let heading_of_line com_pats =
  let open Str in
  let heading_regexps =
    comment_regexps com_pats
      (match !heading_style with
       | Atx -> "\\(#+\\) *\\([^ #]\\|[^ ].*[^ #]\\) *#*"
       | Html -> "<h\\([1-9]\\)> *\\([^ ]\\|[^ ].*[^ ]\\) *</h\\1>"
       | Wiki -> "\\(=+\\) *\\([^ =]\\|[^ ].*[^ =]\\) *=*")
  in
  fun line ->
    if line_match heading_regexps line then
      let l, txt = matched_group 1 line, matched_group 2 line in
      let level =
        if !heading_style = Html then
          int_of_string l
        else
          String.length l
      in
      Some (level, txt)
    else
      None
    

let toc_error_prefix = "Table of contents place ill-defined"

let toc_unicity q s =
  match Queue.length q with
  | 0 -> fcrash "%s: no %s found." toc_error_prefix s
  | 1 -> pop q
  | n ->
      fcrash "%s: several %s found, at lines %s." toc_error_prefix s
        (String.concat ", " (List.map string_of_int (list_of_queue q)))


(* Returns :
   lines before TOC, <TOC> line #, headings, </TOC> line #, lines after TOC *)
let read_source comments inchan
    : string Queue.t * int * heading Queue.t * int * string Queue.t =
  let open Str in
  let heading_of_line = heading_of_line comments in
  let toc_begins_regexps = comment_regexps comments "<TOC *>"
  and toc_ends_regexps = comment_regexps comments "</TOC *>" in

  let headings = Queue.create () in
  let toc_begins = Queue.create () and toc_ends = Queue.create () in
  let all_lines = Queue.create () in
  let line_number = ref 0 in

  (* Scan file *)
  (try
     while true do
       let line = input_line inchan in
       incr line_number;

       (match heading_of_line line with
        | Some (level, txt) ->
            push (!line_number, level, txt) headings
        | None ->
            if line_match toc_begins_regexps line then
              push !line_number toc_begins
            else if line_match toc_ends_regexps line then
              push !line_number toc_ends
       );

       push line all_lines
     done
   with End_of_file -> () );

  (* Validate TOC place *)
  let toc_begin = toc_unicity toc_begins "<TOC>"
  and toc_end = toc_unicity toc_ends "</TOC>" in
  if toc_begin >= toc_end then
    fcrash "%s: it ends (line %d) before it starts (line %d)." toc_error_prefix
      toc_end  toc_begin;

  (* Divide the lines into before and after the TOC *)
  let before_toc = Queue.create () and after_toc = Queue.create () in
  for i = 1 to toc_begin do
    push (pop all_lines) before_toc
  done;
  for i = toc_begin + 1 to toc_end - 1 do
    ignore (pop all_lines)
  done;
  for i = toc_end to !line_number do
    push (pop all_lines) after_toc
  done;
  assert (Queue.is_empty all_lines);

  before_toc, toc_begin, headings, toc_end, after_toc



(* = Generate table of contents = *)
(* == Adjust line numbers and levels == *)

let adjust toc_begin toc_end special headings : heading Queue.t =
  let lines_offset =
    Queue.length headings + (if special then 1 else 0) - (toc_end-toc_begin-1) in
  let min_level = ref max_int in
  Queue.iter (fun (n, l, t) -> min_level := min l !min_level) headings;
  queue_map
    (fun (n, l, t) ->
      (if n < toc_begin then n else n + lines_offset), l - !min_level, t )
    headings


(* == Generate TOC lines == *)

let line_of_heading line_begin line_end mlns (line, level, text) =
  let sides_length =
    String.length line_begin
    + level * !indent_width
    + 1 + mlns
    + String.length line_end
  in
  if sides_length >= !line_length then
    fcrash "Line length (%d chars) too short to represent heading at line %d."
      !line_length  line;
  sprintf "%s%s%s%s%s"
    line_begin
    (String.make (level * !indent_width) ' ')
    (let l = String.length text in
     if sides_length + l <= !line_length then
       text ^ " " ^ (String.make (!line_length - sides_length - l - 1) '.')
     else
       String.sub text 0 (!line_length - sides_length - (String.length !etc))
       ^ !etc )
    (let s = string_of_int line in
     (String.make (mlns - String.length s + 1) ' ') ^ s )
    line_end


(* Returns the ready-to-print lines of the TOC *)
let toc_of_headings comment toc_begin toc_end headings : string Queue.t =
  if Queue.is_empty headings then
    Queue.create ()
  else
    let cb, ce =
      match comment with
      | BeginEnd (b, e) -> b, e
      | Line b -> b, ""
    in
    let special =
      String.length cb = 2 && String.length ce = 2 && cb.[1] = ce.[0] in
    (* In the "special" case we will generate something like
     * the present comment.
    *)
    let line_begin, line_end =
      if special then
        " " ^ (String.sub cb 1 1) ^ " ", ""
      else
        cb ^ " ", if ce <> "" then " " ^ ce else ""
    in

    let adjusted_headings = adjust toc_begin toc_end special headings in
    let max_line_number_size =
      match queue_last adjusted_headings
      with n, l, t -> String.length (string_of_int n)
    in
    let toc =
      queue_map (line_of_heading line_begin line_end max_line_number_size)
        adjusted_headings
    in
    if special then
      ((Queue.peek toc).[0] <- cb.[0];
       Queue.push (" " ^ ce) toc );
    toc



(* = I/O = *)

(* Read; compute; write *)
let process_channels action comments rchan =
  let before_toc, toc_begin, headings, toc_end, after_toc =
    read_source comments rchan
  in
  let toc =
    if action = Clear then
      Queue.create ()
    else
      toc_of_headings (List.hd comments) toc_begin toc_end headings
  in
  fun wchan ->
    List.iter
      (Queue.iter (fprintf wchan "%s\n"))
      (if action = TocOnly then [toc] else [before_toc; toc; after_toc])


(* Process a file *)
let process_file action comment_style source =
  let open Unix in

  (* Get comment patterns *)
  let comment_style_1 =
    if comment_style <> "" then comment_style else extension source
  in
  let comments = find_comments comment_style_1 in

  if (stat source).st_kind <> S_REG then
    fcrash "'%s' is not a regular file." source;

  (* Read source file; compute table of contents *)
  let writing_function =
    with_file_as_rchan source (process_channels action comments)
  in

  (* Output result *)
  match !output with
  | File f ->
      with_file_as_wchan f writing_function

  | InPlace ->
      if action = TocOnly then
        warn 1 "replacing file contents with only a table of contents.";

      if !backup_suffix <> "" then
        (let backup = source ^ !backup_suffix in
         if Sys.file_exists backup && (stat backup).st_kind <> S_REG then
           fcrash "Backup file '%s' already exists but is not a regular file." backup;

         (* Create backup file *)
         let cp = "cp -fp" in
         let com =
           sprintf "%s %s %s" cp (Filename.quote source) (Filename.quote backup)
         in
         let ret = Sys.command com in
         if ret <> 0 then
           gcrash "Backup ('%s') failed with code %d! Aborting." cp ret);

      (* Overwrite source file *)
      let wchan = open_out source in
      (try
         writing_function wchan;
         close_out wchan
       with e ->
         log "Exception encountered while writing modified file!";
         close_out_noerr wchan;
         raise e)

  | Stdout ->
      writing_function Pervasives.stdout



(* = CLI = *)

let heading_style_of_string s =
  match String.lowercase s with
  | "atx" | "markdown" -> Atx
  | "html" -> Html
  | "wiki" -> Wiki
  | _ -> invalid_arg "heading_style_of_string"


let action = ref Fill
let comment_style = ref ""
let files = Queue.create ()

let speclist =
  let action_clear = Clear in (* versus Arg.Clear, for OCaml < 4.01 *)
  let open Arg in
  align [
    ("--clear",
     Unit (fun () -> action := action_clear),
     " Clear table of contents.");
    ("--toc-only",
     Unit (fun () -> action := TocOnly; output := Stdout),
     " Print table of contents only. Default output = stdout.\n");

    ("--in-place",
     Unit (fun () -> output := InPlace),
     " Overwrite source file(s). This is the default.");
    ("--output",
     String (fun s -> output := File s),
     "FILE Output to FILE.");
    ("--stdout",
     Unit (fun () -> output := Stdout),
     " Output to stdout.\n");

    ("--backup-suffix",
     Set_string backup_suffix,
     "STRING (in place) Suffix for backup files, default = '~'.");
    ("--no-backup",
     Unit (fun () -> backup_suffix := ""),
     " (in place) No backup.");
    ("--comment-style",
     Set_string comment_style,
     "EXT Use comment style associated to file extension EXT.");
    ("--etc",
     Set_string etc,
     "STRING End of line for long headings, default = ' [...]'.");
    ("--heading-style",
     String (fun s -> heading_style := heading_style_of_string s),
     "STYLE Heading style. wiki, atx (markdown), or html. Default = wiki.");
    ("--indent-width",
     Set_int indent_width,
     "INT Indent width in spaces, default = 2.");
    ("--line-length",
     Set_int line_length,
     "INT Line length in characters, default = 80.");
    ("--quiet",
     Unit (fun () -> verbosity := 0),
     " No warnings.\n");
  ]

let anon_fun f =
  push f files

let usage_msg =
  sprintf "Usage: %s (OPTION|FILE)*

See README.md or https://github.com/nahoj/sourcetoc.
" sourcetoc

let main () =
  Arg.parse speclist anon_fun usage_msg;
  if Queue.is_empty files then
    (if !comment_style = "" then
       gcrash "Please specify a comment style.";
     try process_channels !action (find_comments !comment_style) stdin stdout
     with FileCrash -> exit 1)
  else
    Queue.iter
      (fun f ->
         try process_file !action !comment_style f
         with FileCrash -> ())
      files


let _ =
  Unix.handle_unix_error main ()
