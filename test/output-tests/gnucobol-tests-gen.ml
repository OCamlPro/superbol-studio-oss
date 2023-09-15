(* -*- tuareg -*- *)
let at_files = [
  "used_binaries.at";
  "configuration.at";
  "syn_copy.at";
  "syn_definition.at";
  "syn_subscripts.at";
  "syn_occurs.at";
  "syn_redefines.at";
  "syn_value.at";
  "syn_file.at";
  "syn_reportwriter.at";
  "syn_refmod.at";
  "syn_misc.at";
  "syn_move.at";
  "syn_multiply.at";
  "syn_screen.at";
  "syn_set.at";
  "syn_functions.at";
  "syn_literals.at";
  "listings.at";
  "run_fundamental.at";
  "run_subscripts.at";
  "run_refmod.at";
  "run_accept.at";
  "run_initialize.at";
  "run_misc.at";
  "run_file.at";
  "run_reportwriter.at";
  "run_returncode.at";
  "run_functions.at";
  "run_extensions.at";
  "run_ml.at";
  "data_binary.at";
  "data_display.at";
  "data_packed.at";
  "data_pointer.at";
];;

List.iter begin fun at_file ->
  let basename = Filename.chop_extension at_file in
  Printf.printf "
(rule
 (with-stdout-to %s.output
  (setenv COB_CONFIG_DIR \"%%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config\"
   (run %%{exe:gnucobol.exe} %s.at))))
(rule
 (alias runtest)
 (action (diff %s.expected %s.output)))
" basename basename basename basename
end at_files
