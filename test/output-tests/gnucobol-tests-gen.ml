(* -*- tuareg -*- *)
let at_files = [
  "backcomp.at";
  "configuration.at";
  "data_binary.at";
  "data_display.at";
  "data_packed.at";
  "data_pointer.at";
  "listings.at";
  "run_accept.at";
  "run_extensions.at";
  "run_file.at";
  "run_functions.at";
  "run_fundamental.at";
  "run_initialize.at";
  "run_manual_screen.at";
  "run_misc.at";
  "run_ml.at";
  "run_refmod.at";
  "run_reportwriter.at";
  "run_returncode.at";
  "run_subscripts.at";
  "syn_copy.at";
  "syn_definition.at";
  "syn_file.at";
  "syn_functions.at";
  "syn_literals.at";
  "syn_misc.at";
  "syn_move.at";
  "syn_multiply.at";
  "syn_occurs.at";
  "syn_redefines.at";
  "syn_refmod.at";
  "syn_reportwriter.at";
  "syn_screen.at";
  "syn_set.at";
  "syn_subscripts.at";
  "syn_value.at";
  "used_binaries.at";
];;

List.iter begin fun at_file ->
  let basename = Filename.chop_extension at_file in
  Printf.printf "
(rule
 (ignore-stderr
  (with-stdout-to %s.output
   (setenv COB_CONFIG_DIR \"%%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config\"
    (run %%{exe:gnucobol.exe} %s.at)))))
(rule
 (alias runtest)
 (action (diff %s.expected %s.output)))
" basename basename basename basename
end at_files
