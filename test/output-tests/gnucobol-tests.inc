
(rule
 (with-stdout-to used_binaries.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} used_binaries.at))))
(rule
 (alias runtest)
 (action (diff used_binaries.expected used_binaries.output)))

(rule
 (with-stdout-to configuration.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} configuration.at))))
(rule
 (alias runtest)
 (action (diff configuration.expected configuration.output)))

(rule
 (with-stdout-to syn_copy.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_copy.at))))
(rule
 (alias runtest)
 (action (diff syn_copy.expected syn_copy.output)))

(rule
 (with-stdout-to syn_definition.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_definition.at))))
(rule
 (alias runtest)
 (action (diff syn_definition.expected syn_definition.output)))

(rule
 (with-stdout-to syn_subscripts.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_subscripts.at))))
(rule
 (alias runtest)
 (action (diff syn_subscripts.expected syn_subscripts.output)))

(rule
 (with-stdout-to syn_occurs.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_occurs.at))))
(rule
 (alias runtest)
 (action (diff syn_occurs.expected syn_occurs.output)))

(rule
 (with-stdout-to syn_redefines.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_redefines.at))))
(rule
 (alias runtest)
 (action (diff syn_redefines.expected syn_redefines.output)))

(rule
 (with-stdout-to syn_value.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_value.at))))
(rule
 (alias runtest)
 (action (diff syn_value.expected syn_value.output)))

(rule
 (with-stdout-to syn_file.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_file.at))))
(rule
 (alias runtest)
 (action (diff syn_file.expected syn_file.output)))

(rule
 (with-stdout-to syn_reportwriter.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_reportwriter.at))))
(rule
 (alias runtest)
 (action (diff syn_reportwriter.expected syn_reportwriter.output)))

(rule
 (with-stdout-to syn_refmod.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_refmod.at))))
(rule
 (alias runtest)
 (action (diff syn_refmod.expected syn_refmod.output)))

(rule
 (with-stdout-to syn_misc.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_misc.at))))
(rule
 (alias runtest)
 (action (diff syn_misc.expected syn_misc.output)))

(rule
 (with-stdout-to syn_move.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_move.at))))
(rule
 (alias runtest)
 (action (diff syn_move.expected syn_move.output)))

(rule
 (with-stdout-to syn_multiply.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_multiply.at))))
(rule
 (alias runtest)
 (action (diff syn_multiply.expected syn_multiply.output)))

(rule
 (with-stdout-to syn_screen.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_screen.at))))
(rule
 (alias runtest)
 (action (diff syn_screen.expected syn_screen.output)))

(rule
 (with-stdout-to syn_set.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_set.at))))
(rule
 (alias runtest)
 (action (diff syn_set.expected syn_set.output)))

(rule
 (with-stdout-to syn_functions.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_functions.at))))
(rule
 (alias runtest)
 (action (diff syn_functions.expected syn_functions.output)))

(rule
 (with-stdout-to syn_literals.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} syn_literals.at))))
(rule
 (alias runtest)
 (action (diff syn_literals.expected syn_literals.output)))

(rule
 (with-stdout-to listings.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} listings.at))))
(rule
 (alias runtest)
 (action (diff listings.expected listings.output)))

(rule
 (with-stdout-to run_fundamental.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_fundamental.at))))
(rule
 (alias runtest)
 (action (diff run_fundamental.expected run_fundamental.output)))

(rule
 (with-stdout-to run_subscripts.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_subscripts.at))))
(rule
 (alias runtest)
 (action (diff run_subscripts.expected run_subscripts.output)))

(rule
 (with-stdout-to run_refmod.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_refmod.at))))
(rule
 (alias runtest)
 (action (diff run_refmod.expected run_refmod.output)))

(rule
 (with-stdout-to run_accept.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_accept.at))))
(rule
 (alias runtest)
 (action (diff run_accept.expected run_accept.output)))

(rule
 (with-stdout-to run_initialize.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_initialize.at))))
(rule
 (alias runtest)
 (action (diff run_initialize.expected run_initialize.output)))

(rule
 (with-stdout-to run_misc.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_misc.at))))
(rule
 (alias runtest)
 (action (diff run_misc.expected run_misc.output)))

(rule
 (with-stdout-to run_file.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_file.at))))
(rule
 (alias runtest)
 (action (diff run_file.expected run_file.output)))

(rule
 (with-stdout-to run_reportwriter.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_reportwriter.at))))
(rule
 (alias runtest)
 (action (diff run_reportwriter.expected run_reportwriter.output)))

(rule
 (with-stdout-to run_returncode.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_returncode.at))))
(rule
 (alias runtest)
 (action (diff run_returncode.expected run_returncode.output)))

(rule
 (with-stdout-to run_functions.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_functions.at))))
(rule
 (alias runtest)
 (action (diff run_functions.expected run_functions.output)))

(rule
 (with-stdout-to run_extensions.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_extensions.at))))
(rule
 (alias runtest)
 (action (diff run_extensions.expected run_extensions.output)))

(rule
 (with-stdout-to run_ml.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} run_ml.at))))
(rule
 (alias runtest)
 (action (diff run_ml.expected run_ml.output)))

(rule
 (with-stdout-to data_binary.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} data_binary.at))))
(rule
 (alias runtest)
 (action (diff data_binary.expected data_binary.output)))

(rule
 (with-stdout-to data_display.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} data_display.at))))
(rule
 (alias runtest)
 (action (diff data_display.expected data_display.output)))

(rule
 (with-stdout-to data_packed.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} data_packed.at))))
(rule
 (alias runtest)
 (action (diff data_packed.expected data_packed.output)))

(rule
 (with-stdout-to data_pointer.output
  (setenv COB_CONFIG_DIR "%{env:DUNE_SOURCEROOT=.}/import/gnucobol/config"
   (run %{exe:gnucobol.exe} data_pointer.at))))
(rule
 (alias runtest)
 (action (diff data_pointer.expected data_pointer.output)))
