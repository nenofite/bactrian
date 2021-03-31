let main () = Bactrian.main () |> print_endline

open Cmdliner

let setup_log =
  let init style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let term =
  let doc = "A really stupid pairwise test generator" in
  let exits = Term.default_exits in
  let man = [] in
  Term.(const main $ setup_log, info "bactrian" ~doc ~exits ~man)

let () = Term.exit (Term.eval term)
